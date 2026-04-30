(ns us.chouser.scratch-link-ev3
  "A Scratch Link proxy that allows the EV3 extension for Scratch 3.0 in the
  browser connect to the Lego EV3 device over an RFCOMM (bluetooth) serial connection.

  Scratch expects a WSS server at:
    wss://device-manager.scratch.mit.edu:20110/scratch/bt

  Setup (one-time):
    1. Generate a self-signed cert:
         openssl req -x509 -newkey rsa:2048 -nodes -days 3650 \\
           -subj '/CN=device-manager.scratch.mit.edu' \\
           -addext 'subjectAltName=DNS:device-manager.scratch.mit.edu' \\
           -keyout scratch-link.key -out scratch-link.crt
         openssl pkcs12 -export -in scratch-link.crt -inkey scratch-link.key \\
           -name scratch-link -passout pass:scratch \\
           -out scratch-link.p12
         keytool -importkeystore -srckeystore scratch-link.p12 \\
           -srcstoretype pkcs12 -srcstorepass scratch \\
           -destkeystore scratch-link.jks -deststorepass scratch

    2. Add to hosts file:
       127.0.0.1 device-manager.scratch.mit.edu
       On NixOS, add to configuration.nix, then sudo nixos-rebuild switch:
         networking.hosts = { \"127.0.0.1\" = [ \"device-manager.scratch.mit.edu\" ]; };

    3. Trust the cert in your browser by visiting:
         https://device-manager.scratch.mit.edu:20110/
       and clicking through the security warning.

   There are REPL examples at the end of the file."
  (:require [clojure.data.json :as json]
            [clojure.string :as string]
            [us.chouser.ev3 :as ev3])
  (:import [org.java_websocket.server WebSocketServer DefaultSSLWebSocketServerFactory]
           [org.java_websocket WebSocket]
           [org.java_websocket.handshake ClientHandshake]
           [javax.net.ssl KeyManagerFactory SSLContext]
           [java.security KeyStore]
           [java.io FileInputStream]
           [java.net InetSocketAddress]
           [java.nio ByteBuffer ByteOrder]
           [java.util Base64]))

(set! *warn-on-reflection* true)

;;;; -------------------------------------------------------------------------
;;;; Interception points -- override these multimethods to intercept traffic.
;;;; -------------------------------------------------------------------------

(defmulti handle-scratch-request
  "Called for every JSON-RPC request Scratch sends.
   `method` is a string like \"send\", \"discover\", \"connect\".
   `params` is a map of the decoded params.
   `ctx` is the session context map (has :conn, :config, :ws,
         :pending-ir-rewrites -- an atom<set> of in-flight sequence numbers).

   Return one of:
     :forward        -- pass the request through to the EV3 as-is (default)
     :drop           -- discard the request, send no response to Scratch
     {:rewrite p}    -- forward the 'send' with substituted params map p
     any other map   -- return this as the JSON-RPC result to Scratch
                        (bypasses the EV3 entirely)"
  (fn [method _params _ctx] method))

(defmethod handle-scratch-request :default [_method _params _ctx]
  :forward)

(defmulti handle-ev3-message
  "Called for every raw byte array received from the EV3 before it is
   forwarded to Scratch as a `didReceiveMessage` notification.
   `data` is a byte[].
   `ctx` is the session context map.

   Return one of:
     :forward   -- send the data to Scratch as-is (default)
     :drop      -- discard the message
     a byte[]   -- send this modified data instead"
  (fn [_data _ctx] :default))

(defmethod handle-ev3-message :default [_data _ctx]
  :forward)

;;;; -------------------------------------------------------------------------
;;;; Internal state
;;;; -------------------------------------------------------------------------

(defonce *server (atom nil))

;; Per-websocket session state. Keyed by WebSocket object.
(defonce *sessions (atom {}))

;;;; -------------------------------------------------------------------------
;;;; Helpers
;;;; -------------------------------------------------------------------------

(defn- b64-encode [^bytes b]
  (.encodeToString (Base64/getEncoder) b))

(defn- b64-decode [^String s]
  (.decode (Base64/getDecoder) s))

(defn- send-json! [^WebSocket ws m]
  (let [text (json/write-str m)]
    #_(println "<< WS" text)
    (.send ws text)))

(defn- notify! [^WebSocket ws method params]
  (send-json! ws {"jsonrpc" "2.0"
                  "method"  method
                  "params"  params}))

(defn- reply! [^WebSocket ws id result]
  (send-json! ws {"jsonrpc" "2.0"
                  "id"      id
                  "result"  result}))

(defn- error-reply! [^WebSocket ws id code message]
  (send-json! ws {"jsonrpc" "2.0"
                  "id"      id
                  "error"   {"code" code "message" message}}))

;; PRIMPAR encoding: if high bit clear, it's a 1-byte immediate (0x00-0x3F).
;; 0x81 = short-form 1-byte follows; 0x82 = short-form 2-bytes follow.
;; We only need to skip one encoded value to find the next.
(defn- primpar-skip
  "Returns the index just past the PRIMPAR-encoded value starting at `i`."
  [^bytes data i]
  (let [b (bit-and (aget data i) 0xFF)]
    (cond
      (= (bit-and b 0xC0) 0x00) (inc i)          ; short constant, 1 byte total
      (= b 0x81)                (+ i 2)           ; LC1, 1 byte follows
      (= b 0x82)                (+ i 3)           ; LC2, 2 bytes follow
      (= b 0x83)                (+ i 5)           ; LC4, 4 bytes follow
      :else                     (inc i))))         ; fallback / GVX/LVX forms

;;; ----- constants -----------------------------------------------------------

;; opINPUT_DEVICE_LIST  0x98  – bulk read: LAYER, COUNT, -> array of [type, mode]
;; opINPUT_READSI       0x9D  – read SI float: LAYER, PORT, TYPE, MODE, -> GVX
;; opINPUT_DEVICE       0x99  – sub-cmd form: sub-CMD, LAYER, PORT, TYPE, MODE, NVALUES, -> GVX...
;; (Fixed: in the old code OP-INPUT-DEVICE and CMD-READY-SI were each defined twice.)

(def ^:private ^Byte OP-INPUT-DEVICE-LIST (unchecked-byte 0x98))
(def ^:private ^Byte OP-INPUT-DEVICE      (unchecked-byte 0x99))
(def ^:private ^Byte OP-INPUT-READSI      (unchecked-byte 0x9D))
(def ^:private ^Byte CMD-READY-SI         (unchecked-byte 0x1D))
(def ^:private ^Byte CMD-READY-RAW        (unchecked-byte 0x1C))
(def ^:private ^Byte TYPE-ULTRASONIC      (unchecked-byte 0x1E))
(def ^:private ^Byte TYPE-IR              (unchecked-byte 0x21))
(def ^:private ^Byte MODE-PROXIMITY       (unchecked-byte 0x00))

;;; ----- helpers ------------------------------------------------------------

(defn- frame-seq
  "Extract the 2-byte LE sequence number from a framed EV3 message."
  [^bytes data]
  (bit-or (bit-and (aget data 2) 0xFF)
          (bit-shift-left (bit-and (aget data 3) 0xFF) 8)))

(defn- find-ultrasonic-type-offset
  "If `data` contains an opINPUT_DEVICE/READY_SI or READY_RAW command
   requesting the ultrasonic sensor (type 0x1E), returns the byte offset
   of the type byte. Returns nil otherwise."
  [^bytes data]
  (when (>= (alength data) 12)
    (let [op  (aget data 7)
          sub (aget data 8)]
      (when (and (= op OP-INPUT-DEVICE)
                 (or (= sub CMD-READY-SI) (= sub CMD-READY-RAW)))
        (let [i-layer (int 9)
              i-port  (primpar-skip data i-layer)
              i-type  (primpar-skip data i-port)]
          (when (and (< i-type (alength data))
                     (= (aget data i-type) TYPE-ULTRASONIC))
            i-type))))))

(defn- parse-gvx-value
  "Return the GV byte offset encoded at byte `i`.
   Short form: 0x6N -> N.  Long form: 0xE1 <byte> -> byte."
  [^bytes data i]
  (let [b (bit-and (aget data i) 0xFF)]
    (if (= (bit-and b 0xE0) 0x60)
      (bit-and b 0x1F)
      (bit-and (aget data (inc i)) 0xFF))))

(defn- scan-readsi-ops
  "Scan the bytecode portion of a direct-command frame (starting at byte 7)
   for opINPUT_READSI (0x9D) ops. Returns a seq of maps:
     {:offset <index of 0x9D byte>
      :port   <sensor port number>
      :type   <type byte>
      :mode   <mode byte>
      :gv-offset <GV destination byte offset>}

   opINPUT_READSI argument layout (all raw bytes, no PRIMPAR encoding):
     [i+0] 0x9D   opcode
     [i+1] LAYER  (1 byte)
     [i+2] PORT   (1 byte)
     [i+3] TYPE   (1 byte, 0x00 = auto-detect)
     [i+4] MODE   (1 byte)
     [i+5] GVX    (1 byte short form 0x6N, or 2 bytes 0xE1 <off> long form)"
  [^bytes data]
  (let [n (alength data)]
    (loop [i 7, acc []]
      (if (>= i n)
        acc
        (let [b (bit-and (aget data i) 0xFF)]
          (if (and (= b 0x9D) (>= n (+ i 7)))
            (let [port    (bit-and (aget data (+ i 2)) 0xFF)
                  typ     (bit-and (aget data (+ i 3)) 0xFF)
                  mode    (bit-and (aget data (+ i 4)) 0xFF)
                  gvx-i   (+ i 5)
                  gvx-b   (bit-and (aget data gvx-i) 0xFF)
                  gv-off  (parse-gvx-value data gvx-i)
                  gvx-len (if (= gvx-b 0xE1) 2 1)]
              (recur (+ i 5 gvx-len)
                     (conj acc {:offset i :port port :type typ
                                :mode mode :gv-offset gv-off})))
            (recur (inc i) acc)))))))

(defn- rewrite-readsi-frame
  "Scan `data` for all opINPUT_READSI ops targeting `ir-port`.
   For each found, rewrite MODE -> 0x00 (proximity), leaving TYPE unchanged.
   Returns [rewritten-frame, [gv-offset ...]] if any were rewritten, else nil."
  [^bytes data ir-port]
  (let [ops    (scan-readsi-ops data)
        ir-ops (filter #(= (:port %) ir-port) ops)]
    (when (seq ir-ops)
      (let [out (aclone data)]
        (doseq [{:keys [offset]} ir-ops]
          (aset out (+ offset 4) MODE-PROXIMITY))  ; MODE -> 0x00 only
        [out (mapv :gv-offset ir-ops)]))))

(defn- rewrite-reply-ir->cm
  "Rewrite the IR proximity float at GV byte offset `gv-offset`
   (i.e. frame byte `5 + gv-offset`) but the scale appears to be the same, so (* 1.0).
   Returns nil if the frame is too short."
  [^bytes data gv-offset]
  (let [start (+ 5 gv-offset)]
    (when (>= (alength data) (+ start 4))
      (let [ir-val (-> (ByteBuffer/wrap data start 4)
                       (.order ByteOrder/LITTLE_ENDIAN)
                       (.getFloat))
            cm     (float (* ir-val 1.0))
            out    (aclone data)]
        (-> (ByteBuffer/wrap out start 4)
            (.order ByteOrder/LITTLE_ENDIAN)
            (.putFloat cm))
        out))))

;;; ----- logging ------------------------------------------------------------

(def *log (atom {}))

(defn log-msg [dir ^bytes msg-bytes]
  (let [k (into [dir (alength msg-bytes)]
                (map (partial get msg-bytes) [4 7 8]))]
    (swap! *log update k
           (fn [vs]
             (let [vs      (or vs [])
                   prev    (last vs)
                   changed? (and prev
                                 (not= (subvec (vec (second prev)) 5 (min 21 (alength msg-bytes)))
                                       (subvec (vec msg-bytes) 5 (min 21 (alength msg-bytes)))))]
               (if changed?
                 (conj vs [(System/nanoTime) msg-bytes])
                 (-> (into [] (take-last 3 vs))
                     (conj [(System/nanoTime) msg-bytes]))))))))

(defn log-event [tag data]
  (swap! *log update [:event tag]
         (fn [vs] (-> (into [] (take-last 20 vs))
                      (conj [(System/nanoTime) data])))))

(def ^:private op-names
  {0x98 "DEVICE_LIST" 0x99 "INPUT_DEVICE" 0x9A "INPUT_READ"
   0x9D "READSI"      0x9E "READEXT"
   0xA3 "OUT_STOP"    0xA6 "OUT_START"    0xA5 "OUT_SPEED"
   0xB0 "OUT_STEP_SPEED" 0xB3 "OUT_TIME_SPEED"
   0xAF "OUT_STEP_SYNC"  0xBF "OUT_TIME_SYNC"
   0x01 "NOP"         0x97 "SOUND"})

(def ^:private sub-names
  {0x1D "READY_SI" 0x1C "READY_RAW" 0x1B "READY_PCT"
   0x05 "GET_TYPEMODE" 0x1A "CLR_CHANGES"})

(def ^:private msg-type-names
  {0x00 "DIR_REPLY" 0x01 "DIR_NOREPLY" 0x02 "REPLY_OK" 0x04 "REPLY_ERR"
   0x80 "SYS_NOREPLY" 0x81 "SYS_REPLY"})

(def ^:private type-names
  {0x00 "auto" 0x1E "ultrasonic" 0x21 "IR" 0x10 "touch"
   0x1D "color" 0x20 "gyro" 0x07 "motor"})

(defn- annotate-bytes [^bytes data]
  (let [n   (alength data)
        mask-get #(when (< % n) (bit-and (aget data %) 0xFF))]
    (map-indexed
      (fn [i b]
        [(format "%02X" (unchecked-byte b))
         (cond
           (= i 0) (str "len=" (bit-or b (bit-shift-left (or (mask-get 1) 0) 8)))
           (= i 2) (str "seq=" (bit-or b (bit-shift-left (or (mask-get 3) 0) 8)))
           (= i 4) (msg-type-names b)
           (= i 5) (when (= (mask-get 4) 0x00)
                     (let [gm (bit-and b 0xFF)
                           lm (bit-shift-right (or (mask-get 6) 0) 2)]
                       (str "gv=" gm (when (pos? lm) (str ",lv=" lm)))))
           (= i 7) (op-names b)
           (= i 8) (cond (= (mask-get 7) 0x99) (sub-names b)
                         (= (mask-get 7) 0x9D) (str "layer=" b)
                         :else nil)
           (= i 9) (cond (= (mask-get 7) 0x9D) (str "port=" b)
                         (= (mask-get 7) 0x99) (str "layer=" b)
                         :else nil)
           (= i 10) (when (= (mask-get 7) 0x9D) (str "type=" (get type-names b b)))
           (= i 11) (when (= (mask-get 7) 0x9D) (str "mode=" b))
           :else nil)])
      data)))

(defn- format-readsi-ops [^bytes data]
  (let [ops (scan-readsi-ops data)]
    (when (seq ops)
      (->> ops
           (map (fn [{:keys [port type mode gv-offset]}]
                  (format "port=%d type=%s mode=%d->gv%d"
                          port (get type-names type (str type)) mode gv-offset)))
           (string/join ", ")
           (str "  READSI[")
           (#(str % "]"))))))

(defn- bytes-string [bytes]
  (->> bytes
       (map #(format "%02X" (unchecked-byte %)))
       (string/join " ")))

(defn print-log! [& [data]]
  (let [entries (->> (for [[k entries] (or data @*log)
                           :let [gap-nano (dec (first (first entries)))]
                           [nanos msg] (cons [gap-nano :gap] entries)]
                       [nanos k msg])
                     (sort))
        first-nano (ffirst entries)]
    (run! (fn [[nanos k msg]]
            (let [ts (quot (- nanos first-nano) 1000000)]
              (cond
                (= :gap msg)
                (println (format "%06d %s :gap" ts (name (if (keyword? k) k (first k)))))

                (map? msg)
                (do (println (format "%06d %s %s" ts (pr-str k) (pr-str (dissoc msg :before :after))))
                    (when (:before msg)
                      (println "         before: " (bytes-string (:before msg))))
                    (when (:after msg)
                      (println "         after : " (bytes-string (:after msg)))))

                (bytes? msg)
                (let [annotated   (annotate-bytes msg)
                      hex-part    (string/join " " (map first annotated))
                      notes       (->> annotated
                                       (keep-indexed (fn [i [_ note]]
                                                       (when note (str "[" i "]" note))))
                                       (string/join "  "))
                      readsi-line (format-readsi-ops msg)]
                  (println (format "%06d %s  %s" ts (name (first k)) hex-part))
                  (when (or (seq notes) readsi-line)
                    (println (format "         %s%s" notes (or readsi-line ""))))))))
          entries)))

;;; ----- interception methods -----------------------------------------------
;; pending-ir-rewrites: atom<map seq -> [gv-offset ...]>

(defmethod handle-scratch-request "send" [_ params ctx]
  (let [raw     (b64-decode (get params "message"))
        ir-port (get-in ctx [:config :ir-port])]
    (log-msg :scr raw)
    (let [ops    (scan-readsi-ops raw)
          ir-ops (filter #(and (= (:port %) ir-port)
                               (> (:mode %) 0))
                         ops)]
      (if (seq ir-ops)
        (let [[rewritten gv-offsets] (rewrite-readsi-frame raw ir-port)]
          (reset! (:pending-ir-rewrites ctx) gv-offsets)
          (log-event :rewrite {:gv-offsets gv-offsets
                               :ir-port    ir-port
                               :before     (vec raw)
                               :after      (vec rewritten)})
          {:rewrite {"message" (b64-encode rewritten) "encoding" "base64"}})
        :forward))))

(defmethod handle-ev3-message :default [^bytes data ctx]
  (log-msg :ev3 data)
  ;; Pass 1: rewrite IR type (0x21) -> ultrasonic (0x1E) in DEVICE_LIST replies.
  (let [out1 (let [out (aclone data)
                   rewrote? (volatile! false)]
               (doseq [i (range (alength out))]
                 (when (= (bit-and (aget out i) 0xFF) 0x21)
                   (aset out i TYPE-ULTRASONIC)
                   (vreset! rewrote? true)))
               (if @rewrote? out data))
        ;; Pass 2: scale IR proximity float -> cm using pending gv-offsets.
        gv-offsets @(:pending-ir-rewrites ctx)
        out2 (if (seq gv-offsets)
               (do
                 (reset! (:pending-ir-rewrites ctx) nil)
                 (reduce (fn [buf gv-off]
                           (or (rewrite-reply-ir->cm buf gv-off) buf))
                         out1
                         gv-offsets))
               out1)]
    (log-event :ev3-rewrite {:type-rewrite?  (not (identical? out1 data))
                              :float-rewrite? (not (identical? out2 out1))
                              :gv-offsets     gv-offsets})
    out2))

;;;; -------------------------------------------------------------------------
;;;; EV3 read loop
;;;; -------------------------------------------------------------------------

(defn- read-loop
  "Continuously reads messages from the EV3 serial port and forwards them
   to Scratch as didReceiveMessage notifications.

   EV3 messages are framed with a 2-byte little-endian length prefix
   (the length covers only the payload, not itself)."
  [^WebSocket ws session-atom]
  (println "EV3 read loop starting")
  (try
    (loop []
      (let [{:keys [conn running?]} @session-atom]
        (when (and running? conn)
          (let [port    ^com.fazecast.jSerialComm.SerialPort (:port conn)
                header  (byte-array 2)
                n       (.readBytes port header 2)]
            (if (< n 2)
              (do
                (println "EV3 read loop: short header read, dropping and delaying")
                (Thread/sleep 3000)
                (recur))
              (let [msg-len (bit-or (bit-and (aget header 0) 0xFF)
                                    (bit-shift-left (bit-and (aget header 1) 0xFF) 8))
                    payload (byte-array msg-len)
                    _       (.readBytes port payload msg-len)
                    data    (byte-array (+ 2 msg-len))]
                (System/arraycopy header 0 data 0 2)
                (System/arraycopy payload 0 data 2 msg-len)
                #_(println (str "EV3 -> " (apply str (map #(format "%02X " (byte %)) data))))
                (let [ctx    @session-atom
                      result (handle-ev3-message data ctx)]
                  (cond
                    (= result :forward)
                    (notify! ws "didReceiveMessage"
                             {"message"  (b64-encode data)
                              "encoding" "base64"})

                    (= result :drop)
                    nil

                    (bytes? result)
                    (notify! ws "didReceiveMessage"
                             {"message"  (b64-encode result)
                              "encoding" "base64"})))
                (recur)))))))
    (catch Exception e
      (println "EV3 read loop error:" (.getMessage e))))
  (println "EV3 read loop stopped"))

;;;; -------------------------------------------------------------------------
;;;; Request dispatch
;;;; -------------------------------------------------------------------------

(defn- do-discover!
  "Fake the discovery phase: immediately report the configured EV3."
  [^WebSocket ws id _params {:keys [config]}]
  (reply! ws id nil)
  (future
    (Thread/sleep 200)
    (notify! ws "didDiscoverPeripheral"
             {"peripheralId" (:ev3-addr config)
              "name"         (:ev3-name config)
              "rssi"         -50})))

(defn- do-connect!
  [^WebSocket ws id _params session-atom]
  (let [{:keys [config]} @session-atom
        conn (ev3/connect! (:rfcomm-port config))]
    (if conn
      (do
        (swap! session-atom assoc :conn conn)
        (let [t (Thread. #(read-loop ws session-atom))]
          (.setDaemon t true)
          (.start t)
          (swap! session-atom assoc :read-thread t))
        (reply! ws id nil))
      (error-reply! ws id -32000 "Could not connect to EV3"))))

(defn- do-send!
  [^WebSocket ws id params {:keys [conn]}]
  (if-not conn
    (error-reply! ws id -32000 "Not connected")
    (let [encoding (get params "encoding" "base64")
          raw      (if (= encoding "base64")
                     (b64-decode (get params "message"))
                     (.getBytes ^String (get params "message") "UTF-8"))]
      #_(println (str "Scratch -> EV3 " (apply str (map #(format "%02X " (byte %)) raw))))
      (ev3/send-command conn raw)
      (reply! ws id nil))))

(defn- dispatch-request!
  [^WebSocket ws id method params session-atom]
  (let [ctx    @session-atom
        result (handle-scratch-request method params ctx)]
    (cond
      (= result :forward)
      (case method
        "discover" (do-discover! ws id params ctx)
        "connect"  (do-connect!  ws id params session-atom)
        "send"     (do-send!     ws id params ctx)
        (do (println "Unknown method:" method)
            (error-reply! ws id -32601 (str "Unknown method: " method))))

      (= result :drop)
      nil

      ;; {:rewrite new-params} -- forward with substituted params
      (and (map? result) (contains? result :rewrite))
      (case method
        "send" (do-send! ws id (:rewrite result) ctx)
        (error-reply! ws id -32000 ":rewrite not supported for this method"))

      ;; Any other map -- return as JSON-RPC result, bypassing EV3
      (map? result)
      (reply! ws id result)

      :else
      (error-reply! ws id -32000 "Bad interception result"))))

;;;; -------------------------------------------------------------------------
;;;; WebSocket server
;;;; -------------------------------------------------------------------------

(defn- ^WebSocketServer make-server [port config]
  (proxy [WebSocketServer] [(InetSocketAddress. port)]

    (onOpen [^WebSocket ws ^ClientHandshake hs]
      (println "Scratch connected from" (.getRemoteSocketAddress ws)
               "path:" (.getResourceDescriptor hs))
      (swap! *sessions assoc ws (atom {:running?            true
                                       :config              config
                                       :conn                nil
                                       :ws                  ws
                                       :pending-ir-rewrites (atom nil)})))

    (onClose [^WebSocket ws code reason remote?]
      (println "Scratch disconnected:" reason)
      (when-let [sa (get @*sessions ws)]
        (swap! sa assoc :running? false)
        ;; Disconnecting the serial port can confuse things -- let the dev do this manually
        #_(when-let [conn (:conn @sa)] (ev3/disconnect-ev3 conn)))
      (swap! *sessions dissoc ws))

    (onMessage [^WebSocket ws ^String message]
      #_(println ">> WS" message)
      (try
        (let [req    (json/read-str message)
              method (get req "method")
              params (get req "params" {})
              id     (get req "id")
              sa     (get @*sessions ws)]
          (if sa
            (dispatch-request! ws id method params sa)
            (println "No session for websocket!")))
        (catch Exception e
          (println "Error handling message:" (.getMessage e)))))

    (onError [^WebSocket ws ^Exception e]
      (println "WebSocket error:" (.getMessage e)))

    (onStart []
      (println "Scratch Link proxy listening on port" port))))

(defn- make-ssl-context [^String keystore-path ^String password]
  (let [ks  (KeyStore/getInstance "JKS")
        pw  (.toCharArray password)
        _   (with-open [in (FileInputStream. keystore-path)]
              (.load ks in pw))
        kmf (KeyManagerFactory/getInstance (KeyManagerFactory/getDefaultAlgorithm))
        _   (.init kmf ks pw)
        ctx (SSLContext/getInstance "TLS")]
    (.init ctx (.getKeyManagers kmf) nil nil)
    ctx))

;;;; -------------------------------------------------------------------------
;;;; Public API
;;;; -------------------------------------------------------------------------

(defn stop-server! []
  (when-let [srv ^WebSocketServer @*server]
    (.stop srv)
    (reset! *server nil)
    (println "Scratch Link proxy stopped")))

(defn start-server!
  "Start the Scratch Link proxy.

   Options:
     :keystore    path to JKS keystore file (default: 'scratch-link.jks')
     :password    keystore password (default: 'scratch')
     :port        WSS port (default: 20110)
     :ev3-addr    Bluetooth MAC address of the EV3 (default: '00:16:53:52:A4:C4')
     :ev3-name    Name reported to Scratch during discovery (default: 'EV3')
     :rfcomm-port RFCOMM device path (default: '/dev/rfcomm0')"
  ([] (start-server! {}))
  ([opts]
   (when @*server
     (println "Server already running; stopping first")
     (stop-server!))
   (let [config {:ev3-addr    (get opts :ev3-addr    "00:16:53:52:A4:C4")
                 :ev3-name    (get opts :ev3-name    "EV3")
                 :rfcomm-port (get opts :rfcomm-port "/dev/rfcomm0")
                 :ir-port     (get opts :ir-port nil)}
         port   (get opts :port 20110)
         ks     (get opts :keystore "scratch-link.jks")
         pw     (get opts :password "scratch")
         srv    (make-server port config)
         ssl    (make-ssl-context ks pw)]
     (.setWebSocketFactory srv (DefaultSSLWebSocketServerFactory. ssl))
     (.start srv)
     (reset! *server srv)
     (println "Scratch Link proxy started on port" port)
     srv)))

;;;; -------------------------------------------------------------------------
;;;; REPL examples
;;;; -------------------------------------------------------------------------

(comment
  ;; Start the server
  (start-server! {:ev3-addr    "00:16:53:52:A4:C4"
                  :ev3-name    "EV3"
                  :ir-port     3  ;; 3 == "port 4" where the IR sensor is connected
                  :rfcomm-port "/dev/rfcomm0"
                  :keystore    "scratch-link.jks"
                  :password    "scratch"})

  ;; Stop it
  (stop-server!)

;; $ sudo rfcomm connect 0 00:16:53:52:A4:C4 1
;; Connected /dev/rfcomm0 to 00:16:53:52:A4:C4 on channel 1
;; Press CTRL-C for hangup

;; If that's not working, try killing all processes holding the files open:
;; $ sudo fuser -k /dev/rfcomm*

;; Then release all the rfcomm connections:
;; $ sudo rfcomm release all

;; Then try again to make the connection. Closing the serial port seems to confuse everything.

  ;; These are unnecessary excdept to test or recover from shutdown
  (ev3/connect! "dev/rfcomm0")
  (ev3/get-name @ev3/*ev3conn)
  (ev3/disconnect-ev3 @ev3/*ev3conn)
  (reset! ev3/*ev3conn nil)

  )
