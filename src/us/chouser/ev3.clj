(ns us.chouser.ev3
  "Connect to a Lego EV3 connect to the Lego EV3 device over an RFCOMM
  (bluetooth) serial connection. Probably should be a separate library,
   but currently the only user is scratch-link-ev3, so colocate for now."
  (:import [com.fazecast.jSerialComm SerialPort]))

;; $ sudo rfcomm connect 0 00:16:53:52:A4:C4 1
;; Connected /dev/rfcomm0 to 00:16:53:52:A4:C4 on channel 1
;; Press CTRL-C for hangup

;; If that's not working, try killing all processes holding the files open:
;; $ sudo fuser -k /dev/rfcomm*

;; Then release all the rfcomm connections:
;; $ sudo rfcomm release all

;; Then try again to make the connection. Closing the serial port seems to confuse everything.

(defonce *ev3conn (atom nil))

(defn bytes-to-hex
  "Convert a byte array to hex string for debugging"
  [bytes]
  (apply str (map #(format "%02X " (byte %)) bytes)))

(defn connect-to-ev3
  "Connect to EV3 via serial port (e.g., /dev/rfcomm0)"
  [port-name]
  (try
    (println "Connecting to EV3 at" port-name)
    (let [serial-port (SerialPort/getCommPort port-name)
          _ (.setComPortParameters serial-port 115200 8 1 0)
          _ (.setComPortTimeouts serial-port SerialPort/TIMEOUT_READ_BLOCKING 1000 1000)
          opened? (.openPort serial-port)]
      (if opened?
        {:port-name port-name
         :port serial-port}
        (println "Failed to open port" port-name)))
    (catch Exception e
      (println "Failed to connect to EV3 at" port-name ":" e))))

(defn disconnect-ev3 [{:keys [port]}]
  (try
    (println "Disconnecting from EV3")
    (.closePort port)
    (catch Exception e
      (println "Error disconnecting:" e))))

(defn connect!
  "Not thread safe"
  [port-name]
  (if-let [conn @*ev3conn]
    (if (= port-name (:port-name conn))
      (do
        (println "EV3 already connected at" port-name)
        conn)
      (do
        (try
          (println "Disconnecting" (:port-name conn))
          (disconnect-ev3 conn)
          (catch Exception e
            (println "Error disconnecting" (:port-name conn) ":" e)))
        (reset! *ev3conn (connect-to-ev3 port-name))))
    (reset! *ev3conn (connect-to-ev3 port-name))))

(def DIRECT-COMMAND-NO-REPLY 0x80)
(def DIRECT-COMMAND-REPLY    0x00)
(def DIRECT_COMMAND-VM 0xD3)
(def DIRECT_COMMAND-PLAY-TONE 0x94)
(def DIRECT_COMMAND-STOP-ALL 0xA1)
(def VM_GET_BRICKNAME  0x0D)

(defn create-direct-command
  ([command-bytes]
   (create-direct-command command-bytes DIRECT-COMMAND-NO-REPLY 0))
  ([command-bytes command-type global-size]
   (let [counter [0x01 0x00]
         header  [(unchecked-byte command-type)
                  (unchecked-byte (bit-and global-size 0xFF))
                  (unchecked-byte (bit-shift-right global-size 8))]
         payload (concat counter header command-bytes)
         size    (count payload)
         size-bytes [(unchecked-byte (bit-and size 0xFF))
                     (unchecked-byte (bit-shift-right size 8))]]
     (byte-array (concat size-bytes payload)))))

(defn send-command [{:keys [port]} cmd]
  (try
    (let [bytes-written (.writeBytes port cmd (count cmd))]
      #_(println "Sent" bytes-written "bytes:" (bytes-to-hex cmd))
      (> bytes-written 0))
    (catch Exception e
      (println "Error sending command:" (.getMessage e))
      false)))

(defn read-reply [{:keys [port]} expected-bytes]
  (try
    (let [buf (byte-array expected-bytes)
          bytes-read (.readBytes port buf expected-bytes)]
      #_(println "Read" bytes-read "bytes:" (bytes-to-hex buf))
      buf)
    (catch Exception e
      (println "Error reading reply:" (.getMessage e))
      nil)))

(defn lcx [value]
  (cond
    (<= 0 value 255)   [(unchecked-byte 0x81) (unchecked-byte value)]
    :else              [(unchecked-byte 0x82)
                        (unchecked-byte (bit-and value 0xFF))
                        (unchecked-byte (bit-shift-right value 8))]))

(defn gvx
  "Encode a global variable reference"
  [offset]
  (if (<= offset 31)
    [(unchecked-byte (bit-or 0x60 offset))]   ; short form
    [(unchecked-byte 0xE1) (unchecked-byte offset)]))

(defn get-name [conn]
  (let [max-len    32 
        command    (byte-array
                     (concat
                       [(unchecked-byte DIRECT_COMMAND-VM)
                        (unchecked-byte VM_GET_BRICKNAME)]
                       (lcx max-len)
                       (gvx 0)))
        cmd        (create-direct-command command DIRECT-COMMAND-REPLY max-len)
        reply-size (+ 2 2 1 max-len)]
    (println "Sending get-name:" (bytes-to-hex cmd))
    (send-command conn cmd)
    (when-let [reply (read-reply conn reply-size)]
      (println "Status:" (format "%02X" (aget reply 4)))
      (let [name-str (->> (drop 5 reply)
                          (take-while #(not= % 0))
                          byte-array
                          String.)]
        (println "EV3 name:" name-str)
        name-str))))

(defn get-ir-reading [conn port]
  (let [global-mem 4
        command (byte-array
                  (concat
                    [0x99 0x1D  ; opINPUT_DEVICE, READY_SI
                     0x00       ; layer
                     (unchecked-byte port) ; port (0-indexed)
                     0x21       ; type: IR
                     0x00       ; mode: proximity
                     0x01]      ; nvalues
                    (gvx 0)))   ; destination: GV0
        cmd (create-direct-command command DIRECT-COMMAND-REPLY global-mem)
        reply-size (+ 2 2 1 global-mem)]
    (send-command conn cmd)
    (when-let [reply (read-reply conn reply-size)]
      (println "Status:" (format "%02X" (aget reply 4)))
      (-> (java.nio.ByteBuffer/wrap reply 5 4)
          (.order java.nio.ByteOrder/LITTLE_ENDIAN)
          (.getFloat)))))

(defn sound-beep
  [conn frequency duration]
  (let [volume 10
        command (byte-array
                 (concat
                  [(unchecked-byte DIRECT_COMMAND-PLAY-TONE)
                   (unchecked-byte 0x01)]
                  (lcx volume)
                  (lcx frequency)
                  (lcx duration)))

        cmd (create-direct-command command)]
    (println "Sending beep:" (bytes-to-hex cmd))
    (send-command conn cmd)))

(comment
  (connect! "dev/rfcomm0")

  (sound-beep @*ev3conn 500 500)
  (get-name @*ev3conn)

  (get-ir-reading @*ev3conn 3)

  (disconnect-ev3 @*ev3conn)
  (reset! *ev3conn nil)
  (get-name @*ev3conn)
  )

