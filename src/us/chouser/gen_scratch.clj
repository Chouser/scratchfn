(ns us.chouser.gen-scratch
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io])
  (:import [java.util.zip ZipOutputStream ZipEntry]
           [java.io FileOutputStream]
           [java.security MessageDigest]
           [java.util Base64]))

;; ============================================================================
;; Utility Functions
;; ============================================================================

(defn generate-id []
  "Generate a unique ID for Scratch objects"
  (str (java.util.UUID/randomUUID)))

(defn md5-hash [content]
  "Calculate MD5 hash of content"
  (let [digest (MessageDigest/getInstance "MD5")
        bytes (.digest digest (.getBytes content "UTF-8"))]
    (apply str (map #(format "%02x" %) bytes))))

(defn file-to-base64 [file-path]
  (let [file-path (if (.exists (io/file file-path))
                    file-path
                    "thumbs/Jumpy Monkey.jpg")
        bytes (with-open [in (io/input-stream file-path)]
                (let [baos (java.io.ByteArrayOutputStream.)]
                  (io/copy in baos)
                  (.toByteArray baos)))]
    (.encodeToString (Base64/getEncoder) bytes)))

;; ============================================================================
;; Block Generation Helpers
;; ============================================================================

(def ^:dynamic *block-counter*)

(defn text-input [value]
  "Create a text input value"
  [1 [10 value]])

(defn number-input [value]
  "Create a number input value"
  [1 [4 (str value)]])

;; ============================================================================
;; Opcode Constructor Functions
;; ============================================================================

(defn op-contains? [LIST ITEM]
  {:opcode "data_listcontainsitem"
   :fields {:LIST (or (:as-list LIST) (throw (ex-info "Expected list" {:list LIST})))}
   :inputs {:ITEM ITEM}})

(defn op-not [OPERAND]
  {:opcode "operator_not"
   :inputs {:OPERAND OPERAND}})

(defn op-and [OPERAND1 OPERAND2]
  {:opcode "operator_and"
   :inputs {:OPERAND1 OPERAND1
            :OPERAND2 OPERAND2}})

(defn op-or [OPERAND1 OPERAND2]
  {:opcode "operator_or"
   :inputs {:OPERAND1 OPERAND1
            :OPERAND2 OPERAND2}})

(defn op-equals [OPERAND1 OPERAND2]
  {:opcode "operator_equals"
   :inputs {:OPERAND1 OPERAND1
            :OPERAND2 OPERAND2}})

(defn op-gt [OPERAND1 OPERAND2]
  {:opcode "operator_gt"
   :inputs {:OPERAND1 OPERAND1
            :OPERAND2 OPERAND2}})

(defn op-+ [NUM1 NUM2]
  {:opcode "operator_add"
   :inputs {:NUM1 NUM1
            :NUM2 NUM2}})

(defn op-- [NUM1 NUM2]
  {:opcode "operator_subtract"
   :inputs {:NUM1 NUM1
            :NUM2 NUM2}})

(defn op-* [NUM1 NUM2]
  {:opcode "operator_multiply"
   :inputs {:NUM1 NUM1
            :NUM2 NUM2}})

(defn op-divide [NUM1 NUM2]
  {:opcode "operator_divide"
   :inputs {:NUM1 NUM1
            :NUM2 NUM2}})

(defn data-variable [VARIABLE]
  {:opcode "data_variable"
   :fields {:VARIABLE (or (:as-variable VARIABLE) (throw (ex-info "Expected variable" {:variable VARIABLE})))}})

(defn data-set-variable [VARIABLE VALUE]
  {:opcode "data_setvariableto"
   :fields {:VARIABLE (or (:as-variable VARIABLE) (throw (ex-info "Expected variable" {:variable VARIABLE})))}
   :inputs {:VALUE VALUE}})

(defn data-change-variable [VARIABLE VALUE]
  {:opcode "data_changevariableby"
   :fields {:VARIABLE (or (:as-variable VARIABLE) (throw (ex-info "Expected variable" {:variable VARIABLE})))}
   :inputs {:VALUE VALUE}})

(defn data-add-to-list [LIST ITEM]
  {:opcode "data_addtolist"
   :fields {:LIST (or (:as-list LIST) (throw (ex-info "Expected list" {:list LIST})))}
   :inputs {:ITEM ITEM}})

(defn data-replace-list-item [LIST INDEX ITEM]
  {:opcode "data_replaceitemoflist"
   :fields {:LIST (or (:as-list LIST) (throw (ex-info "Expected list" {:list LIST})))}
   :inputs {:INDEX INDEX, :ITEM ITEM}})

(defn data-delete-from-list [LIST INDEX]
  {:opcode "data_deleteoflist"
   :fields {:LIST (or (:as-list LIST) (throw (ex-info "Expected list" {:list LIST})))}
   :inputs {:INDEX INDEX}})

(defn data-delete-all-list [LIST]
  {:opcode "data_deletealloflist"
   :fields {:LIST (or (:as-list LIST) (throw (ex-info "Expected list" {:list LIST})))}})

(defn data-item-of-list [LIST INDEX]
  {:opcode "data_itemoflist"
   :fields {:LIST (or (:as-list LIST) (throw (ex-info "Expected list" {:list LIST})))}
   :inputs {:INDEX INDEX}})

(defn data-length-of-list [LIST]
  {:opcode "data_lengthoflist"
   :fields {:LIST (or (:as-list LIST) (throw (ex-info "Expected list" {:list LIST})))}})

(defn control-if [CONDITION SUBSTACK]
  {:opcode "control_if"
   :inputs {:CONDITION CONDITION
            :SUBSTACK SUBSTACK}})

(defn control-if-else [CONDITION SUBSTACK SUBSTACK2]
  {:opcode "control_if_else"
   :inputs {:CONDITION CONDITION
            :SUBSTACK SUBSTACK
            :SUBSTACK2 SUBSTACK2}})

(defn control-repeat [TIMES SUBSTACK]
  {:opcode "control_repeat"
   :inputs {:TIMES TIMES
            :SUBSTACK SUBSTACK}})

(defn control-forever [SUBSTACK]
  {:opcode "control_forever"
   :inputs {:SUBSTACK SUBSTACK}})

(defn control-wait [DURATION]
  {:opcode "control_wait"
   :inputs {:DURATION DURATION}})

(defn event-broadcast [BROADCAST_INPUT]
  {:opcode "event_broadcast"
   :inputs {:BROADCAST_INPUT (or (:as-input BROADCAST_INPUT) (throw (ex-info "Expected broadcast" {:broadcast BROADCAST_INPUT})))}})

(defn event-broadcast-and-wait [BROADCAST_INPUT]
  {:opcode "event_broadcastandwait"
   :inputs {:BROADCAST_INPUT (or (:as-input BROADCAST_INPUT) (throw (ex-info "Expected broadcast" {:broadcast BROADCAST_INPUT})))}})

(defn event-when-flag-clicked [& {:as opts}]
  (merge {:opcode "event_whenflagclicked"
          :topLevel true}
         opts))

(defn event-when-sprite-clicked [& {:as opts}]
  (merge {:opcode "event_whenthisspriteclicked"
          :topLevel true}
         opts))

(defn event-when-broadcast-received [BROADCAST_OPTION & {:as opts}]
  (merge {:opcode "event_whenbroadcastreceived"
          :topLevel true
          :fields {:BROADCAST_OPTION (or (:as-variable BROADCAST_OPTION) (throw (ex-info "Expected broadcast" {:broadcast BROADCAST_OPTION})))}}
         opts))

(defn motion-change-x-by [DX] {:opcode "motion_changexby", :inputs {:DX DX}})
(defn motion-change-y-by [DY] {:opcode "motion_changeyby", :inputs {:DY DY}})
(defn motion-goto-random [] {:opcode "motion_goto",
                             :inputs {:TO {:opcode "motion_goto_menu",
                                           :fields {:TO ["_random_" nil]}}}})

(defn motion-x-position [] {:opcode "motion_xposition"})
(defn motion-y-position [] {:opcode "motion_yposition"})
(defn looks-show [] {:opcode "looks_show"})
(defn looks-hide [] {:opcode "looks_hide"})
(defn looks-change-effect-by [EFFECT CHANGE]
  (assert (contains? #{:GHOST} EFFECT))
  {:opcode "looks_changeeffectby"
   :fields {:EFFECT [EFFECT nil]}
   :inputs {:CHANGE CHANGE}})

(defn do-block
  "Chain blocks together using :next. Takes multiple blocks and links them sequentially."
  [& blocks]
  (reduce (fn [acc block]
            (assoc block :next acc))
          nil
          (reverse blocks)))

;; ============================================================================
;; Script Generation for Lesson Sprites
;; ============================================================================

(declare flatten-block)

(defn flatten-blockmap [parent-id blockmap]
  (let [flats (map (partial flatten-block parent-id) (vals blockmap))]
    {:map (zipmap (keys blockmap) (map :top flats))
     :blocks (mapcat :blocks flats)}))

;; return a seq of [id node], where the first in the sequence is the "top"
(defn flatten-block [parent-id block]
  (if-not (map? block)
    {:top block}
    (let [id (str "block" (set! *block-counter* (inc *block-counter*)))
          fields (->> block :fields (flatten-blockmap id))
          inputs (->> block :inputs (flatten-blockmap id))
          next-flat (->> block :next (flatten-block parent-id))]
      {:top [2 id] ;; refer to block by id
       :blocks (into [[id (merge {:shadow false
                                  :topLevel false}
                                 block
                                 {:parent parent-id
                                  :inputs (:map inputs)
                                  :fields (:map fields)
                                  :next (second (:top next-flat))})]]
                     (mapcat :blocks [fields inputs next-flat]))})))

(defn top-level-block [& blocks]
  (->> (apply do-block blocks) (flatten-block nil) :blocks (into {})))

(defn make-variables [m]
  (->> (map (fn [[k init] id]
              [k {:variable [id [k init]]
                  :as-variable [k id]}])
            m (repeatedly generate-id))
       (into {})))

(defn make-stage-variables [m]
  (->> (map (fn [[k init] id]
              [k {:stage-variable [id [k init]]
                  :as-variable [k id]}])
            m (repeatedly generate-id))
       (into {})))

(defn make-lists [m]
  (->> (map (fn [[k init] id]
              [k {:list [id [k init]]
                  :as-list [k id]}])
            m (repeatedly generate-id))
       (into {})))

(defn make-stage-lists [m]
  (->> (map (fn [[k init] id]
              [k {:stage-list [id [k init]]
                  :as-list [k id]}])
            m (repeatedly generate-id))
       (into {})))

(defn make-broadcasts [ks]
  (->> (map (fn [k id]
              [k {:broadcast [id k]
                  :as-input [1 [11 k id]]
                  :as-variable [k id]}])
            ks (repeatedly generate-id))
       (into {})))

(defn generate-sb3 [output-sb3-path builds]
  (let [assets (cons
                {:file-name "project.json"
                 :content (-> {:targets (mapv :target builds)
                               :monitors []
                               :extensions []
                               :meta {:semver "3.0.0"
                                      :vm "0.2.0"
                                      :agent ""}}
                              json/write-str)}
                (mapcat :assets builds))]

    ;; Write ZIP file
    (with-open [zip (ZipOutputStream. (FileOutputStream. output-sb3-path))]
      (doseq [asset assets]
        (doto zip
          (.putNextEntry (ZipEntry. (:file-name asset)))
          (.write (.getBytes (:content asset) "UTF-8"))
          (.closeEntry))))))
