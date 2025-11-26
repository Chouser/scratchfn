(ns us.chouser.gen-scratch
  (:require [clojure.data.json :as json])
  (:import [java.util.zip ZipOutputStream ZipEntry]
           [java.io FileOutputStream]
           [java.security MessageDigest]))

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

(defn op-equals [OPERAND1 OPERAND2]
  {:opcode "operator_equals"
   :inputs {:OPERAND1 OPERAND1
            :OPERAND2 OPERAND2}})

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

(defn do-block
  "Chain blocks together using :next. Takes multiple blocks and links them sequentially."
  [& blocks]
  (reduce (fn [acc block]
            (assoc block :next acc))
          nil
          (reverse blocks)))

(defn looks-show []
  {:opcode "looks_show"})

(defn looks-hide []
  {:opcode "looks_hide"})

;; ============================================================================
;; Script Generation for Lesson Sprites
;; ============================================================================

(declare flatten-block)

(defn flatten-blockmap [parent-id blockmap]
  (let [flats (map (partial flatten-block parent-id) (vals blockmap))]
    {:map (zipmap (keys blockmap) (map :top flats))
     :blocks (mapcat :blocks flats)}))

;; return a seq of [id node], where the first in the sequence is the "top"
(defn flatten-block
  [parent-id block]
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

(defn generate-script-1
  "When this sprite clicked - toggle lesson completion"
  [{:keys [completed-lessons lesson-num rebuild-state]} x y]
  (top-level-block
   (event-when-sprite-clicked :x x :y y)
   (control-if
    (op-not (op-contains? completed-lessons (number-input lesson-num)))
    (do-block
     (data-add-to-list completed-lessons (number-input lesson-num))
     (event-broadcast rebuild-state)))))

(defn generate-script-3
  "When receiving add your topics"
  [{:keys [learned-topics completed-lessons is-completed my-intros lesson-num add-your-topics topic counter] :as ctx} x y]
  (top-level-block
   (event-when-broadcast-received add-your-topics :x x :y y)
   (data-set-variable is-completed (op-contains? completed-lessons (number-input lesson-num)))
   (control-if
    (op-equals (data-variable is-completed) (text-input "true"))
    (do-block
     (data-set-variable counter (number-input 1))
     (control-repeat
      (data-length-of-list my-intros)
      (do-block
       (data-set-variable topic (data-item-of-list my-intros (data-variable counter)))
       (control-if
        (op-not (op-contains? learned-topics (data-variable topic)))
        (data-add-to-list learned-topics (data-variable topic)))
       (data-change-variable counter (number-input 1))))))))

(defn generate-script-4
  "When receiving topics updated - show/hide based on availability"
  [{:keys [learned-topics available my-uses is-completed topic counter topics-updated] :as ctx} x y]
  (top-level-block
   (event-when-broadcast-received topics-updated :x x :y y)
   (data-set-variable available (text-input "true"))
   (data-set-variable counter (number-input 1))
   (control-repeat
    (data-length-of-list my-uses)
    (do-block
     (data-set-variable topic (data-item-of-list my-uses (data-variable counter)))
     (control-if
      (op-not (op-contains? learned-topics (data-variable topic)))
      (data-set-variable available (text-input "false")))
     (data-change-variable counter (number-input 1))))
   (control-if-else
    (op-and
     (op-equals (data-variable available) (text-input "true"))
     (op-not (op-equals (data-variable is-completed) (text-input "true"))))
    (looks-show)
    (looks-hide))))

;; ============================================================================
;; Costume Generation
;; ============================================================================

(defn generate-stage-backdrop []
  "<svg version=\"1.1\" width=\"480\" height=\"360\" viewBox=\"0 0 480 360\">
  <rect width=\"480\" height=\"360\" fill=\"#ffffff\"/>
</svg>")

(defn generate-back-button-costume []
  "<svg version=\"1.1\" width=\"80\" height=\"40\" viewBox=\"0 0 80 40\">
  <rect width=\"80\" height=\"40\" fill=\"#ff6b6b\" stroke=\"#000000\" stroke-width=\"2\"/>
  <text x=\"40\" y=\"25\" text-anchor=\"middle\" font-family=\"Arial\" font-size=\"16\" fill=\"#ffffff\">Back</text>
</svg>")

(defn generate-lesson-costume [lesson-name]
  (format "<svg version=\"1.1\" width=\"90\" height=\"60\" viewBox=\"0 0 90 60\">
  <rect width=\"90\" height=\"60\" fill=\"#4287f5\" stroke=\"#000000\" stroke-width=\"2\"/>
  <text x=\"45\" y=\"35\" text-anchor=\"middle\" font-family=\"Arial\" font-size=\"12\" fill=\"#ffffff\">%s</text>
</svg>" lesson-name))

(defn create-costume [svg-content name]
  (let [hash (md5-hash svg-content)
        md5ext (str hash ".svg")]
    {:costume {:assetId hash
               :name name
               :md5ext md5ext
               :dataFormat "svg"
               :rotationCenterX 45
               :rotationCenterY 30}
     :file-name md5ext
     :content svg-content}))

;; ============================================================================
;; Sprite Generation
;; ============================================================================

(defn lesson-position [lesson-num]
  (let [col (mod (dec lesson-num) 6)
        row (quot (dec lesson-num) 6)]
    {:x (- (* col 100) 250)
     :y (- 150 (* row 80))}))

(defn create-lesson-sprite [ctx lesson]
  (let [lesson-num (:lessonNumber lesson)

        ctx (merge ctx
                   (make-variables {:is-completed false
                                    :available false
                                    :topic ""
                                    :counter 0})
                   (make-lists {:my-intros (:intros lesson)
                                :my-uses (:uses lesson)})
                   {:lesson-num lesson-num})

        pos (lesson-position lesson-num)
        all-blocks (merge (generate-script-1 ctx (:x pos) (:y pos))
                          (generate-script-3 ctx (+ 1000 (:x pos)) (:y pos))
                          (generate-script-4 ctx (+ 1500 (:x pos)) (:y pos)))
        costume-data (create-costume (generate-lesson-costume (:name lesson)) (:name lesson))]
    {:target {:isStage false
              :name (:name lesson)
              :variables (into {} (keep :variable) (vals ctx))
              :lists (into {} (keep :list) (vals ctx))
              :broadcasts {}
              :blocks all-blocks
              :comments {}
              :currentCostume 0
              :costumes [(:costume costume-data)]
              :sounds []
              :volume 100
              :layerOrder (inc lesson-num)
              :visible true
              :x (:x pos)
              :y (:y pos)
              :size 100
              :direction 90
              :draggable false
              :rotationStyle "all around"}
     :assets [costume-data]}))

(defn create-back-button-sprite [{:keys [completed-lessons rebuild-state]}]
  (let [blocks (top-level-block
                (event-when-sprite-clicked :x -200 :y -150)
                (data-delete-from-list completed-lessons (number-input "last"))
                (event-broadcast rebuild-state))

        costume-data (create-costume (generate-back-button-costume) "back-button")]

    {:target {:isStage false
              :name "BackButton"
              :variables {}
              :lists {}
              :broadcasts {}
              :blocks blocks
              :comments {}
              :currentCostume 0
              :costumes [(:costume costume-data)]
              :sounds []
              :volume 100
              :layerOrder 1
              :visible true
              :x -200
              :y -150
              :size 100
              :direction 90
              :draggable false
              :rotationStyle "all around"}
     :assets [costume-data]}))

(defn create-stage [{:keys [learned-topics rebuild-state add-your-topics topics-updated] :as ctx}]
  (let [;; Initial flag script to trigger topics updated
        flagclicked (top-level-block
                     (event-when-flag-clicked :x 0 :y 0)
                     (event-broadcast rebuild-state))
        rebuildstate (top-level-block
                      (event-when-broadcast-received rebuild-state :x 500 :y 0)
                      (data-delete-all-list learned-topics)
                      (event-broadcast-and-wait add-your-topics)
                      (event-broadcast topics-updated))
        backdrop-data (create-costume (generate-stage-backdrop) "backdrop1")]
    {:target {:isStage true
              :name "Stage"
              :variables {}
              :lists (into {} (keep :stage-list) (vals ctx))
              :broadcasts (into {} (keep :broadcast) (vals ctx))
              :blocks (merge flagclicked rebuildstate)
              :comments {}
              :currentCostume 0
              :costumes [(:costume backdrop-data)]
              :sounds []
              :volume 100
              :layerOrder 0
              :tempo 60
              :videoTransparency 50
              :videoState "off"
              :textToSpeechLanguage nil}
     :assets [backdrop-data]}))

;; ============================================================================
;; Main Generation Function
;; ============================================================================

(defn generate-sb3 [lessons output-sb3-path]
  (binding [*block-counter* 0]
    (let [ctx (merge (make-stage-lists {:learned-topics []
                                        :completed-lessons []})
                     (make-broadcasts [:rebuild-state :add-your-topics :topics-updated]))

          builds (-> [(create-stage ctx)
                      (create-back-button-sprite ctx)]
                     (into (map #(create-lesson-sprite ctx %) lessons)))

          ;; Create project structure
          project {:targets (mapv :target builds)
                   :monitors []
                   :extensions []
                   :meta {:semver "3.0.0"
                          :vm "0.2.0"
                          :agent ""}}]

      ;; Write ZIP file
      (with-open [zip (ZipOutputStream. (FileOutputStream. output-sb3-path))]
        ;; Add project.json
        (.putNextEntry zip (ZipEntry. "project.json"))
        (.write zip (.getBytes (json/write-str project) "UTF-8"))
        (.closeEntry zip)

        ;; Add asset files
        (doseq [asset (mapcat :assets builds)]
          (.putNextEntry zip (ZipEntry. (:file-name asset)))
          (.write zip (.getBytes (:content asset) "UTF-8"))
          (.closeEntry zip)))

      (println (str "Successfully generated " output-sb3-path)))))
