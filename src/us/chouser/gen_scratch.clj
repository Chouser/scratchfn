(ns us.chouser.gen-scratch
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io])
  (:import [java.util.zip ZipOutputStream ZipEntry]
           [java.io FileOutputStream ByteArrayOutputStream]
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

(defn next-block-id []
  (str "block" (set! *block-counter* (inc *block-counter*))))

(defn text-input [value]
  "Create a text input value"
  [1 [10 value]])

(defn number-input [value]
  "Create a number input value"
  [1 [4 (str value)]])

(defn block-input [block-id]
  "Reference another block as input"
  [2 block-id])

(defn broadcast-input [name id]
  "Create a broadcast input"
  [1 [11 name id]])

(defn variable-field [name id]
  "Create a variable field"
  [name id])

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
    (let [id (next-block-id)
          fields (->> block :fields (flatten-blockmap id))
          inputs (->> block :inputs (flatten-blockmap id))
          next-flat (->> block :next (flatten-block parent-id))]
      {:top (block-input id)
       :blocks (into [[id (merge {:shadow false
                                  :topLevel false}
                                 block
                                 {:parent parent-id
                                  :inputs (:map inputs)
                                  :fields (:map fields)
                                  :next (second (:top next-flat))})]]
                     (mapcat :blocks [fields inputs next-flat]))})))

(defn top-level-block [op]
  (->> op (flatten-block nil) :blocks (into {})))

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
   {:opcode "event_whenthisspriteclicked"
    :topLevel true :x x :y y
    :next {:opcode "control_if"
           :inputs {:CONDITION
                    {:opcode "operator_not"
                     :inputs {:OPERAND
                              {:opcode "data_listcontainsitem"
                               :fields {:LIST (:as-list completed-lessons)}
                               :inputs {:ITEM (number-input lesson-num)}}}}
                    :SUBSTACK
                    {:opcode "data_addtolist"
                     :fields {:LIST (:as-list completed-lessons)}
                     :inputs {:ITEM (number-input lesson-num)}}}
           :next {:opcode "event_broadcast"
                  :inputs {:BROADCAST_INPUT (:as-input rebuild-state)}}}}))

(defn generate-script-3
  "When receiving add your topics"
  [{:keys [learned-topics completed-lessons is-completed my-intros lesson-num add-your-topics topic counter] :as ctx} x y]
  (top-level-block
   {:opcode "event_whenbroadcastreceived"
    :topLevel true :x x :y y
    :fields {:BROADCAST_OPTION (:as-variable add-your-topics)}
    :next {:opcode "data_setvariableto"
           :fields {:VARIABLE (:as-variable is-completed)}
           :inputs {:VALUE {:opcode "data_listcontainsitem"
                            :fields {:LIST (:as-list completed-lessons)}
                            :inputs {:ITEM (number-input lesson-num)}}}
           :next {:opcode "control_if"
                  :inputs {:CONDITION
                           {:opcode "operator_equals"
                            :inputs {:OPERAND1
                                     {:opcode "data_variable"
                                      :fields {:VARIABLE (:as-variable is-completed)}}
                                     :OPERAND2 (text-input "true")}}
                           :SUBSTACK
                           {:opcode "data_setvariableto"
                            :fields {:VARIABLE (:as-variable counter)}
                            :inputs {:VALUE (number-input 1)}
                            :next {:opcode "control_repeat"
                                   :inputs {:TIMES
                                            {:opcode "data_lengthoflist"
                                             :fields {:LIST (:as-list my-intros)}}
                                            :SUBSTACK
                                            {:opcode "data_setvariableto"
                                             :fields {:VARIABLE (:as-variable topic)}
                                             :inputs {:VALUE
                                                      {:opcode "data_itemoflist"
                                                       :fields {:LIST (:as-list my-intros)}
                                                       :inputs {:INDEX
                                                                {:opcode "data_variable"
                                                                 :fields {:VARIABLE (:as-variable counter)}}}}}
                                             :next {:opcode "control_if"
                                                    :inputs {:CONDITION
                                                             {:opcode "operator_not"
                                                              :inputs {:OPERAND
                                                                       {:opcode "data_listcontainsitem"
                                                                        :fields {:LIST (:as-list learned-topics)}
                                                                        :inputs {:ITEM
                                                                                 {:opcode "data_variable"
                                                                                  :fields {:VARIABLE (:as-variable topic)}}}}}}
                                                             :SUBSTACK
                                                             {:opcode "data_addtolist"
                                                              :fields {:LIST (:as-list learned-topics)}
                                                              :inputs {:ITEM
                                                                       {:opcode "data_variable"
                                                                        :fields {:VARIABLE (:as-variable topic)}}}}}
                                                    :next {:opcode "data_changevariableby"
                                                           :fields {:VARIABLE (:as-variable counter)}
                                                           :inputs {:VALUE (number-input 1)}}}}}}}}}}}))

(defn generate-script-4
  "When receiving topics updated - show/hide based on availability"
  [{:keys [learned-topics available my-uses is-completed topic counter topics-updated] :as ctx} x y]
  (top-level-block
   {:opcode "event_whenbroadcastreceived"
    :topLevel true :x x :y y
    :fields {:BROADCAST_OPTION (:as-variable topics-updated)}
    :next {:opcode "data_setvariableto"
           :fields {:VARIABLE (:as-variable available)}
           :inputs {:VALUE (text-input "true")}
           :next {:opcode "data_setvariableto"
                  :fields {:VARIABLE (:as-variable counter)}
                  :inputs {:VALUE (number-input 1)}
                  :next {:opcode "control_repeat"
                         :inputs {:TIMES
                                  {:opcode "data_lengthoflist"
                                   :fields {:LIST (:as-list my-uses)}}
                                  :SUBSTACK
                                  {:opcode "data_setvariableto"
                                   :fields {:VARIABLE (:as-variable topic)}
                                   :inputs {:VALUE
                                            {:opcode "data_itemoflist"
                                             :fields {:LIST (:as-list my-uses)}
                                             :inputs {:INDEX
                                                      {:opcode "data_variable"
                                                       :fields {:VARIABLE (:as-variable counter)}}}}}
                                   :next {:opcode "control_if"
                                          :inputs {:CONDITION
                                                   {:opcode "operator_not"
                                                    :inputs {:OPERAND
                                                             {:opcode "data_listcontainsitem"
                                                              :fields {:LIST (:as-list learned-topics)}
                                                              :inputs {:ITEM
                                                                       {:opcode "data_variable"
                                                                        :fields {:VARIABLE (:as-variable topic)}}}}}}
                                                   :SUBSTACK
                                                   {:opcode "data_setvariableto"
                                                    :fields {:VARIABLE (:as-variable available)}
                                                    :inputs {:VALUE (text-input "false")}}}
                                          :next {:opcode "data_changevariableby"
                                                 :fields {:VARIABLE (:as-variable counter)}
                                                 :inputs {:VALUE (number-input 1)}}}}}
                         :next {:opcode "control_if_else"
                                :inputs {:CONDITION
                                         {:opcode "operator_and"
                                          :inputs {:OPERAND1
                                                   {:opcode "operator_equals"
                                                    :inputs {:OPERAND1
                                                             {:opcode "data_variable"
                                                              :fields {:VARIABLE (:as-variable available)}}
                                                             :OPERAND2 (text-input "true")}}
                                                   :OPERAND2
                                                   {:opcode "operator_not"
                                                    :inputs {:OPERAND
                                                             {:opcode "operator_equals"
                                                              :inputs {:OPERAND1
                                                                       {:opcode "data_variable"
                                                                        :fields {:VARIABLE (:as-variable is-completed)}}
                                                                       :OPERAND2 (text-input "true")}}}}}}
                                         :SUBSTACK
                                         {:opcode "looks_show"}
                                         :SUBSTACK2
                                         {:opcode "looks_hide"}}}}}}}))

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
                {:opcode "event_whenthisspriteclicked"
                 :topLevel true :x -200 :y -150
                 :next {:opcode "data_deleteoflist"
                        :fields {:LIST (:as-list completed-lessons)}
                        :inputs {:INDEX (number-input "last")}
                        :next {:opcode "event_broadcast"
                               :inputs {:BROADCAST_INPUT (:as-input rebuild-state)}}}})

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
                     {:opcode "event_whenflagclicked"
                      :topLevel true :x 0 :y 0
                      :next {:opcode "event_broadcast"
                             :inputs {:BROADCAST_INPUT (:as-input rebuild-state)}}})
        rebuildstate (top-level-block
                      {:opcode "event_whenbroadcastreceived"
                       :topLevel true :x 500 :y 0
                       :fields {:BROADCAST_OPTION (:as-variable rebuild-state)}
                       :next {:opcode "data_deletealloflist"
                              :fields {:LIST (:as-list learned-topics)}
                              :next {:opcode "event_broadcastandwait"
                                     :inputs {:BROADCAST_INPUT (:as-input add-your-topics)}
                                     :next {:opcode "event_broadcast"
                                            :inputs {:BROADCAST_INPUT (:as-input topics-updated)}}}}})
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