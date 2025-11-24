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

(defn create-block
  "Create a basic block structure"
  [opcode & {:keys [next parent inputs fields shadow topLevel x y]
             :or {next nil parent nil inputs {} fields {} 
                  shadow false topLevel false x nil y nil}}]
  (cond-> {:opcode opcode
           :next next
           :parent parent
           :inputs inputs
           :fields fields
           :shadow shadow
           :topLevel topLevel}
    x (assoc :x x)
    y (assoc :y y)))

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

(defn list-field [name id]
  "Create a list field"
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

#_(flatten-blockmap 0 {:VARIABLE (variable-field "lessonNumber" 101010)})
#_(binding [*block-counter* 0]
  (flatten-block nil {:opcode "data_deleteoflist"
                  :fields {:LIST (list-field "completedLessons" 'completed-lessons-id)}
                  :inputs {:INDEX
                           {:opcode "data_itemnumoflist"
                            :fields {:LIST (list-field "completedLessons" 'completed-lessons-id)}
                            :inputs {:ITEM
                                     {:opcode "data_variable"
                                      :fields {:VARIABLE (variable-field "lessonNumber" 'lesson-num)}}}}}}))

(defn generate-script-1
  "When this sprite clicked - toggle lesson completion"
  [lesson-num completed-lessons-id x y broadcast-rebuild-id]
  (top-level-block
   {:opcode "event_whenthisspriteclicked"
    :topLevel true :x x :y y
    :next {:opcode "control_if"
           :inputs {:CONDITION
                    {:opcode "operator_not"
                     :inputs {:OPERAND
                              {:opcode "data_listcontainsitem"
                               :fields {:LIST (list-field "completedLessons" completed-lessons-id)}
                               :inputs {:ITEM
                                        {:opcode "data_variable"
                                         :fields {:VARIABLE (variable-field "lessonNumber" lesson-num)}}}}}}
                    :SUBSTACK
                    {:opcode "data_addtolist"
                     :fields {:LIST (list-field "completedLessons" completed-lessons-id)}
                     :inputs {:ITEM
                              {:opcode "data_variable"
                               :fields {:VARIABLE (variable-field "lessonNumber" lesson-num)}}}}}
           :next {:opcode "event_broadcast"
                  :inputs {:BROADCAST_INPUT (broadcast-input "rebuild state" broadcast-rebuild-id)}}}}))

(defn generate-script-2
  "When receiving rebuild state"
  [lesson-num-var-id is-completed-id completed-lessons-id 
   broadcast-rebuild-id broadcast-add-topics-id broadcast-topics-updated-id x y]
  (let [hat-id (next-block-id)
        set-var-id (next-block-id)
        contains-id (next-block-id)
        lesson-var-id (next-block-id)
        broadcast-wait-id (next-block-id)
        broadcast-id (next-block-id)]
    {hat-id (create-block "event_whenbroadcastreceived"
                          :next set-var-id :topLevel true :x x :y y
                          :fields {:BROADCAST_OPTION (variable-field "rebuild state" broadcast-rebuild-id)})
     set-var-id (create-block "data_setvariableto"
                              :parent hat-id :next broadcast-wait-id
                              :fields {:VARIABLE (variable-field "isCompleted" is-completed-id)}
                              :inputs {:VALUE (block-input contains-id)})
     contains-id (create-block "data_listcontainsitem"
                               :parent set-var-id
                               :fields {:LIST (list-field "completedLessons" completed-lessons-id)}
                               :inputs {:ITEM (block-input lesson-var-id)})
     lesson-var-id (create-block "data_variable"
                                 :parent contains-id
                                 :fields {:VARIABLE (variable-field "lessonNumber" lesson-num-var-id)})
     broadcast-wait-id (create-block "event_broadcastandwait"
                                     :parent set-var-id :next broadcast-id
                                     :inputs {:BROADCAST_INPUT (broadcast-input "add your topics" broadcast-add-topics-id)})
     broadcast-id (create-block "event_broadcast"
                                :parent broadcast-wait-id
                                :inputs {:BROADCAST_INPUT (broadcast-input "topics updated" broadcast-topics-updated-id)})}))

(defn generate-script-3
  "When receiving add your topics"
  [is-completed-id my-intros-id learned-topics-id broadcast-add-topics-id topic-var-id counter-var-id x y]
  (top-level-block
   {:opcode "event_whenbroadcastreceived"
    :topLevel true :x x :y y
    :fields {:BROADCAST_OPTION (variable-field "add your topics" broadcast-add-topics-id)}
    :next {:opcode "control_if"
           :inputs {:CONDITION
                    {:opcode "operator_equals"
                     :inputs {:OPERAND1
                              {:opcode "data_variable"
                               :fields {:VARIABLE (variable-field "isCompleted" is-completed-id)}}
                              :OPERAND2 (text-input "true")}}
                    :SUBSTACK
                    {:opcode "data_setvariableto"
                     :fields {:VARIABLE (variable-field "counter" counter-var-id)}
                     :inputs {:VALUE (number-input 1)}
                     :next {:opcode "control_repeat"
                            :inputs {:TIMES
                                     {:opcode "data_lengthoflist"
                                      :fields {:LIST (list-field "myIntros" my-intros-id)}}
                                     :SUBSTACK
                                     {:opcode "data_setvariableto"
                                      :fields {:VARIABLE (variable-field "topic" topic-var-id)}
                                      :inputs {:VALUE
                                               {:opcode "data_itemoflist"
                                                :fields {:LIST (list-field "myIntros" my-intros-id)}
                                                :inputs {:INDEX
                                                         {:opcode "data_variable"
                                                          :fields {:VARIABLE (variable-field "counter" counter-var-id)}}}}}
                                      :next {:opcode "control_if"
                                             :inputs {:CONDITION
                                                      {:opcode "operator_not"
                                                       :inputs {:OPERAND
                                                                {:opcode "data_listcontainsitem"
                                                                 :fields {:LIST (list-field "learnedTopics" learned-topics-id)}
                                                                 :inputs {:ITEM
                                                                          {:opcode "data_variable"
                                                                           :fields {:VARIABLE (variable-field "topic" topic-var-id)}}}}}}
                                                      :SUBSTACK
                                                      {:opcode "data_addtolist"
                                                       :fields {:LIST (list-field "learnedTopics" learned-topics-id)}
                                                       :inputs {:ITEM
                                                                {:opcode "data_variable"
                                                                 :fields {:VARIABLE (variable-field "topic" topic-var-id)}}}}}
                                             :next {:opcode "data_changevariableby"
                                                    :fields {:VARIABLE (variable-field "counter" counter-var-id)}
                                                    :inputs {:VALUE (number-input 1)}}}}}}}}}}))

(defn generate-script-4
  "When receiving topics updated - show/hide based on availability"
  [available-id my-uses-id learned-topics-id is-completed-id topic-var-id counter-var-id broadcast-topics-updated-id x y]
  (top-level-block
   {:opcode "event_whenbroadcastreceived"
    :topLevel true :x x :y y
    :fields {:BROADCAST_OPTION (variable-field "topics updated" broadcast-topics-updated-id)}
    :next {:opcode "data_setvariableto"
           :fields {:VARIABLE (variable-field "available" available-id)}
           :inputs {:VALUE (text-input "true")}
           :next {:opcode "data_setvariableto"
                  :fields {:VARIABLE (variable-field "counter" counter-var-id)}
                  :inputs {:VALUE (number-input 1)}
                  :next {:opcode "control_repeat"
                         :inputs {:TIMES
                                  {:opcode "data_lengthoflist"
                                   :fields {:LIST (list-field "myUses" my-uses-id)}}
                                  :SUBSTACK
                                  {:opcode "data_setvariableto"
                                   :fields {:VARIABLE (variable-field "topic" topic-var-id)}
                                   :inputs {:VALUE
                                            {:opcode "data_itemoflist"
                                             :fields {:LIST (list-field "myUses" my-uses-id)}
                                             :inputs {:INDEX
                                                      {:opcode "data_variable"
                                                       :fields {:VARIABLE (variable-field "counter" counter-var-id)}}}}}
                                   :next {:opcode "control_if"
                                          :inputs {:CONDITION
                                                   {:opcode "operator_not"
                                                    :inputs {:OPERAND
                                                             {:opcode "data_listcontainsitem"
                                                              :fields {:LIST (list-field "learnedTopics" learned-topics-id)}
                                                              :inputs {:ITEM
                                                                       {:opcode "data_variable"
                                                                        :fields {:VARIABLE (variable-field "topic" topic-var-id)}}}}}}
                                                   :SUBSTACK
                                                   {:opcode "data_setvariableto"
                                                    :fields {:VARIABLE (variable-field "available" available-id)}
                                                    :inputs {:VALUE (text-input "false")}}}
                                          :next {:opcode "data_changevariableby"
                                                 :fields {:VARIABLE (variable-field "counter" counter-var-id)}
                                                 :inputs {:VALUE (number-input 1)}}}}}
                         :next {:opcode "control_if_else"
                                :inputs {:CONDITION
                                         {:opcode "operator_and"
                                          :inputs {:OPERAND1
                                                   {:opcode "operator_equals"
                                                    :inputs {:OPERAND1
                                                             {:opcode "data_variable"
                                                              :fields {:VARIABLE (variable-field "available" available-id)}}
                                                             :OPERAND2 (text-input "true")}}
                                                   :OPERAND2
                                                   {:opcode "operator_not"
                                                    :inputs {:OPERAND
                                                             {:opcode "operator_equals"
                                                              :inputs {:OPERAND1
                                                                       {:opcode "data_variable"
                                                                        :fields {:VARIABLE (variable-field "isCompleted" is-completed-id)}}
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

(defn create-lesson-sprite [lesson broadcasts learned-topics-id completed-lessons-id]
  (let [lesson-num (:lessonNumber lesson)
        lesson-name (:name lesson)
        intros (:intros lesson)
        uses (:uses lesson)

        ;; Generate IDs
        lesson-num-var-id (generate-id)
        is-completed-id (generate-id)
        available-id (generate-id)
        my-intros-id (generate-id)
        my-uses-id (generate-id)
        topic-var-id (generate-id)
        counter-var-id (generate-id)

        ;; Position
        pos (lesson-position lesson-num)

        ;; Generate all scripts
        script1 (generate-script-1 lesson-num-var-id completed-lessons-id
                                   (:x pos) (:y pos) (:rebuild-state broadcasts))
        script2 (generate-script-2 lesson-num-var-id is-completed-id completed-lessons-id
                                   (:rebuild-state broadcasts) (:add-your-topics broadcasts)
                                   (:topics-updated broadcasts) (+ 500 (:x pos)) (:y pos))
        script3 (generate-script-3 is-completed-id my-intros-id learned-topics-id
                                   (:add-your-topics broadcasts) topic-var-id counter-var-id
                                   (+ 1000 (:x pos)) (:y pos))
        script4 (generate-script-4 available-id my-uses-id learned-topics-id is-completed-id
                                   topic-var-id counter-var-id (:topics-updated broadcasts)
                                   (+ 1500 (:x pos)) (:y pos))

        ;; Merge all blocks
        all-blocks (merge script1 script2 script3 script4)

        ;; Generate costume
        costume-data (create-costume (generate-lesson-costume lesson-name)
                                     (str "lesson" lesson-num))]

    {:sprite {:isStage false
              :name (str "L " lesson-name)
              :variables {lesson-num-var-id ["lessonNumber" lesson-num]
                          is-completed-id ["isCompleted" false]
                          available-id ["available" false]
                          topic-var-id ["topic" ""]
                          counter-var-id ["topic" 0]}
              :lists {my-intros-id ["myIntros" (vec intros)]
                      my-uses-id ["myUses" (vec uses)]}
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

(defn create-back-button-sprite [completed-lessons-id broadcast-rebuild-id]
  (let [hat-id (next-block-id)
        delete-id (next-block-id)
        broadcast-id (next-block-id)
        
        blocks {hat-id (create-block "event_whenthisspriteclicked"
                                     :next delete-id :topLevel true :x -200 :y -150)
                delete-id (create-block "data_deleteoflist"
                                        :parent hat-id :next broadcast-id
                                        :fields {:LIST (list-field "completedLessons" completed-lessons-id)}
                                        :inputs {:INDEX (number-input "last")})
                broadcast-id (create-block "event_broadcast"
                                           :parent delete-id
                                           :inputs {:BROADCAST_INPUT (broadcast-input "rebuild state" broadcast-rebuild-id)})}
        
        costume-data (create-costume (generate-back-button-costume) "back-button")]
    
    {:sprite {:isStage false
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

(defn create-stage [broadcasts learned-topics-id completed-lessons-id]
  (let [;; Initial flag script to trigger topics updated
        hat-id (next-block-id)
        broadcast-id (next-block-id)
        
        blocks {hat-id (create-block "event_whenflagclicked"
                                     :next broadcast-id :topLevel true :x 0 :y 0)
                broadcast-id (create-block "event_broadcast"
                                           :parent hat-id
                                           :inputs {:BROADCAST_INPUT (broadcast-input "topics updated" 
                                                                                     (:topics-updated broadcasts))})}
        
        backdrop-data (create-costume (generate-stage-backdrop) "backdrop1")]
    
    {:stage {:isStage true
             :name "Stage"
             :variables {}
             :lists {learned-topics-id ["learnedTopics" []]
                     completed-lessons-id ["completedLessons" []]}
             :broadcasts {(:rebuild-state broadcasts) "rebuild state"
                          (:add-your-topics broadcasts) "add your topics"
                          (:topics-updated broadcasts) "topics updated"}
             :blocks blocks
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

    ;; Read and parse JSON
    (let [;; Generate shared IDs
          learned-topics-id (generate-id)
          completed-lessons-id (generate-id)
          broadcasts {:rebuild-state (generate-id)
                      :add-your-topics (generate-id)
                      :topics-updated (generate-id)}

          ;; Create stage
          stage-data (create-stage broadcasts learned-topics-id completed-lessons-id)

          ;; Create back button
          back-button-data (create-back-button-sprite completed-lessons-id (:rebuild-state broadcasts))

          ;; Create lesson sprites
          lesson-sprites (map #(create-lesson-sprite % broadcasts learned-topics-id completed-lessons-id)
                              lessons)

          ;; Collect all targets and assets
          all-targets (concat [(:stage stage-data) (:sprite back-button-data)]
                              (map :sprite lesson-sprites))
          all-assets (concat (:assets stage-data) (:assets back-button-data)
                             (mapcat :assets lesson-sprites))

          ;; Create project structure
          project {:targets (vec all-targets)
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
        (doseq [asset all-assets]
          (.putNextEntry zip (ZipEntry. (:file-name asset)))
          (.write zip (.getBytes (:content asset) "UTF-8"))
          (.closeEntry zip)))

      (println (str "Successfully generated " output-sb3-path)))))

;; ============================================================================
;; Example Usage
;; ============================================================================

(comment
  ;; Example: Generate SB3 from lessons.json
  (generate-sb3 "lessons.json" "output.sb3"))
  
  ;; Example input JSON structure:
  ;; {
  ;;   "lessons": [
  ;;     {"lessonNumber": 1, "name": "Intro", "intro": ["basics"], "uses": []},
  ;;     {"lessonNumber": 2, "name": "Advanced", "intro": ["loops"], "uses": ["basics"]}
  ;;   ]
  ;; }
  