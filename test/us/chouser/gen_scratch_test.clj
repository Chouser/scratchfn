(ns us.chouser.gen-scratch-test
  (:require [us.chouser.gen-scratch :as sg]))

;; TODO
;; - why do custom blocks not work yet
;; - clean up the generation and use of custom blocks, put that in gen-scratch
;; - why does dragging a variable out of an input _not_ leave behind a place to type!?
;; - can we generate stable ids everywhere and stop using uuids?
;; - rename all generator functions to be more pleasant.

;; ============================================================================
;; Test Generator Implementation
;; ============================================================================

(defn record-test
  "Generate a call to the custom 'record test' block"
  [test-name passed-condition]
  {:opcode "procedures_call"
   :mutation {:tagName "mutation"
              :children []
              :proccode "record test %s %b"
              :argumentids (str "[\"arg0\",\"arg1\"]")
              :warp "false"}
   :inputs {:arg0 (sg/as-input test-name)
            :arg1 (sg/as-input passed-condition)}})

(defn gen-operator-tests
  "Generate test blocks for operator category"
  [{:keys [temp]}]
  (sg/do-block
   (record-test "op-+" (sg/op-equals (sg/op-+ 2 3) 5))
   (record-test "op--" (sg/op-equals (sg/op-- 10 3) 7))
   (record-test "op-*" (sg/op-equals (sg/op-* 4 5) 20))
   (record-test "op-divide" (sg/op-equals (sg/op-divide 20 4) 5))
   (record-test "op-join" (sg/op-equals (sg/op-join "Hello" "World") "HelloWorld"))
   (record-test "op-length" (sg/op-equals (sg/op-length "test") 4))
   (record-test "op-letter-of" (sg/op-equals (sg/op-letter-of 1 "ABC") "A"))
   (record-test "op-contains-true" (sg/op-contains "hello" "ell"))
   (record-test "op-contains-false" (sg/op-not (sg/op-contains "hello" "xyz")))
   (record-test "op-mod" (sg/op-equals (sg/op-mod 10 3) 1))
   (record-test "op-round" (sg/op-equals (sg/op-round 3.7) 4))
   (record-test "op-mathop-abs" (sg/op-equals (sg/op-mathop "abs" -5) 5))
   (record-test "op-mathop-sqrt" (sg/op-equals (sg/op-mathop "sqrt" 9) 3))
   (record-test "op-equals-true" (sg/op-equals 5 5))
   (record-test "op-equals-false" (sg/op-not (sg/op-equals 5 6)))
   (record-test "op-gt-true" (sg/op-gt 10 5))
   (record-test "op-gt-false" (sg/op-not (sg/op-gt 5 10)))
   (record-test "op-lt-true" (sg/op-lt 5 10))
   (record-test "op-lt-false" (sg/op-not (sg/op-lt 10 5)))
   (record-test "op-and-true" (sg/op-and true true))

   (record-test "op-and-false" (sg/op-not (sg/op-and true false)))
   (record-test "op-or-true" (sg/op-or true false))
   (record-test "op-or-false" (sg/op-not (sg/op-or false false)))
   (record-test "op-not-true" (sg/op-not false))
   (record-test "op-not-false" (sg/op-not (sg/op-not true)))

   ;; Test random (just check it's in range)
   (sg/data-set-variable temp (sg/op-random 1 10))
   (record-test "op-random-min" (sg/op-or (sg/op-gt temp 0) (sg/op-equals temp 1)))
   (record-test "op-random-max" (sg/op-or (sg/op-lt temp 11) (sg/op-equals temp 10)))))

(defn gen-motion-tests
  "Generate test blocks for motion category"
  [_]
  (sg/do-block
   ;; Reset position
   (sg/motion-goto-xy 0 0)
   (sg/motion-point-in-direction 90)

   ;; Test goto-xy
   (sg/motion-goto-xy 50 30)
   (record-test "motion-goto-xy-x" (sg/op-equals (sg/motion-x-position) 50))
   (record-test "motion-goto-xy-y" (sg/op-equals (sg/motion-y-position) 30))

   ;; Test set-x
   (sg/motion-set-x -20)
   (record-test "motion-set-x" (sg/op-equals (sg/motion-x-position) -20))

   ;; Test set-y
   (sg/motion-set-y 15)
   (record-test "motion-set-y" (sg/op-equals (sg/motion-y-position) 15))

   ;; Test change-x-by
   (sg/motion-goto-xy 0 0)
   (sg/motion-change-x-by 10)
   (record-test "motion-change-x-by" (sg/op-equals (sg/motion-x-position) 10))

   ;; Test change-y-by
   (sg/motion-goto-xy 0 0)
   (sg/motion-change-y-by -5)
   (record-test "motion-change-y-by" (sg/op-equals (sg/motion-y-position) -5))

   ;; Test move-steps
   (sg/motion-goto-xy 0 0)
   (sg/motion-point-in-direction 90)
   (sg/motion-move-steps 10)
   (record-test "motion-move-steps" (sg/op-gt (sg/motion-x-position) 5))

   ;; Test turn-right
   (sg/motion-point-in-direction 90)
   (sg/motion-turn-right 45)
   (record-test "motion-turn-right" (sg/op-equals (sg/motion-direction) 135))

   ;; Test turn-left
   (sg/motion-point-in-direction 90)
   (sg/motion-turn-left 30)
   (record-test "motion-turn-left" (sg/op-equals (sg/motion-direction) 60))

   ;; Test point-in-direction
   (sg/motion-point-in-direction 0)
   (record-test "motion-point-in-direction" (sg/op-equals (sg/motion-direction) 0))))

(defn gen-data-tests
  "Generate test blocks for data (variables and lists)"
  [{:keys [test-var test-list]}]
  (sg/do-block
   ;; Variable tests
   (sg/data-set-variable test-var 42)
   (record-test "data-set-variable" (sg/op-equals test-var 42))

   (sg/data-change-variable test-var 8)
   (record-test "data-change-variable" (sg/op-equals test-var 50))

   ;; List tests - setup
   (sg/data-delete-all-list test-list)

   ;; Test add-to-list
   (sg/data-add-to-list test-list "first")
   (sg/data-add-to-list test-list "second")
   (sg/data-add-to-list test-list "third")
   (record-test "data-add-to-list-length" (sg/op-equals (sg/data-length-of-list test-list) 3))

   ;; Test item-of-list
   (record-test "data-item-of-list" (sg/op-equals (sg/data-item-of-list test-list 1) "first"))

   ;; Test replace-list-item
   (sg/data-replace-list-item test-list 2 "SECOND")
   (record-test "data-replace-list-item" (sg/op-equals (sg/data-item-of-list test-list 2) "SECOND"))

   ;; Test list-contains
   (record-test "op-contains?-true" (sg/op-contains? test-list "third"))
   (record-test "op-contains?-false" (sg/op-not (sg/op-contains? test-list "fourth")))

   ;; Test item-num-of-list
   (record-test "data-item-num-of-list" (sg/op-equals (sg/data-item-num-of-list test-list "third") 3))

   ;; Test insert-at-list
   (sg/data-insert-at-list test-list 1 "zero")
   (record-test "data-insert-at-list" (sg/op-equals (sg/data-item-of-list test-list 1) "zero"))
   (record-test "data-insert-at-list-length" (sg/op-equals (sg/data-length-of-list test-list) 4))

   ;; Test delete-from-list
   (sg/data-delete-from-list test-list 1)
   (record-test "data-delete-from-list-length" (sg/op-equals (sg/data-length-of-list test-list) 3))

   ;; Test delete-all-list
   (sg/data-delete-all-list test-list)
   (record-test "data-delete-all-list" (sg/op-equals (sg/data-length-of-list test-list) 0))))

(defn gen-control-tests
  "Generate test blocks for control flow"
  [{:keys [counter temp]}]
  (sg/do-block
   ;; Test repeat
   (sg/data-set-variable counter 0)
   (sg/control-repeat 5
                      (sg/data-change-variable counter 1))
   (record-test "control-repeat" (sg/op-equals counter 5))

   ;; Test if (true branch)
   (sg/data-set-variable temp 0)
   (sg/control-if true
                  (sg/data-set-variable temp 1))
   (record-test "control-if-true" (sg/op-equals temp 1))

   ;; Test if (false branch - should not execute)
   (sg/data-set-variable temp 0)
   (sg/control-if false
                  (sg/data-set-variable temp 1))
   (record-test "control-if-false" (sg/op-equals temp 0))

   ;; Test if-else (true branch)
   (sg/data-set-variable temp 0)
   (sg/control-if-else true
                       (sg/data-set-variable temp 1)
                       (sg/data-set-variable temp 2))
   (record-test "control-if-else-true" (sg/op-equals temp 1))

   ;; Test if-else (false branch)
   (sg/data-set-variable temp 0)
   (sg/control-if-else false
                       (sg/data-set-variable temp 1)
                       (sg/data-set-variable temp 2))
   (record-test "control-if-else-false" (sg/op-equals temp 2))

   ;; Test repeat-until
   (sg/data-set-variable counter 0)
   (sg/control-repeat-until (sg/op-equals counter 3)
                            (sg/data-change-variable counter 1))
   (record-test "control-repeat-until" (sg/op-equals counter 3))

   ;; Test wait (verify timer advances)
   (sg/sensing-reset-timer)
   (sg/control-wait 0.1)
   (record-test "control-wait" (sg/op-gt (sg/sensing-timer) 0.05))))

(defn gen-sensing-tests
  "Generate test blocks for sensing category"
  [_]
  (sg/do-block
   ;; Test timer
   (sg/sensing-reset-timer)
   (sg/control-wait 0.05)
   (record-test "sensing-timer" (sg/op-gt (sg/sensing-timer) 0))

   ;; Test mouse position (just verify they return numbers, can't control mouse)
   (record-test "sensing-mouse-x" (sg/op-or
                                   (sg/op-gt (sg/sensing-mouse-x) -300)
                                   (sg/op-equals (sg/sensing-mouse-x) -300)))

   (record-test "sensing-mouse-y" (sg/op-or
                                   (sg/op-gt (sg/sensing-mouse-y) -300)
                                   (sg/op-equals (sg/sensing-mouse-y) -300)))

   ;; Test current (just verify it returns something)
   (record-test "sensing-current-year" (sg/op-gt (sg/sensing-current "year") 2020))
   (record-test "sensing-current-month" (sg/op-and
                                         (sg/op-gt (sg/sensing-current "month") 0)
                                         (sg/op-lt (sg/sensing-current "month") 13)))

   ;; Test days since 2000
   (record-test "sensing-days-since-2000" (sg/op-gt (sg/sensing-days-since-2000) 9000))))

(defn gen-looks-tests
  "Generate test blocks for looks category"
  [_]
  (sg/do-block
   ;; Test show/hide
   (sg/looks-show)
   (sg/looks-hide)
   (sg/looks-show)

   ;; Test size
   (sg/looks-set-size-to 100)
   (record-test "looks-set-size-to" (sg/op-equals (sg/looks-size) 100))

   (sg/looks-change-size-by 50)
   (record-test "looks-change-size-by" (sg/op-equals (sg/looks-size) 150))

   ;; Test effects
   (sg/looks-clear-effects)
   (sg/looks-change-effect-by :GHOST 25)
   (sg/looks-set-effect-to "GHOST" 0)
   (sg/looks-clear-effects)

   ;; Test say (visual only, can't verify)
   (sg/looks-say "Testing looks blocks")
   (sg/control-wait 0.1)
   (sg/looks-say "")))

(defn gen-sound-tests
  "Generate test blocks for sound category (can't verify audio, just execute)"
  [_]
  (sg/do-block
   ;; Test volume
   (sg/sound-set-volume-to 50)
   (record-test "sound-set-volume-to" (sg/op-equals (sg/sound-volume) 50))

   (sg/sound-change-volume-by 20)
   (record-test "sound-change-volume-by" (sg/op-equals (sg/sound-volume) 70))

   ;; Reset to reasonable volume
   (sg/sound-set-volume-to 100)))

(defn define-record-test-block
  "Define the custom 'record test' block"
  [{:keys [test-count passed-count failed-tests]}]
  (sg/do-block
   {:opcode "procedures_definition"
    :topLevel true
    :x 538
    :y 51
    :inputs {:custom_block {:opcode "procedures_prototype"
                            :shadow true
                            :mutation {:tagName "mutation"
                                       :children [] ;; ????
                                       :proccode "record test %s %b"
                                       :argumentids (str "[\"arg0\",\"arg1\"]")
                                       :argumentnames (str "[\"name\",\"passed\"]")
                                       :argumentdefaults (str "[\"nonsense\",\"false\"]")
                                       :warp "true"}
                            :inputs {:arg0 {:opcode "argument_reporter_string_number"
                                            :shadow true
                                            :fields {:VALUE ["name" nil]}}
                                     :arg1 {:opcode "argument_reporter_boolean"
                                            :shadow true
                                            :fields {:VALUE ["passed" nil]}}}}}}
   (sg/data-change-variable test-count 1)
   (sg/control-if-else
    {:opcode "argument_reporter_boolean"
     :fields {:VALUE ["passed" nil]}}
    (sg/data-change-variable passed-count 1)
    (sg/data-add-to-list failed-tests
                         {:opcode "argument_reporter_string_number"
                          :fields {:VALUE ["name" nil]}}))))

(defn generate-stage-backdrop []
  "<svg version=\"1.1\" width=\"480\" height=\"360\" viewBox=\"0 0 480 360\">
  <rect width=\"480\" height=\"360\" fill=\"#ffffff\"/>
</svg>")

(defn generate-button-costume [text]
  (str "<svg version=\"1.1\" width=\"80\" height=\"40\" viewBox=\"0 0 80 40\">
  <rect width=\"80\" height=\"40\" fill=\"#ff6b6b\" stroke=\"#000000\" stroke-width=\"2\"/>
  <text x=\"40\" y=\"25\" text-anchor=\"middle\" font-family=\"Arial\" font-size=\"16\" fill=\"#ffffff\">" text "</text>
</svg>"))

(defn generate-test-project
  "Generate a comprehensive test project for all block generators"
  []
  (binding [sg/*block-counter* 0]
    (let [{:keys [test-count passed-count failed-tests] :as ctx}
          , (merge (sg/make-variables {:test-count 0
                                       :passed-count 0
                                       :temp 0
                                       :test-var 0
                                       :counter 0})
                   (sg/make-lists {:test-list []
                                   :failed-tests []}))
          backdrop-data (sg/create-costume (generate-stage-backdrop) "backdrop1")
          test-button (sg/create-costume (generate-button-costume "Run Test") "runtest")]

      [{:target {:isStage true
                 :name "Stage"
                 :variables {}
                 :lists {}
                 :broadcasts {}
                 :blocks {}
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
        :assets [backdrop-data]}
       {:target
        {:isStage false
         :name "Test Runner"
         :variables (into {} (keep :stage-variable) (vals ctx))
         :lists (into {} (keep :stage-list) (vals ctx))
         :broadcasts {}
         :blocks (merge
                  ;; Custom block definition
                  (sg/top-level-block
                   (define-record-test-block ctx))

                  ;; Main test script
                  (sg/top-level-block
                   (sg/event-when-flag-clicked)

                   ;; Initialize
                   (sg/data-set-variable test-count 0)
                   (sg/data-set-variable passed-count 0)
                   (sg/data-delete-all-list failed-tests)
                   (sg/looks-say "Running tests...")

                   ;; Run all test groups sequentially
                   (gen-operator-tests ctx)
                   (gen-motion-tests ctx)
                   (gen-data-tests ctx)
                   (gen-control-tests ctx)
                   (gen-sensing-tests ctx)
                   (gen-looks-tests ctx)
                   (gen-sound-tests ctx)

                   ;; Display results
                   (sg/control-if (sg/op-equals test-count passed-count)
                                  (sg/looks-say
                                   (sg/op-join "ALL "
                                               (sg/op-join test-count " TESTS PASSED ✓"))))

                   (sg/control-if (sg/op-not (sg/op-equals test-count passed-count))
                                  (sg/do-block
                                   (sg/looks-say
                                    (sg/op-join "FAILED: "
                                                (sg/op-join (sg/op-- test-count passed-count)
                                                            (sg/op-join " of "
                                                                        (sg/op-join test-count " tests")))))
                                   (sg/data-show-list failed-tests)))))

         :costumes [(:costume test-button)]
         :sounds []
         :volume 100
         :layerOrder 1
         :visible true
         :x 0
         :y 0
         :size 100
         :direction 90
         :draggable false
         :rotationStyle "all around"}
        :assets [test-button]}
       (sg/monitor failed-tests {})])))

(defn -main []
  (sg/generate-sb3 "scratch-test.sb3" (generate-test-project))
  (println "Generated scratch-test.sb3"))

(-main)