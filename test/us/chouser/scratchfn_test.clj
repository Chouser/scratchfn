(ns us.chouser.scratchfn-test
  (:require [us.chouser.scratchfn :as $]))

;; TODO
;; - rename all generator functions to be more pleasant.
;; - can a variable be set to a color? Then used as a pen color?
;; - think more clearly about the two places :scratch-literal is used
;; - combine pen-drawing and color-touch sensing
;; - implement and test generated wav sounds

;; ============================================================================
;; Test Generator Implementation
;; ============================================================================

(def record-test-proc ($/proc-handle "record test %s %b" [:name :passed]))

(defn record-test
  "Generate a call to the custom 'record test' block"
  [test-name passed-condition]
  ($/call-proc record-test-proc test-name passed-condition))

(defn gen-operator-tests
  "Generate test blocks for operator category"
  [{:keys [temp]}]
  ($/do-block
   (record-test "op-+" ($/op-equals ($/op-+ 2 3) 5))
   (record-test "op--" ($/op-equals ($/op-- 10 3) 7))
   (record-test "op-*" ($/op-equals ($/op-* 4 5) 20))
   (record-test "op-divide" ($/op-equals ($/op-divide 20 4) 5))
   (record-test "op-join" ($/op-equals ($/op-join "Hello" "World") "HelloWorld"))
   (record-test "op-length" ($/op-equals ($/op-length "test") 4))
   (record-test "op-letter-of" ($/op-equals ($/op-letter-of 1 "ABC") "A"))
   (record-test "op-contains-true" ($/op-contains "hello" "ell"))
   (record-test "op-contains-false" ($/op-not ($/op-contains "hello" "xyz")))
   (record-test "op-mod" ($/op-equals ($/op-mod 10 3) 1))
   (record-test "op-round" ($/op-equals ($/op-round 3.7) 4))
   (record-test "op-mathop-abs" ($/op-equals ($/op-mathop "abs" -5) 5))
   (record-test "op-mathop-sqrt" ($/op-equals ($/op-mathop "sqrt" 9) 3))
   (record-test "op-equals-true" ($/op-equals 5 5))
   (record-test "op-equals-false" ($/op-not ($/op-equals 5 6)))
   (record-test "op-gt-true" ($/op-gt 10 5))
   (record-test "op-gt-false" ($/op-not ($/op-gt 5 10)))
   (record-test "op-lt-true" ($/op-lt 5 10))
   (record-test "op-lt-false" ($/op-not ($/op-lt 10 5)))
   (record-test "op-and-true" ($/op-and true true))

   (record-test "op-and-false" ($/op-not ($/op-and true false)))
   (record-test "op-or-true" ($/op-or true false))
   (record-test "op-or-false" ($/op-not ($/op-or false false)))
   (record-test "op-not-true" ($/op-not false))
   (record-test "op-not-false" ($/op-not ($/op-not true)))

   ;; Test random (just check it's in range)
   ($/data-set-variable temp ($/op-random 1 10))
   (record-test "op-random-min" ($/op-or ($/op-gt temp 0) ($/op-equals temp 1)))
   (record-test "op-random-max" ($/op-or ($/op-lt temp 11) ($/op-equals temp 10)))))

(defn gen-motion-tests
  "Generate test blocks for motion category"
  [_]
  ($/do-block
   ;; Reset position
   ($/motion-goto-xy 0 0)
   ($/motion-point-in-direction 90)

   ;; Test goto-xy
   ($/motion-goto-xy 50 30)
   (record-test "motion-goto-xy-x" ($/op-equals ($/motion-x-position) 50))
   (record-test "motion-goto-xy-y" ($/op-equals ($/motion-y-position) 30))

   ;; Test set-x
   ($/motion-set-x -20)
   (record-test "motion-set-x" ($/op-equals ($/motion-x-position) -20))

   ;; Test set-y
   ($/motion-set-y 15)
   (record-test "motion-set-y" ($/op-equals ($/motion-y-position) 15))

   ;; Test change-x-by
   ($/motion-goto-xy 0 0)
   ($/motion-change-x-by 10)
   (record-test "motion-change-x-by" ($/op-equals ($/motion-x-position) 10))

   ;; Test change-y-by
   ($/motion-goto-xy 0 0)
   ($/motion-change-y-by -5)
   (record-test "motion-change-y-by" ($/op-equals ($/motion-y-position) -5))

   ;; Test move-steps
   ($/motion-goto-xy 0 0)
   ($/motion-point-in-direction 90)
   ($/motion-move-steps 10)
   (record-test "motion-move-steps" ($/op-gt ($/motion-x-position) 5))

   ;; Test turn-right
   ($/motion-point-in-direction 90)
   ($/motion-turn-right 45)
   (record-test "motion-turn-right" ($/op-equals ($/motion-direction) 135))

   ;; Test turn-left
   ($/motion-point-in-direction 90)
   ($/motion-turn-left 30)
   (record-test "motion-turn-left" ($/op-equals ($/motion-direction) 60))

   ;; Test point-in-direction
   ($/motion-point-in-direction 0)
   (record-test "motion-point-in-direction" ($/op-equals ($/motion-direction) 0))))

(defn gen-data-tests
  "Generate test blocks for data (variables and lists)"
  [{:keys [test-var test-list]}]
  ($/do-block
   ;; Variable tests
   ($/data-set-variable test-var 42)
   (record-test "data-set-variable" ($/op-equals test-var 42))

   ($/data-change-variable test-var 8)
   (record-test "data-change-variable" ($/op-equals test-var 50))

   ;; List tests - setup
   ($/data-delete-all-list test-list)

   ;; Test add-to-list
   ($/data-add-to-list test-list "first")
   ($/data-add-to-list test-list "second")
   ($/data-add-to-list test-list "third")
   (record-test "data-add-to-list-length" ($/op-equals ($/data-length-of-list test-list) 3))

   ;; Test item-of-list
   (record-test "data-item-of-list" ($/op-equals ($/data-item-of-list test-list 1) "first"))

   ;; Test replace-list-item
   ($/data-replace-list-item test-list 2 "SECOND")
   (record-test "data-replace-list-item" ($/op-equals ($/data-item-of-list test-list 2) "SECOND"))

   ;; Test list-contains
   (record-test "op-contains?-true" ($/op-contains? test-list "third"))
   (record-test "op-contains?-false" ($/op-not ($/op-contains? test-list "fourth")))

   ;; Test item-num-of-list
   (record-test "data-item-num-of-list" ($/op-equals ($/data-item-num-of-list test-list "third") 3))

   ;; Test insert-at-list
   ($/data-insert-at-list test-list 1 "zero")
   (record-test "data-insert-at-list" ($/op-equals ($/data-item-of-list test-list 1) "zero"))
   (record-test "data-insert-at-list-length" ($/op-equals ($/data-length-of-list test-list) 4))

   ;; Test delete-from-list
   ($/data-delete-from-list test-list 1)
   (record-test "data-delete-from-list-length" ($/op-equals ($/data-length-of-list test-list) 3))

   ;; Test delete-all-list
   ($/data-delete-all-list test-list)
   (record-test "data-delete-all-list" ($/op-equals ($/data-length-of-list test-list) 0))))

(defn gen-control-tests
  "Generate test blocks for control flow"
  [{:keys [counter temp]}]
  ($/do-block
   ;; Test repeat
   ($/data-set-variable counter 0)
   ($/control-repeat 5
                      ($/data-change-variable counter 1))
   (record-test "control-repeat" ($/op-equals counter 5))

   ;; Test if (true branch)
   ($/data-set-variable temp 0)
   ($/control-if true
                  ($/data-set-variable temp 1))
   (record-test "control-if-true" ($/op-equals temp 1))

   ;; Test if (false branch - should not execute)
   ($/data-set-variable temp 0)
   ($/control-if false
                  ($/data-set-variable temp 1))
   (record-test "control-if-false" ($/op-equals temp 0))

   ;; Test if-else (true branch)
   ($/data-set-variable temp 0)
   ($/control-if-else true
                       ($/data-set-variable temp 1)
                       ($/data-set-variable temp 2))
   (record-test "control-if-else-true" ($/op-equals temp 1))

   ;; Test if-else (false branch)
   ($/data-set-variable temp 0)
   ($/control-if-else false
                       ($/data-set-variable temp 1)
                       ($/data-set-variable temp 2))
   (record-test "control-if-else-false" ($/op-equals temp 2))

   ;; Test repeat-until
   ($/data-set-variable counter 0)
   ($/control-repeat-until ($/op-equals counter 3)
                            ($/data-change-variable counter 1))
   (record-test "control-repeat-until" ($/op-equals counter 3))

   ;; Test wait (verify timer advances)
   ($/sensing-reset-timer)
   ($/control-wait 0.1)
   (record-test "control-wait" ($/op-gt ($/sensing-timer) 0.05))))

(defn gen-sensing-tests
  "Generate test blocks for sensing category"
  [_]
  ($/do-block
   ;; Test timer
   ($/sensing-reset-timer)
   ($/control-wait 0.05)
   (record-test "sensing-timer" ($/op-gt ($/sensing-timer) 0))

   ;; Test mouse position (just verify they return numbers, can't control mouse)
   (record-test "sensing-mouse-x" ($/op-or
                                   ($/op-gt ($/sensing-mouse-x) -300)
                                   ($/op-equals ($/sensing-mouse-x) -300)))

   (record-test "sensing-mouse-y" ($/op-or
                                   ($/op-gt ($/sensing-mouse-y) -300)
                                   ($/op-equals ($/sensing-mouse-y) -300)))

   ;; Test current (just verify it returns something)
   (record-test "sensing-current-year" ($/op-gt ($/sensing-current "year") 2020))
   (record-test "sensing-current-month" ($/op-and
                                         ($/op-gt ($/sensing-current "month") 0)
                                         ($/op-lt ($/sensing-current "month") 13)))

   ;; Test days since 2000
   (record-test "sensing-days-since-2000" ($/op-gt ($/sensing-days-since-2000) 9000))))

(defn gen-looks-tests
  "Generate test blocks for looks category"
  [_]
  ($/do-block
   ;; Test show/hide
   ($/looks-show)
   ($/looks-hide)
   ($/looks-show)

   ;; Test size
   ($/looks-set-size-to 100)
   (record-test "looks-set-size-to" ($/op-equals ($/looks-size) 100))

   ($/looks-change-size-by 50)
   (record-test "looks-change-size-by" ($/op-equals ($/looks-size) 150))

   ;; Test effects
   ($/looks-clear-effects)
   ($/looks-change-effect-by :GHOST 25)
   ($/looks-set-effect-to "GHOST" 0)
   ($/looks-clear-effects)

   ;; Test say (visual only, can't verify)
   ($/looks-say "Testing looks blocks")
   ($/control-wait 0.1)
   ($/looks-say "")))

(defn gen-sound-tests
  "Generate test blocks for sound category (can't verify audio, just execute)"
  [_]
  ($/do-block
   ;; Test volume
   ($/sound-set-volume-to 50)
   (record-test "sound-set-volume-to" ($/op-equals ($/sound-volume) 50))

   ($/sound-change-volume-by 20)
   (record-test "sound-change-volume-by" ($/op-equals ($/sound-volume) 70))

   ;; Test effects (can't verify, just execute)
   ($/sound-set-effect-to "PITCH" 100)
   ($/sound-change-effect-by "PITCH" 50)
   ($/sound-clear-effects)

   ;; Test play blocks (audio only, can't verify)
   ;; Note: These would fail if no sounds are loaded, but demonstrate the generators work
   ;; ($/sound-play [1 [10 "0"]])
   ;; ($/sound-play-until-done [1 [10 "0"]])
   ($/sound-stop-all-sounds)

   ;; Reset to reasonable volume
   ($/sound-set-volume-to 100)))

(defn gen-event-tests
  "Generate test blocks for event category (execute to verify they work)"
  [{:keys [test-broadcast temp]}]
  ($/do-block
   ;; Test broadcast mechanism
   ($/data-set-variable temp 0)
   ($/event-broadcast test-broadcast)
   ($/control-wait 0.1)
   (record-test "event-broadcast" ($/op-equals temp 1))

   ;; Test broadcast-and-wait
   ($/data-set-variable temp 0)
   ($/event-broadcast-and-wait test-broadcast)
   (record-test "event-broadcast-and-wait" ($/op-equals temp 1))))

(defn gen-pen-tests
  "Generate test blocks for pen extension"
  [_]
  ($/do-block
   ;; Test pen blocks (visual only, can't verify)
   ($/pen-clear)
   ($/pen-pen-down)
   ($/pen-set-color ($/color "#ff0000"))
   ($/pen-set-size-to 5)
   ($/motion-move-steps 50)
   ($/pen-change-size-by 2)
   ($/pen-set-param-to :color 50)
   ($/pen-change-param-by :brightness 10)
   ($/pen-stamp)
   ($/pen-pen-up)
   ($/pen-clear)))

(defn gen-additional-operator-tests
  "Test additional operator functions not covered in initial tests"
  [{:keys [temp]}]
  ($/do-block
   ;; Test mathop functions
   (record-test "op-mathop-floor" ($/op-equals ($/op-mathop "floor" 3.7) 3))
   (record-test "op-mathop-ceiling" ($/op-equals ($/op-mathop "ceiling" 3.2) 4))
   (record-test "op-mathop-sin" ($/op-equals ($/op-round ($/op-mathop "sin" 90)) 1))
   (record-test "op-mathop-cos" ($/op-equals ($/op-round ($/op-mathop "cos" 0)) 1))
   (record-test "op-mathop-tan" ($/op-equals ($/op-round ($/op-mathop "tan" 45)) 1))
   (record-test "op-mathop-ln" ($/op-gt ($/op-mathop "ln" 10) 2))
   (record-test "op-mathop-log" ($/op-equals ($/op-mathop "log" 100) 2))
   (record-test "op-mathop-e^" ($/op-gt ($/op-mathop "e ^" 2) 7))
   (record-test "op-mathop-10^" ($/op-equals ($/op-mathop "10 ^" 3) 1000))))

(defn gen-additional-motion-tests
  "Test additional motion blocks"
  [_]
  ($/do-block
   ;; Test glide-secs-to-xy
   ($/motion-goto-xy 0 0)
   ($/motion-glide-secs-to-xy 0.1 50 50)
   (record-test "motion-glide-secs-to-xy-x" ($/op-gt ($/motion-x-position) 40))
   (record-test "motion-glide-secs-to-xy-y" ($/op-gt ($/motion-y-position) 40))

   ;; Test motion-goto-random (just verify it executes)
   ($/motion-goto-random)
   (record-test "motion-goto-random" ($/op-or 
                                       ($/op-gt ($/motion-x-position) -300)
                                       ($/op-equals ($/motion-x-position) -300)))

   ;; Test motion-goto with mouse pointer
   ($/motion-goto {:opcode "motion_goto_menu"
                    :fields {:TO ["_mouse_" nil]}})

   ;; Test motion-glide-to with random position
   ($/motion-glide-to 0.1 {:opcode "motion_glideto_menu"
                             :fields {:TO ["_random_" nil]}})

   ;; Test motion-point-towards
   ($/motion-point-towards {:opcode "motion_pointtowards_menu"
                             :fields {:TOWARDS ["_mouse_" nil]}})

   ;; Test motion-set-rotation-style
   ($/motion-set-rotation-style "left-right")
   ($/motion-set-rotation-style "don't rotate")
   ($/motion-set-rotation-style "all around")))

(defn gen-additional-looks-tests
  "Test additional looks blocks not covered in initial tests"
  [_]
  ($/do-block
   ;; Test say-for-secs and think-for-secs (visual only)
   ($/looks-say-for-secs "Testing say" 0.1)
   ($/looks-think-for-secs "Testing think" 0.1)
   ($/looks-think "Thinking...")
   ($/control-wait 0.05)
   ($/looks-think "")

   ;; Test costume switching (visual only, can't easily verify)
   ($/looks-next-costume)
   (record-test "looks-costume-number" ($/op-gt ($/looks-costume-number) 0))
   (record-test "looks-costume-name" ($/op-gt ($/op-length ($/looks-costume-name)) 0))

   ;; Test backdrop (execute to verify)
   ($/looks-next-backdrop)
   (record-test "looks-backdrop-number" ($/op-gt ($/looks-backdrop-number) 0))
   (record-test "looks-backdrop-name" ($/op-gt ($/op-length ($/looks-backdrop-name)) 0))

   ;; Test layer ordering (execute to verify no errors)
   ($/looks-go-to-layer "front")
   ($/looks-go-layers "forward" 1)
   ($/looks-go-layers "backward" 1)
   ($/looks-go-to-layer "back")))

(defn gen-additional-control-tests
  "Test additional control blocks"
  [{:keys [counter temp]}]
  ($/do-block
   ;; Test wait-until
   ($/sensing-reset-timer)
   ($/control-wait-until ($/op-gt ($/sensing-timer) 0.05))
   (record-test "control-wait-until" ($/op-gt ($/sensing-timer) 0.04))

   ;; Test for-each (note: this is experimental in Scratch)
   ($/data-set-variable counter 0)
   ($/control-for-each counter 3
                       ($/data-change-variable temp 1))
   (record-test "control-for-each" ($/op-gt temp 0))

   ;; Test control-forever with control-stop
   ;; This is tricky to test - we'll increment a counter in a forever loop
   ;; then stop it after a condition
   ($/data-set-variable counter 0)
   ($/control-forever
    ($/do-block
     ($/data-change-variable counter 1)
     ($/control-if ($/op-gt counter 5)
                   ($/do-block
                    (record-test "control-forever" ($/op-equals 1 1))
                    ($/control-stop "this script")))))))

(defn gen-additional-sensing-tests
  "Test additional sensing blocks"
  [{:keys [temp]}]
  ($/do-block
   ;; Test sensing-of (check sprite properties)
   (record-test "sensing-of-x" ($/op-or
                                ($/op-gt ($/sensing-of "x position" [1 [11 "_stage_" "_stage_"]]) -300)
                                ($/op-equals ($/sensing-of "x position" [1 [11 "_stage_" "_stage_"]]) -300)))

   ;; Test loudness (just verify it returns a number)
   (record-test "sensing-loudness" ($/op-or
                                    ($/op-gt ($/sensing-loudness) -1)
                                    ($/op-equals ($/sensing-loudness) 0)))

   ;; Test username (just verify it returns something)
   (record-test "sensing-username" ($/op-or
                                    ($/op-gt ($/op-length ($/sensing-username)) 0)
                                    ($/op-equals ($/op-length ($/sensing-username)) 0)))))

(defn gen-additional-data-tests
  "Test additional data blocks"
  [{:keys [test-var test-list]}]
  ($/do-block
   ;; Test show/hide variable (visual only)
   ($/data-show-variable test-var)
   ($/data-hide-variable test-var)
   ($/data-show-variable test-var)

   ;; Test show/hide list (visual only)
   ($/data-show-list test-list)
   ($/data-hide-list test-list)))

(defn gen-music-tests
  "Test music extension blocks (audio only, can't verify)"
  [_]
  ($/do-block
   ;; Test music blocks (execute to verify no errors)
   ($/music-set-tempo 120)
   (record-test "music-get-tempo" ($/op-equals ($/music-get-tempo) 120))
   ($/music-change-tempo 20)
   (record-test "music-change-tempo" ($/op-equals ($/music-get-tempo) 140))

   ;; Test other music blocks (audio only)
   ($/music-set-instrument ($/instrument :trombone))
   ($/music-play-note-for-beats 60 0.1)
   ($/music-play-drum-for-beats ($/drum :bongo) 0.1)
   ($/music-rest-for-beats 0.1)))

(defn gen-video-sensing-tests
  "Test video sensing extension blocks (camera required, just execute)"
  [_]
  ($/do-block
   ;; Test video sensing blocks (visual only)
   ($/video-toggle [1 [10 "off"]])
   ($/video-set-transparency 50)))

(defn gen-text-to-speech-tests
  "Test text-to-speech extension blocks (audio only, just execute)"
  [_]
  ($/do-block
   ;; Test TTS blocks (audio only)
   ($/tts-set-voice "alto")
   ($/tts-speak "Testing")))

(defn gen-clone-tests
  "Test clone-related control blocks"
  [{:keys [temp]}]
  ($/do-block
   ;; Test create clone (visual only, hard to verify)
   ($/data-set-variable temp 0)
   ($/control-create-clone-of [1 [11 "_myself_" "_myself_"]])
   ($/control-wait 0.1)
   ;; Note: Clone behavior would need to be set up separately
   ))

(defn gen-interaction-blocks
  "Test blocks that require user interaction (just execute)"
  [_]
  ($/do-block
   ;; These blocks require user input, so we just execute them
   ;; to verify they're generated correctly
   ;; sensing-ask-and-wait would block execution, so skip it in automated tests
   ;; ($/sensing-ask-and-wait "Test question")
   ;; (record-test "sensing-answer" ($/op-gt ($/op-length ($/sensing-answer)) -1))

   ;; Test key-pressed (can't simulate, just execute)
   ($/control-if ($/sensing-key-pressed [1 [11 "space" "AuJ)y]$O1hb4+R,R2S=V"]])
                  ($/looks-say "Space pressed"))

   ;; Test mouse-down (can't simulate, just execute)
   ($/control-if ($/sensing-mouse-down)
                  ($/looks-say "Mouse down"))

   ;; Test set-drag-mode (execute only)
   ($/sensing-set-drag-mode "draggable")
   ($/sensing-set-drag-mode "not draggable")

   ;; Test touching blocks (can't easily verify without specific setup)
   ($/control-if ($/sensing-touching-object [1 [11 "_edge_" "_edge_"]])
                  ($/looks-say "Touching edge"))
   ($/control-if ($/sensing-touching-color [1 [9 "#ff0000"]])
                  ($/looks-say "Touching red"))
   ($/control-if ($/sensing-color-touching-color [1 [9 "#ff0000"]] [1 [9 "#0000ff"]])
                  ($/looks-say "Red touching blue"))

   ;; Test distance-to (just verify it returns a number)
   (record-test "sensing-distance-to" ($/op-or
                                       ($/op-gt ($/sensing-distance-to [1 [11 "_mouse_" "_mouse_"]]) -1)
                                       ($/op-equals ($/sensing-distance-to [1 [11 "_mouse_" "_mouse_"]]) 0)))))
(def test-vars [#'gen-operator-tests
                #'gen-motion-tests
                #'gen-data-tests
                #'gen-control-tests
                #'gen-sensing-tests
                #'gen-looks-tests
                #'gen-sound-tests

                #_gen-event-tests
                #'gen-pen-tests
                #'gen-additional-operator-tests
                #'gen-additional-motion-tests
                #'gen-additional-looks-tests
                #'gen-additional-control-tests
                #_gen-additional-sensing-tests
                #'gen-additional-data-tests
                #'gen-music-tests
                #_gen-video-sensing-tests
                #_gen-text-to-speech-tests
                #_gen-clone-tests
                #_gen-interaction-blocks])

(defn define-record-test-block
  "Define the custom 'record test' block"
  [{:keys [test-count passed-count failed-tests]}]
  ($/define-proc record-test-proc
    {:x 538 :y 51}
    {:warp "true"}
    (fn [name passed]
      ($/do-block
       ($/data-change-variable test-count 1)
       ($/control-if-else
        passed
        ($/data-change-variable passed-count 1)
        ($/data-add-to-list failed-tests name))))))

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
  (binding [$/*block-counter* 0]
    (let [{:keys [test-count passed-count failed-tests] :as ctx}
          , (merge ($/make-variables {:test-count 0
                                      :passed-count 0
                                      :temp 0
                                      :test-var 0
                                      :counter 0})
                   ($/make-lists {:test-list []
                                  :failed-tests []}))
          backdrop-data ($/create-costume (generate-stage-backdrop) "backdrop1")
          test-button ($/create-costume (generate-button-costume "Run Test") "runtest")
          test-handles (->> test-vars (map #(-> % meta :name str
                                                (->> (re-find #"^gen-(.*)"))
                                                second
                                                ($/proc-handle []))))]

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
                  ($/top-level-block
                   (define-record-test-block ctx))

                  (apply merge
                         (map (fn [handle v n]
                                ($/top-level-block
                                 ($/define-proc handle
                                   {:x (+ 538 (* n 300)) :y 451}
                                   {:warp "true"}
                                   #(v ctx))))
                              test-handles test-vars (range)))

                  ;; Main test script
                  ($/top-level-block
                   ($/event-when-flag-clicked)

                   ;; Initialize
                   ($/data-set-variable test-count 0)
                   ($/data-set-variable passed-count 0)
                   ($/data-delete-all-list failed-tests)
                   ($/looks-say "Running tests...")

                   ;; Run all test groups sequentially
                   (apply $/do-block (map $/call-proc test-handles))

                   ;; Display results
                   ($/control-if ($/op-equals test-count passed-count)
                                 ($/looks-say
                                  ($/op-join "ALL "
                                             ($/op-join test-count " TESTS PASSED ✓"))))

                   ($/control-if ($/op-not ($/op-equals test-count passed-count))
                                 ($/do-block
                                  ($/looks-say
                                   ($/op-join "FAILED: "
                                              ($/op-join ($/op-- test-count passed-count)
                                                         ($/op-join " of "
                                                                    ($/op-join test-count " tests")))))
                                  ($/data-show-list failed-tests)))))

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
       ($/monitor failed-tests {})])))

(defn -main []
  ($/generate-sb3 "scratch-test.sb3" (generate-test-project))
  (println "Generated scratch-test.sb3"))

(-main)
