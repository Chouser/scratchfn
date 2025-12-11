(ns us.chouser.gen-scratch-test
  (:require [us.chouser.gen-scratch :as sg]))

;; TODO
;; - rename all generator functions to be more pleasant.
;; - can a variable be set to a color? Then used as a pen color?
;; - think more clearly about the two places :scratch-literal is used
;; - combine pen-drawing and color-touch sensing

;; ============================================================================
;; Test Generator Implementation
;; ============================================================================

(def record-test-proc (sg/proc-handle "record test %s %b" [:name :passed]))

(defn record-test
  "Generate a call to the custom 'record test' block"
  [test-name passed-condition]
  (sg/call-proc record-test-proc test-name passed-condition))

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

   ;; Test effects (can't verify, just execute)
   (sg/sound-set-effect-to "PITCH" 100)
   (sg/sound-change-effect-by "PITCH" 50)
   (sg/sound-clear-effects)

   ;; Test play blocks (audio only, can't verify)
   ;; Note: These would fail if no sounds are loaded, but demonstrate the generators work
   ;; (sg/sound-play [1 [10 "0"]])
   ;; (sg/sound-play-until-done [1 [10 "0"]])
   (sg/sound-stop-all-sounds)

   ;; Reset to reasonable volume
   (sg/sound-set-volume-to 100)))

(defn gen-event-tests
  "Generate test blocks for event category (execute to verify they work)"
  [{:keys [test-broadcast temp]}]
  (sg/do-block
   ;; Test broadcast mechanism
   (sg/data-set-variable temp 0)
   (sg/event-broadcast test-broadcast)
   (sg/control-wait 0.1)
   (record-test "event-broadcast" (sg/op-equals temp 1))

   ;; Test broadcast-and-wait
   (sg/data-set-variable temp 0)
   (sg/event-broadcast-and-wait test-broadcast)
   (record-test "event-broadcast-and-wait" (sg/op-equals temp 1))))

(defn gen-pen-tests
  "Generate test blocks for pen extension"
  [_]
  (sg/do-block
   ;; Test pen blocks (visual only, can't verify)
   (sg/pen-clear)
   (sg/pen-pen-down)
   (sg/pen-set-color (sg/color "#ff0000"))
   (sg/pen-set-size-to 5)
   (sg/motion-move-steps 50)
   (sg/pen-change-size-by 2)
   (sg/pen-set-param-to :color 50)
   (sg/pen-change-param-by :brightness 10)
   (sg/pen-stamp)
   (sg/pen-pen-up)
   (sg/pen-clear)))

(defn gen-additional-operator-tests
  "Test additional operator functions not covered in initial tests"
  [{:keys [temp]}]
  (sg/do-block
   ;; Test mathop functions
   (record-test "op-mathop-floor" (sg/op-equals (sg/op-mathop "floor" 3.7) 3))
   (record-test "op-mathop-ceiling" (sg/op-equals (sg/op-mathop "ceiling" 3.2) 4))
   (record-test "op-mathop-sin" (sg/op-equals (sg/op-round (sg/op-mathop "sin" 90)) 1))
   (record-test "op-mathop-cos" (sg/op-equals (sg/op-round (sg/op-mathop "cos" 0)) 1))
   (record-test "op-mathop-tan" (sg/op-equals (sg/op-round (sg/op-mathop "tan" 45)) 1))
   (record-test "op-mathop-ln" (sg/op-gt (sg/op-mathop "ln" 10) 2))
   (record-test "op-mathop-log" (sg/op-equals (sg/op-mathop "log" 100) 2))
   (record-test "op-mathop-e^" (sg/op-gt (sg/op-mathop "e ^" 2) 7))
   (record-test "op-mathop-10^" (sg/op-equals (sg/op-mathop "10 ^" 3) 1000))))

(defn gen-additional-motion-tests
  "Test additional motion blocks"
  [_]
  (sg/do-block
   ;; Test glide-secs-to-xy
   (sg/motion-goto-xy 0 0)
   (sg/motion-glide-secs-to-xy 0.1 50 50)
   (record-test "motion-glide-secs-to-xy-x" (sg/op-gt (sg/motion-x-position) 40))
   (record-test "motion-glide-secs-to-xy-y" (sg/op-gt (sg/motion-y-position) 40))

   ;; Test motion-goto-random (just verify it executes)
   (sg/motion-goto-random)
   (record-test "motion-goto-random" (sg/op-or 
                                       (sg/op-gt (sg/motion-x-position) -300)
                                       (sg/op-equals (sg/motion-x-position) -300)))

   ;; Test motion-goto with mouse pointer
   (sg/motion-goto {:opcode "motion_goto_menu"
                    :fields {:TO ["_mouse_" nil]}})

   ;; Test motion-glide-to with random position
   (sg/motion-glide-to 0.1 {:opcode "motion_glideto_menu"
                             :fields {:TO ["_random_" nil]}})

   ;; Test motion-point-towards
   (sg/motion-point-towards {:opcode "motion_pointtowards_menu"
                             :fields {:TOWARDS ["_mouse_" nil]}})

   ;; Test motion-set-rotation-style
   (sg/motion-set-rotation-style "left-right")
   (sg/motion-set-rotation-style "don't rotate")
   (sg/motion-set-rotation-style "all around")))

(defn gen-additional-looks-tests
  "Test additional looks blocks not covered in initial tests"
  [_]
  (sg/do-block
   ;; Test say-for-secs and think-for-secs (visual only)
   (sg/looks-say-for-secs "Testing say" 0.1)
   (sg/looks-think-for-secs "Testing think" 0.1)
   (sg/looks-think "Thinking...")
   (sg/control-wait 0.05)
   (sg/looks-think "")

   ;; Test costume switching (visual only, can't easily verify)
   (sg/looks-next-costume)
   (record-test "looks-costume-number" (sg/op-gt (sg/looks-costume-number) 0))
   (record-test "looks-costume-name" (sg/op-gt (sg/op-length (sg/looks-costume-name)) 0))

   ;; Test backdrop (execute to verify)
   (sg/looks-next-backdrop)
   (record-test "looks-backdrop-number" (sg/op-gt (sg/looks-backdrop-number) 0))
   (record-test "looks-backdrop-name" (sg/op-gt (sg/op-length (sg/looks-backdrop-name)) 0))

   ;; Test layer ordering (execute to verify no errors)
   (sg/looks-go-to-layer "front")
   (sg/looks-go-layers "forward" 1)
   (sg/looks-go-layers "backward" 1)
   (sg/looks-go-to-layer "back")))

(defn gen-additional-control-tests
  "Test additional control blocks"
  [{:keys [counter temp]}]
  (sg/do-block
   ;; Test wait-until
   (sg/sensing-reset-timer)
   (sg/control-wait-until (sg/op-gt (sg/sensing-timer) 0.05))
   (record-test "control-wait-until" (sg/op-gt (sg/sensing-timer) 0.04))

   ;; Test for-each (note: this is experimental in Scratch)
   (sg/data-set-variable counter 0)
   (sg/control-for-each counter 3
                        (sg/data-change-variable temp 1))
   (record-test "control-for-each" (sg/op-gt temp 0))

   ;; Test control-forever with control-stop
   ;; This is tricky to test - we'll increment a counter in a forever loop
   ;; then stop it after a condition
   (sg/data-set-variable counter 0)
   #_(sg/control-forever
      (sg/do-block
       (sg/data-change-variable counter 1)
       (sg/control-if (sg/op-gt counter 5)
                      (sg/control-stop "this script"))))
   #_(sg/control-wait 0.1)
   #_(record-test "control-forever-stop" (sg/op-gt counter 5))))

(defn gen-additional-sensing-tests
  "Test additional sensing blocks"
  [{:keys [temp]}]
  (sg/do-block
   ;; Test sensing-of (check sprite properties)
   (record-test "sensing-of-x" (sg/op-or
                                (sg/op-gt (sg/sensing-of "x position" [1 [11 "_stage_" "_stage_"]]) -300)
                                (sg/op-equals (sg/sensing-of "x position" [1 [11 "_stage_" "_stage_"]]) -300)))

   ;; Test loudness (just verify it returns a number)
   (record-test "sensing-loudness" (sg/op-or
                                    (sg/op-gt (sg/sensing-loudness) -1)
                                    (sg/op-equals (sg/sensing-loudness) 0)))

   ;; Test username (just verify it returns something)
   (record-test "sensing-username" (sg/op-or
                                    (sg/op-gt (sg/op-length (sg/sensing-username)) 0)
                                    (sg/op-equals (sg/op-length (sg/sensing-username)) 0)))))

(defn gen-additional-data-tests
  "Test additional data blocks"
  [{:keys [test-var test-list]}]
  (sg/do-block
   ;; Test show/hide variable (visual only)
   (sg/data-show-variable test-var)
   (sg/data-hide-variable test-var)
   (sg/data-show-variable test-var)

   ;; Test show/hide list (visual only)
   (sg/data-show-list test-list)
   (sg/data-hide-list test-list)))

(defn gen-music-tests
  "Test music extension blocks (audio only, can't verify)"
  [_]
  (sg/do-block
   ;; Test music blocks (execute to verify no errors)
   (sg/music-set-tempo 120)
   (record-test "music-get-tempo" (sg/op-equals (sg/music-get-tempo) 120))
   (sg/music-change-tempo 20)
   (record-test "music-change-tempo" (sg/op-equals (sg/music-get-tempo) 140))

   ;; Test other music blocks (audio only)
   (sg/music-set-instrument [1 [10 "1"]])
   (sg/music-play-note-for-beats 60 0.1)
   (sg/music-play-drum-for-beats [1 [10 "1"]] 0.1)
   (sg/music-rest-for-beats 0.1)))

(defn gen-video-sensing-tests
  "Test video sensing extension blocks (camera required, just execute)"
  [_]
  (sg/do-block
   ;; Test video sensing blocks (visual only)
   (sg/video-toggle [1 [10 "off"]])
   (sg/video-set-transparency 50)))

(defn gen-text-to-speech-tests
  "Test text-to-speech extension blocks (audio only, just execute)"
  [_]
  (sg/do-block
   ;; Test TTS blocks (audio only)
   (sg/tts-set-voice "alto")
   (sg/tts-speak "Testing")))

(defn gen-clone-tests
  "Test clone-related control blocks"
  [{:keys [temp]}]
  (sg/do-block
   ;; Test create clone (visual only, hard to verify)
   (sg/data-set-variable temp 0)
   (sg/control-create-clone-of [1 [11 "_myself_" "_myself_"]])
   (sg/control-wait 0.1)
   ;; Note: Clone behavior would need to be set up separately
   ))

(defn gen-interaction-blocks
  "Test blocks that require user interaction (just execute)"
  [_]
  (sg/do-block
   ;; These blocks require user input, so we just execute them
   ;; to verify they're generated correctly
   ;; sensing-ask-and-wait would block execution, so skip it in automated tests
   ;; (sg/sensing-ask-and-wait "Test question")
   ;; (record-test "sensing-answer" (sg/op-gt (sg/op-length (sg/sensing-answer)) -1))

   ;; Test key-pressed (can't simulate, just execute)
   (sg/control-if (sg/sensing-key-pressed [1 [11 "space" "AuJ)y]$O1hb4+R,R2S=V"]])
                  (sg/looks-say "Space pressed"))

   ;; Test mouse-down (can't simulate, just execute)
   (sg/control-if (sg/sensing-mouse-down)
                  (sg/looks-say "Mouse down"))

   ;; Test set-drag-mode (execute only)
   (sg/sensing-set-drag-mode "draggable")
   (sg/sensing-set-drag-mode "not draggable")

   ;; Test touching blocks (can't easily verify without specific setup)
   (sg/control-if (sg/sensing-touching-object [1 [11 "_edge_" "_edge_"]])
                  (sg/looks-say "Touching edge"))
   (sg/control-if (sg/sensing-touching-color [1 [9 "#ff0000"]])
                  (sg/looks-say "Touching red"))
   (sg/control-if (sg/sensing-color-touching-color [1 [9 "#ff0000"]] [1 [9 "#0000ff"]])
                  (sg/looks-say "Red touching blue"))

   ;; Test distance-to (just verify it returns a number)
   (record-test "sensing-distance-to" (sg/op-or
                                       (sg/op-gt (sg/sensing-distance-to [1 [11 "_mouse_" "_mouse_"]]) -1)
                                       (sg/op-equals (sg/sensing-distance-to [1 [11 "_mouse_" "_mouse_"]]) 0)))))

(defn define-record-test-block
  "Define the custom 'record test' block"
  [{:keys [test-count passed-count failed-tests]}]
  (sg/define-proc record-test-proc
    {:x 538 :y 51}
    {:warp "true"}
    (fn [name passed]
      (sg/do-block
       (sg/data-change-variable test-count 1)
       (sg/control-if-else
        passed
        (sg/data-change-variable passed-count 1)
        (sg/data-add-to-list failed-tests name))))))

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
          test-button (sg/create-costume (generate-button-costume "Run Test") "runtest")
          operator-tests (sg/proc-handle "operator tests" [])]

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

                  (sg/top-level-block
                   (sg/define-proc operator-tests
                     {:x 538 :y 351}
                     {:warp "true"}
                     #(gen-operator-tests ctx)))

                  ;; Main test script
                  (sg/top-level-block
                   (sg/event-when-flag-clicked)

                   ;; Initialize
                   (sg/data-set-variable test-count 0)
                   (sg/data-set-variable passed-count 0)
                   (sg/data-delete-all-list failed-tests)
                   (sg/looks-say "Running tests...")

                   ;; Run all test groups sequentially
                   (sg/call-proc operator-tests)
                   (gen-motion-tests ctx)
                   (gen-data-tests ctx)
                   (gen-control-tests ctx)
                   (gen-sensing-tests ctx)
                   (gen-looks-tests ctx)
                   (gen-sound-tests ctx)

                   #_(gen-event-tests ctx)
                   (gen-pen-tests ctx)
                   (gen-additional-operator-tests ctx)
                   (gen-additional-motion-tests ctx)
                   (gen-additional-looks-tests ctx)
                   (gen-additional-control-tests ctx)
                   #_(gen-additional-sensing-tests ctx)
                   (gen-additional-data-tests ctx)
                   #_(gen-music-tests ctx)
                   #_(gen-video-sensing-tests ctx)
                   #_(gen-text-to-speech-tests ctx)
                   #_(gen-clone-tests ctx)
                   #_(gen-interaction-blocks ctx)

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
