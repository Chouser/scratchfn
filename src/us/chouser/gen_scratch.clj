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

(defn as-variable [x]
  (or (:as-variable x)
      (throw (ex-info "Expected variable" {:variable x}))))

(defn data-variable [VARIABLE]
  {:opcode "data_variable"
   :fields {:VARIABLE (as-variable VARIABLE)}})

(defn as-input [x]
  (cond (string? x) (text-input x)
        (number? x) (number-input x)
        (boolean? x) (text-input (str x))
        (:as-variable x) (data-variable x)
        (:opcode x) x
        :else (throw (ex-info (str "Unknown input object " (pr-str x)) {:x x}))))

(declare op-equals)

(defn as-boolean-input [x]
  (cond (boolean? x) (if x
                       (op-equals 1 1)
                       (op-equals 0 2))
        (:as-variable x) (data-variable x)
        (:opcode x) x
        (or (string? x) (number? x)) (throw (ex-info (str "Non-boolean input " (pr-str x)) {:x x}))
        :else (throw (ex-info (str "Unknown input object " (pr-str x)) {:x x}))))

(defn as-list [x]
  (or (:as-list x) (throw (ex-info "Expected list" {:list x}))))

;; ============================================================================
;; Opcode Constructor Functions
;; ============================================================================

(defn op-contains? [LIST ITEM]
  {:opcode "data_listcontainsitem"
   :fields {:LIST (as-list LIST)}
   :inputs {:ITEM (as-input ITEM)}})

(defn op-not [OPERAND]
  {:opcode "operator_not"
   :inputs {:OPERAND (as-boolean-input OPERAND)}})

(defn op-and [OPERAND1 OPERAND2]
  {:opcode "operator_and"
   :inputs {:OPERAND1 (as-boolean-input OPERAND1)
            :OPERAND2 (as-boolean-input OPERAND2)}})

(defn op-or [OPERAND1 OPERAND2]
  {:opcode "operator_or"
   :inputs {:OPERAND1 (as-boolean-input OPERAND1)
            :OPERAND2 (as-boolean-input OPERAND2)}})

(defn op-equals [OPERAND1 OPERAND2]
  {:opcode "operator_equals"
   :inputs {:OPERAND1 (as-input OPERAND1)
            :OPERAND2 (as-input OPERAND2)}})

(defn op-gt [OPERAND1 OPERAND2]
  {:opcode "operator_gt"
   :inputs {:OPERAND1 (as-input OPERAND1)
            :OPERAND2 (as-input OPERAND2)}})

(defn op-+ [NUM1 NUM2]
  {:opcode "operator_add"
   :inputs {:NUM1 (as-input NUM1)
            :NUM2 (as-input NUM2)}})

(defn op-- [NUM1 NUM2]
  {:opcode "operator_subtract"
   :inputs {:NUM1 (as-input NUM1)
            :NUM2 (as-input NUM2)}})

(defn op-* [NUM1 NUM2]
  {:opcode "operator_multiply"
   :inputs {:NUM1 (as-input NUM1)
            :NUM2 (as-input NUM2)}})

(defn op-divide [NUM1 NUM2]
  {:opcode "operator_divide"
   :inputs {:NUM1 (as-input NUM1)
            :NUM2 (as-input NUM2)}})

(defn data-set-variable [VARIABLE VALUE]
  {:opcode "data_setvariableto"
   :fields {:VARIABLE (as-variable VARIABLE)}
   :inputs {:VALUE (as-input VALUE)}})

(defn data-change-variable [VARIABLE VALUE]
  {:opcode "data_changevariableby"
   :fields {:VARIABLE (as-variable VARIABLE)}
   :inputs {:VALUE (as-input VALUE)}})

(defn data-add-to-list [LIST ITEM]
  {:opcode "data_addtolist"
   :fields {:LIST (as-list LIST)}
   :inputs {:ITEM (as-input ITEM)}})

(defn data-replace-list-item [LIST INDEX ITEM]
  {:opcode "data_replaceitemoflist"
   :fields {:LIST (as-list LIST)}
   :inputs {:INDEX (as-input INDEX), :ITEM (as-input ITEM)}})

(defn data-delete-from-list [LIST INDEX]
  {:opcode "data_deleteoflist"
   :fields {:LIST (as-list LIST)}
   :inputs {:INDEX (if (= :last INDEX)
                     (number-input "last")
                     (as-input INDEX))}})

(defn data-delete-all-list [LIST]
  {:opcode "data_deletealloflist"
   :fields {:LIST (as-list LIST)}})

(defn data-item-of-list [LIST INDEX]
  {:opcode "data_itemoflist"
   :fields {:LIST (as-list LIST)}
   :inputs {:INDEX (as-input INDEX)}})

(defn data-length-of-list [LIST]
  {:opcode "data_lengthoflist"
   :fields {:LIST (as-list LIST)}})

(defn control-if [CONDITION SUBSTACK]
  {:opcode "control_if"
   :inputs {:CONDITION (as-boolean-input CONDITION)
            :SUBSTACK (as-input SUBSTACK)}})

(defn control-if-else [CONDITION SUBSTACK SUBSTACK2]
  {:opcode "control_if_else"
   :inputs {:CONDITION (as-boolean-input CONDITION)
            :SUBSTACK (as-input SUBSTACK)
            :SUBSTACK2 (as-input SUBSTACK2)}})

(defn control-repeat [TIMES SUBSTACK]
  {:opcode "control_repeat"
   :inputs {:TIMES (as-input TIMES)
            :SUBSTACK (as-input SUBSTACK)}})

(defn control-forever [SUBSTACK]
  {:opcode "control_forever"
   :inputs {:SUBSTACK (as-input SUBSTACK)}})

(defn control-wait [DURATION]
  {:opcode "control_wait"
   :inputs {:DURATION (as-input DURATION)}})

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
          :fields {:BROADCAST_OPTION (or (:as-variable BROADCAST_OPTION)
                                         (throw (ex-info "Expected broadcast" {:broadcast BROADCAST_OPTION})))}}
         opts))

(defn motion-change-x-by [DX] {:opcode "motion_changexby", :inputs {:DX (as-input DX)}})
(defn motion-change-y-by [DY] {:opcode "motion_changeyby", :inputs {:DY (as-input DY)}})
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
   :inputs {:CHANGE (as-input CHANGE)}})

(defn- assoc-final-next [block acc]
  (assoc block :next (if (:next block)
                       (assoc-final-next (:next block) acc)
                       acc)))

(defn do-block
  "Chain blocks together using :next. Takes multiple blocks and links them sequentially."
  [& blocks]
  (reduce (fn [acc block]
            (assoc-final-next block acc)
            #_(assoc block :next acc))
          nil
          (reverse blocks)))

;; ============================================================================
;; Additional Motion Blocks
;; ============================================================================

(defn motion-move-steps [STEPS]
  {:opcode "motion_movesteps"
   :inputs {:STEPS (as-input STEPS)}})

(defn motion-turn-right [DEGREES]
  {:opcode "motion_turnright"
   :inputs {:DEGREES (as-input DEGREES)}})

(defn motion-turn-left [DEGREES]
  {:opcode "motion_turnleft"
   :inputs {:DEGREES (as-input DEGREES)}})

(defn motion-goto-xy [X Y]
  {:opcode "motion_gotoxy"
   :inputs {:X (as-input X)
            :Y (as-input Y)}})

(defn motion-goto [TO]
  {:opcode "motion_goto"
   :inputs {:TO (as-input TO)}})

(defn motion-glide-secs-to-xy [SECS X Y]
  {:opcode "motion_glidesecstoxy"
   :inputs {:SECS (as-input SECS)
            :X (as-input X)
            :Y (as-input Y)}})

(defn motion-glide-to [SECS TO]
  {:opcode "motion_glideto"
   :inputs {:SECS (as-input SECS)
            :TO (as-input TO)}})

(defn motion-point-in-direction [DIRECTION]
  {:opcode "motion_pointindirection"
   :inputs {:DIRECTION (as-input DIRECTION)}})

(defn motion-point-towards [TOWARDS]
  {:opcode "motion_pointtowards"
   :inputs {:TOWARDS (as-input TOWARDS)}})

(defn motion-set-x [X]
  {:opcode "motion_setx"
   :inputs {:X (as-input X)}})

(defn motion-set-y [Y]
  {:opcode "motion_sety"
   :inputs {:Y (as-input Y)}})

(defn motion-set-rotation-style [STYLE]
  {:opcode "motion_setrotationstyle"
   :fields {:STYLE [STYLE nil]}})

(defn motion-direction []
  {:opcode "motion_direction"})

;; ============================================================================
;; Additional Looks Blocks
;; ============================================================================

(defn looks-say-for-secs [MESSAGE SECS]
  {:opcode "looks_sayforsecs"
   :inputs {:MESSAGE (as-input MESSAGE)
            :SECS (as-input SECS)}})

(defn looks-say [MESSAGE]
  {:opcode "looks_say"
   :inputs {:MESSAGE (as-input MESSAGE)}})

(defn looks-think-for-secs [MESSAGE SECS]
  {:opcode "looks_thinkforsecs"
   :inputs {:MESSAGE (as-input MESSAGE)
            :SECS (as-input SECS)}})

(defn looks-think [MESSAGE]
  {:opcode "looks_think"
   :inputs {:MESSAGE (as-input MESSAGE)}})

(defn looks-switch-costume-to [COSTUME]
  {:opcode "looks_switchcostumeto"
   :inputs {:COSTUME (as-input COSTUME)}})

(defn looks-next-costume []
  {:opcode "looks_nextcostume"})

(defn looks-switch-backdrop-to [BACKDROP]
  {:opcode "looks_switchbackdropto"
   :inputs {:BACKDROP (as-input BACKDROP)}})

(defn looks-next-backdrop []
  {:opcode "looks_nextbackdrop"})

(defn looks-change-size-by [CHANGE]
  {:opcode "looks_changesizeby"
   :inputs {:CHANGE (as-input CHANGE)}})

(defn looks-set-size-to [SIZE]
  {:opcode "looks_setsizeto"
   :inputs {:SIZE (as-input SIZE)}})

(defn looks-set-effect-to [EFFECT VALUE]
  {:opcode "looks_seteffectto"
   :fields {:EFFECT [EFFECT nil]}
   :inputs {:VALUE (as-input VALUE)}})

(defn looks-clear-effects []
  {:opcode "looks_cleargraphiceffects"})

(defn looks-go-to-layer [FRONT_BACK]
  {:opcode "looks_gotofrontback"
   :fields {:FRONT_BACK [FRONT_BACK nil]}})

(defn looks-go-layers [FORWARD_BACKWARD NUM]
  {:opcode "looks_goforwardbackwardlayers"
   :fields {:FORWARD_BACKWARD [FORWARD_BACKWARD nil]}
   :inputs {:NUM (as-input NUM)}})

(defn looks-costume-number []
  {:opcode "looks_costumenumbername"
   :fields {:NUMBER_NAME ["number" nil]}})

(defn looks-costume-name []
  {:opcode "looks_costumenumbername"
   :fields {:NUMBER_NAME ["name" nil]}})

(defn looks-backdrop-number []
  {:opcode "looks_backdropnumbername"
   :fields {:NUMBER_NAME ["number" nil]}})

(defn looks-backdrop-name []
  {:opcode "looks_backdropnumbername"
   :fields {:NUMBER_NAME ["name" nil]}})

(defn looks-size []
  {:opcode "looks_size"})

;; ============================================================================
;; Sound Blocks
;; ============================================================================

(defn sound-play-until-done [SOUND_MENU]
  {:opcode "sound_playuntildone"
   :inputs {:SOUND_MENU (as-input SOUND_MENU)}})

(defn sound-play [SOUND_MENU]
  {:opcode "sound_play"
   :inputs {:SOUND_MENU (as-input SOUND_MENU)}})

(defn sound-stop-all-sounds []
  {:opcode "sound_stopallsounds"})

(defn sound-change-effect-by [EFFECT VALUE]
  {:opcode "sound_changeeffectby"
   :fields {:EFFECT [EFFECT nil]}
   :inputs {:VALUE (as-input VALUE)}})

(defn sound-set-effect-to [EFFECT VALUE]
  {:opcode "sound_seteffectto"
   :fields {:EFFECT [EFFECT nil]}
   :inputs {:VALUE (as-input VALUE)}})

(defn sound-clear-effects []
  {:opcode "sound_cleareffects"})

(defn sound-change-volume-by [VOLUME]
  {:opcode "sound_changevolumeby"
   :inputs {:VOLUME (as-input VOLUME)}})

(defn sound-set-volume-to [VOLUME]
  {:opcode "sound_setvolumeto"
   :inputs {:VOLUME (as-input VOLUME)}})

(defn sound-volume []
  {:opcode "sound_volume"})

;; ============================================================================
;; Additional Event Blocks
;; ============================================================================

(defn event-when-key-pressed [KEY_OPTION & {:as opts}]
  (merge {:opcode "event_whenkeypressed"
          :topLevel true
          :fields {:KEY_OPTION [KEY_OPTION nil]}}
         opts))

(defn event-when-backdrop-switches-to [BACKDROP & {:as opts}]
  (merge {:opcode "event_whenbackdropswitchesto"
          :topLevel true
          :fields {:BACKDROP [BACKDROP nil]}}
         opts))

(defn event-when-greater-than [WHENGREATERTHANMENU VALUE & {:as opts}]
  (merge {:opcode "event_whengreaterthan"
          :topLevel true
          :fields {:WHENGREATERTHANMENU [WHENGREATERTHANMENU nil]}
          :inputs {:VALUE (as-input VALUE)}}
         opts))

;; ============================================================================
;; Additional Control Blocks
;; ============================================================================

(defn control-repeat-until [CONDITION SUBSTACK]
  {:opcode "control_repeat_until"
   :inputs {:CONDITION (as-input CONDITION)
            :SUBSTACK (as-input SUBSTACK)}})

(defn control-while [CONDITION SUBSTACK]
  {:opcode "control_while"
   :inputs {:CONDITION (as-input CONDITION)
            :SUBSTACK (as-input SUBSTACK)}})

(defn control-for-each [VARIABLE VALUE SUBSTACK]
  {:opcode "control_for_each"
   :fields {:VARIABLE (as-variable VARIABLE)}
   :inputs {:VALUE (as-input VALUE)
            :SUBSTACK (as-input SUBSTACK)}})

(defn control-stop [STOP_OPTION]
  {:opcode "control_stop"
   :fields {:STOP_OPTION [STOP_OPTION nil]}
   :mutation {:tagName "mutation" :hasnext "false"}})

(defn control-wait-until [CONDITION]
  {:opcode "control_wait_until"
   :inputs {:CONDITION (as-input CONDITION)}})

(defn control-create-clone-of [CLONE_OPTION]
  {:opcode "control_create_clone_of"
   :inputs {:CLONE_OPTION (as-input CLONE_OPTION)}})

(defn control-delete-this-clone []
  {:opcode "control_delete_this_clone"})

(defn control-when-start-as-clone [& {:as opts}]
  (merge {:opcode "control_start_as_clone"
          :topLevel true}
         opts))

;; ============================================================================
;; Sensing Blocks
;; ============================================================================

(defn sensing-touching-object [TOUCHINGOBJECTMENU]
  {:opcode "sensing_touchingobject"
   :inputs {:TOUCHINGOBJECTMENU (as-input TOUCHINGOBJECTMENU)}})

(defn sensing-touching-color [COLOR]
  {:opcode "sensing_touchingcolor"
   :inputs {:COLOR (as-input COLOR)}})

(defn sensing-color-touching-color [COLOR COLOR2]
  {:opcode "sensing_coloristouchingcolor"
   :inputs {:COLOR (as-input COLOR)
            :COLOR2 (as-input COLOR2)}})

(defn sensing-distance-to [DISTANCETOMENU]
  {:opcode "sensing_distanceto"
   :inputs {:DISTANCETOMENU (as-input DISTANCETOMENU)}})

(defn sensing-ask-and-wait [QUESTION]
  {:opcode "sensing_askandwait"
   :inputs {:QUESTION (as-input QUESTION)}})

(defn sensing-answer []
  {:opcode "sensing_answer"})

(defn sensing-key-pressed [KEY_OPTION]
  {:opcode "sensing_keypressed"
   :inputs {:KEY_OPTION (as-input KEY_OPTION)}})

(defn sensing-mouse-down []
  {:opcode "sensing_mousedown"})

(defn sensing-mouse-x []
  {:opcode "sensing_mousex"})

(defn sensing-mouse-y []
  {:opcode "sensing_mousey"})

(defn sensing-set-drag-mode [DRAG_MODE]
  {:opcode "sensing_setdragmode"
   :fields {:DRAG_MODE [DRAG_MODE nil]}})

(defn sensing-loudness []
  {:opcode "sensing_loudness"})

(defn sensing-timer []
  {:opcode "sensing_timer"})

(defn sensing-reset-timer []
  {:opcode "sensing_resettimer"})

(defn sensing-of [PROPERTY OBJECT]
  {:opcode "sensing_of"
   :fields {:PROPERTY [PROPERTY nil]}
   :inputs {:OBJECT (as-input OBJECT)}})

(defn sensing-current [CURRENTMENU]
  {:opcode "sensing_current"
   :fields {:CURRENTMENU [CURRENTMENU nil]}})

(defn sensing-days-since-2000 []
  {:opcode "sensing_dayssince2000"})

(defn sensing-username []
  {:opcode "sensing_username"})

;; ============================================================================
;; Additional Operator Blocks
;; ============================================================================

(defn op-lt [OPERAND1 OPERAND2]
  {:opcode "operator_lt"
   :inputs {:OPERAND1 (as-input OPERAND1)
            :OPERAND2 (as-input OPERAND2)}})

(defn op-random [FROM TO]
  {:opcode "operator_random"
   :inputs {:FROM (as-input FROM)
            :TO (as-input TO)}})

(defn op-join [STRING1 STRING2]
  {:opcode "operator_join"
   :inputs {:STRING1 (as-input STRING1)
            :STRING2 (as-input STRING2)}})

(defn op-letter-of [LETTER STRING]
  {:opcode "operator_letter_of"
   :inputs {:LETTER (as-input LETTER)
            :STRING (as-input STRING)}})

(defn op-length [STRING]
  {:opcode "operator_length"
   :inputs {:STRING (as-input STRING)}})

(defn op-contains [STRING1 STRING2]
  {:opcode "operator_contains"
   :inputs {:STRING1 (as-input STRING1)
            :STRING2 (as-input STRING2)}})

(defn op-mod [NUM1 NUM2]
  {:opcode "operator_mod"
   :inputs {:NUM1 (as-input NUM1)
            :NUM2 (as-input NUM2)}})

(defn op-round [NUM]
  {:opcode "operator_round"
   :inputs {:NUM (as-input NUM)}})

(defn op-mathop [OPERATOR NUM]
  {:opcode "operator_mathop"
   :fields {:OPERATOR [OPERATOR nil]}
   :inputs {:NUM (as-input NUM)}})

;; ============================================================================
;; Additional Data Blocks
;; ============================================================================

(defn data-show-variable [VARIABLE]
  {:opcode "data_showvariable"
   :fields {:VARIABLE (as-variable VARIABLE)}})

(defn data-hide-variable [VARIABLE]
  {:opcode "data_hidevariable"
   :fields {:VARIABLE (as-variable VARIABLE)}})

(defn data-show-list [LIST]
  {:opcode "data_showlist"
   :fields {:LIST (as-list LIST)}})

(defn data-hide-list [LIST]
  {:opcode "data_hidelist"
   :fields {:LIST (as-list LIST)}})

(defn data-insert-at-list [LIST INDEX ITEM]
  {:opcode "data_insertatlist"
   :fields {:LIST (as-list LIST)}
   :inputs {:INDEX (as-input INDEX)
            :ITEM (as-input ITEM)}})

(defn data-item-num-of-list [LIST ITEM]
  {:opcode "data_itemnumoflist"
   :fields {:LIST (as-list LIST)}
   :inputs {:ITEM (as-input ITEM)}})

;; ============================================================================
;; Pen Extension Blocks
;; ============================================================================

(defn pen-clear []
  {:opcode "pen_clear"})

(defn pen-stamp []
  {:opcode "pen_stamp"})

(defn pen-pen-down []
  {:opcode "pen_penDown"})

(defn pen-pen-up []
  {:opcode "pen_penUp"})

(defn pen-set-color [COLOR]
  {:opcode "pen_setPenColorToColor"
   :inputs {:COLOR (as-input COLOR)}})

(defn pen-change-param-by [COLOR_PARAM VALUE]
  {:opcode "pen_changePenColorParamBy"
   :inputs {:COLOR_PARAM (as-input COLOR_PARAM)
            :VALUE (as-input VALUE)}})

(defn pen-set-param-to [COLOR_PARAM VALUE]
  {:opcode "pen_setPenColorParamTo"
   :inputs {:COLOR_PARAM (as-input COLOR_PARAM)
            :VALUE (as-input VALUE)}})

(defn pen-change-size-by [SIZE]
  {:opcode "pen_changePenSizeBy"
   :inputs {:SIZE (as-input SIZE)}})

(defn pen-set-size-to [SIZE]
  {:opcode "pen_setPenSizeTo"
   :inputs {:SIZE (as-input SIZE)}})

;; ============================================================================
;; Music Extension Blocks
;; ============================================================================

(defn music-play-drum-for-beats [DRUM BEATS]
  {:opcode "music_playDrumForBeats"
   :inputs {:DRUM (as-input DRUM)
            :BEATS (as-input BEATS)}})

(defn music-rest-for-beats [BEATS]
  {:opcode "music_restForBeats"
   :inputs {:BEATS (as-input BEATS)}})

(defn music-play-note-for-beats [NOTE BEATS]
  {:opcode "music_playNoteForBeats"
   :inputs {:NOTE (as-input NOTE)
            :BEATS (as-input BEATS)}})

(defn music-set-instrument [INSTRUMENT]
  {:opcode "music_setInstrument"
   :inputs {:INSTRUMENT (as-input INSTRUMENT)}})

(defn music-set-tempo [TEMPO]
  {:opcode "music_setTempo"
   :inputs {:TEMPO (as-input TEMPO)}})

(defn music-change-tempo [TEMPO]
  {:opcode "music_changeTempo"
   :inputs {:TEMPO (as-input TEMPO)}})

(defn music-get-tempo []
  {:opcode "music_getTempo"})

;; ============================================================================
;; Video Sensing Extension Blocks
;; ============================================================================

(defn video-on [ATTRIBUTE SUBJECT]
  {:opcode "videoSensing_videoOn"
   :fields {:ATTRIBUTE [ATTRIBUTE nil]}
   :inputs {:SUBJECT (as-input SUBJECT)}})

(defn video-toggle [VIDEO_STATE]
  {:opcode "videoSensing_videoToggle"
   :inputs {:VIDEO_STATE (as-input VIDEO_STATE)}})

(defn video-set-transparency [TRANSPARENCY]
  {:opcode "videoSensing_setVideoTransparency"
   :inputs {:TRANSPARENCY (as-input TRANSPARENCY)}})

;; ============================================================================
;; Text to Speech Extension Blocks
;; ============================================================================

(defn tts-speak [WORDS]
  {:opcode "text2speech_speakAndWait"
   :inputs {:WORDS (as-input WORDS)}})

(defn tts-set-voice [VOICE]
  {:opcode "text2speech_setVoice"
   :fields {:VOICE [VOICE nil]}})

(defn tts-set-language [LANGUAGE]
  {:opcode "text2speech_setLanguage"
   :fields {:LANGUAGE [LANGUAGE nil]}})

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
      {:top (if (#{"procedures_prototype" "argument_reporter_string_number" "argument_reporter_boolean"}
                 (:opcode block))
              [1 id] ;; shadow block (?) by id
              [2 id]) ;; refer to block by id
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

(defn monitor [obj opts]
  (let [v (or (:variable obj) (:list obj))
        [id [k init]] v]
    {:monitor (merge {:id id
                      :mode (if (:variable obj) "default" "list")
                      :opcode (if (:variable obj) "data_variable" "data_listcontents")
                      :params {:LIST k}
                      :spriteName nil ;; nil for stage, name for sprite!
                      :value init
                      :visible true
                      :x 5
                      :y 5
                      :width 0
                      :height 0}
                     opts)}))

(defn generate-sb3 [output-sb3-path builds]
  (println (keep :monitor builds))
  (let [assets (cons
                {:file-name "project.json"
                 :content (-> {:targets (keep :target builds)
                               :monitors (keep :monitor builds)
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
