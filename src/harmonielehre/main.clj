(ns harmonienlehre.main
  (:require
   [harmonielehre.kernel :as kernel]
   [harmonielehre.midi :as midi]))


(def recorded-notes* (ref []))
(def state* (atom {:last-ts (System/currentTimeMillis)
                   :starting-ts nil
                   :end-ts nil
                   :active {}
                   :finished {}}))

(defn midi->note [midi-msg dur]
  (let [[pc oct] (kernel/abs-pitch->pitch (:note midi-msg))]
    (midi/->Note pc oct dur (:vel midi-msg) )))

(comment
  (midi->note {:note 60 :vel 64} 88))

(defn active-note
  "Records the instant when a note became active"
  [note midi-msg ts]
  ;; the note may alredy be active
  (when (not (contains? (:active @state*) note))
    (swap! state* assoc-in [:active note] (assoc midi-msg :ts ts))))

(defn finished-note
  "Record a note being off"
  [note midi-msg current-ts]
  ;; only do something if we know of an active version of this note
  (when-let [sound (get-in @state* [:active note])]
    ;; record this note
    (dosync (alter recorded-notes* conj
                   (midi->note sound
                               ;; the duration of a note
                               ;; is the current timestamp
                               ;; substracting when it became active
                               (- current-ts (:ts sound)))))
    ;; mark it as no longer active
    (swap! state* update-in [:active] dissoc note)
    (swap! state* assoc-in  [:finished note] sound)))

(defn record-midi
  "Given an incoming message and a timestamp, record it."
  [midi-msg ts]
  (let [command (midi/midi-shortmessage-command (:cmd midi-msg))
        note (:note midi-msg)]
    (case command
      :note-on  (active-note note midi-msg ts)
      :note-off (finished-note note midi-msg ts)
      nil)))

(comment
  ())

;; ]}
;; harmonienlehre.main> (record-note {:cmd 144 :note 64} (System/currentTimeMillis))
;; {:last-ts 1567292680091,
;; :notes
;; [{:pitch :C, :octave 4, :duration 1/4}
;; {:pitch :C, :octave 4, :duration 1/4}
;; {:pitch :E, :octave 4, :duration 1/4}]}

;; now we're ready to handle events:


;; all the code needed to perform notes coming from the piano!
(comment
  ;; KDP110 is part of the unique identifier for my piano,
  ;; run (midi/midi-devices) to display all valid midi devices (software and hardware)
  ;; currently active. For the KDP110 piano, setup is here:
  ;; http://www.kawai-global.com/support/bluetooth/#connect-macos
  (def kdp (midi/midi-in "KDP110"))
  (midi/midi-handle-events kdp record-note)
  (midi/perform (@recorded-notes :notes)))
