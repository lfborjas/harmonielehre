(ns harmonienlehre.main
  (:require
   [harmonielehre.kernel :as kernel]
   [harmonielehre.midi :as midi]))


(def recorded-notes (atom {:last-ts (System/currentTimeMillis) :notes []}))

(defn midi->note [midi-msg]
  (let [[pc oct] (kernel/abs-pitch->pitch (:note midi-msg))]
    (midi/->Note pc oct 1/4)))

(comment
  (midi->note {:note 60}))

(defn record-note
  "Given a midi message and a note, add it to the recorded-notes atom

  Only considers note-on events"
  [midi-msg ts]
  (let [command (midi/midi-shortmessage-command (:cmd midi-msg))
        note    (:note midi-msg)]
    (when (= :note-on command)
      (swap! recorded-notes assoc :last-ts ts)
      (swap! recorded-notes assoc :notes
             (conj (@recorded-notes :notes)
                   (midi->note midi-msg))))))

(comment
  (record-note {:cmd 144 :note 64} (System/currentTimeMillis)))

;; ]}
;; harmonienlehre.main> (record-note {:cmd 144 :note 64} (System/currentTimeMillis))
;; {:last-ts 1567292680091,
;; :notes
;; [{:pitch :C, :octave 4, :duration 1/4}
;; {:pitch :C, :octave 4, :duration 1/4}
;; {:pitch :E, :octave 4, :duration 1/4}]}

;; now we're ready to handle events:

(comment
  (def kdp (midi/midi-in "KDP110"))
  (midi/midi-handle-events kdp record-note)
  (midi/perform (@recorded-notes :notes)))