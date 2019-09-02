(ns harmonienlehre.main
  (:require
   [harmonielehre.kernel :as kernel]
   [harmonielehre.midi :as midi]))


(def recorded-notes* (ref []))
(def state* (atom {:last-ts (System/currentTimeMillis)
                   :inst nil
                   :starting-ts nil
                   :end-ts nil
                   :active {}
                   :finished {}}))

(defn midi->note [midi-msg dur]
  (let [[pc oct] (kernel/abs-pitch->pitch (:note midi-msg))]
    (midi/->Note pc oct dur (:vel midi-msg))))

(comment
  (midi->note {:note 60 :vel 64} 88))

(defn record-silence
  "Potentially record a silence if time has elapsed between notes"
  [now last-ts]
  (let [elapsed (- now last-ts)]
    (if (and (> elapsed 0)
             (empty? (:active @state*)))
      ;; if no notes are active, and sometime has elapsed, record as a silence
      (dosync (alter recorded-notes* conj (midi/->Rest elapsed))))))

(defn active-note
  "Records the instant when a note became active"
  [note midi-msg ts]
  ;; the note may already be active
  (when (not (contains? (:active @state*) note))
    (record-silence ts (:last-ts @state*))
    (swap! state* assoc-in [:active note] (assoc midi-msg :ts ts))))

(defn finished-note
  "Record a note being off"
  [note midi-msg current-ts]
  ;; only do something if we know of an active version of this note
  (when-let [sound (get-in @state* [:active note])]
    ;; record this note
    (dosync (alter recorded-notes* conj
                   (merge (midi->note sound
                                      ;; the duration of a note
                                      ;; is the current timestamp
                                      ;; substracting when it became active
                                      (- current-ts (:ts sound)))
                          ;; add the optional start/end timestamps
                          ;; to the note to aid in sequencing.
                          {:start-ts (:ts sound) :end-ts current-ts})))
    ;; mark it as no longer active
    (swap! state* update-in [:active] dissoc note)
    (swap! state* assoc-in  [:finished note] sound)
    ;; record the last time a note was off
    (swap! state* assoc :last-ts current-ts)))

(defn record-midi
  "Given an incoming message and a timestamp, record it."
  [midi-msg ts]
  (let [command (midi/midi-shortmessage-command (:cmd midi-msg))
        note (:note midi-msg)]
    (case command
      :note-on  (active-note note midi-msg ts)
      :note-off (finished-note note midi-msg ts)
      nil)))

;; harmonienlehre.main> (record-midi {:cmd 144 :note 64 :vel 28} (System/currentTimeMillis))
;; {:last-ts 1567308173809,
;;  :starting-ts nil,
;;  :end-ts nil,
;;  :active {64 {:cmd 144, :note 64, :vel 28, :ts 1567308201297}},
;;  :finished {64 {:cmd 144, :note 64, :vel 28, :ts 1567308166970}}}
;; harmonienlehre.main> (record-midi {:cmd 128 :note 64 :vel 28} (System/currentTimeMillis))
;; {:last-ts 1567308215512,
;;  :starting-ts nil,
;;  :end-ts nil,
;;  :active {},
;;  :finished {64 {:cmd 144, :note 64, :vel 28, :ts 1567308201297}}}
;; harmonienlehre.main> @recorded-notes*
;; [{:rest 12358}
;;  {:pitch :E, :octave 4, :duration 6839, :velocity 28}
;;  {:rest 27488}
;;  {:pitch :E, :octave 4, :duration 14215, :velocity 28}]
;; harmonienlehre.main> 

(defn persist-notes
  "saves the given list of notes to the specified file"
  [notes filename]
  (spit filename (clojure.string/join "\n" (map pr-str notes))
        :append true))

(def saved-recorded-notes (partial persist-notes @recorded-notes*))

(defn read-notes
  "reads notes from a file"
  [filename]
  (->> (slurp filename)
       clojure.string/split-lines
       (map read-string)))

;; all the code needed to perform notes coming from the piano!
(comment
  ;; KDP110 is part of the unique identifier for my piano,
  ;; run (midi/midi-devices) to display all valid midi devices (software and hardware)
  ;; currently active. For the KDP110 piano, setup is here:
  ;; http://www.kawai-global.com/support/bluetooth/#connect-macos
  ;; Make sure to connect the device each time in through the MIDI Studio
  ;; (part of Audio MIDI Setup)
  (def kdp (midi/midi-in "KDP110"))
  (swap! state* assoc :inst kdp)
  (midi/midi-handle-events kdp record-midi)
  (midi/perform  @recorded-notes*))
