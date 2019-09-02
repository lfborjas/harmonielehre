(ns harmonielehre.midi
  (:require [harmonielehre.kernel :as kernel])
  (:import  [javax.sound.midi MidiSystem
             Sequence
             Sequencer
             Synthesizer
             Receiver
             ShortMessage
             MidiEvent]
            [java.util.regex Pattern]))


;; fns based on the MIDI standard:
;; http://fmslogo.sourceforge.net/manual/midi-table.html

(def midi-shortmessage-command 
  {ShortMessage/CHANNEL_PRESSURE :channel-pressure
   ShortMessage/CONTROL_CHANGE :control-change
   ShortMessage/NOTE_OFF :note-off
   ShortMessage/NOTE_ON :note-on
   ShortMessage/PITCH_BEND :pitch-bend
   ShortMessage/POLY_PRESSURE :poly-pressure
   ShortMessage/PROGRAM_CHANGE :program-change})

(def commands
  {:note-on ShortMessage/NOTE_ON
   :note-off ShortMessage/NOTE_OFF})

;; The code here is from the Programming Clojure book:
;; https://github.com/stuarthalloway/programming-clojure/blob/master/src/examples/midi.clj

(defprotocol MidiNote
  (key-number [this])
  (play [this midi-channel tempo]))

(defn dur->msec
  "Given a fractional description of a note and a tempo, return a millisecond value"
  [dur tempo]
  (let [duration-to-bpm {1 240, 1/2 120, 1/4 60, 1/8 30, 1/16 15}]
    (* 1000 (/ (duration-to-bpm dur)
               tempo))))

;; TODO: implement this one too!
#_(defn msec->dur
    "Given a duration in milliseconds, return it relative to a tempo"
    [ms tempo]
    (womp))

(defn scale-dur
  "Given an object with duration, and a tempo scale, return a value in millis"
  [m s]
  (/ (:duration m) (* 1000 s)))

(defrecord Rest [duration]
  MidiNote
  ;; 123 is MIDI for all notes off but... not using it for exactly that
  (key-number [this] 123)
  ;; "playing" a rest is simply blocking the channel's thread doing nothing
  ;; for a few milliseconds (MIDI has no use, or notation, for rests)
  (play [this _ tempo]
    (Thread/sleep (scale-dur this tempo))))

(defrecord Note [pitch octave duration velocity]
  MidiNote
  (key-number [this]
    (kernel/pitch->abs-pitch [(:pitch this) (:octave this)]))
  (play [this midi-channel tempo]
    (let [velocity (or (:velocity this) 64)]
      ;; immediately start playing the note
      (.noteOn midi-channel (key-number this) velocity)
      (Thread/sleep (scale-dur this tempo))
      (.noteOff midi-channel (key-number this) velocity))))

(defn perform
  "Plays a seq of notes live, scaled by a given tempo"
  ;; TODO: this currently plays notes in sequence,
  ;; which is good for my purposes currently but, hilariously,
  ;; precludes the playing of chords! Seek inspiration in
  ;; Euterpea to solve this: https://github.com/Euterpea/Euterpea2/tree/master/Euterpea
  [notes & {:keys [tempo] :or {tempo 1}}]
  (with-open [synth (doto (MidiSystem/getSynthesizer) .open)]
    (let [channel (aget (.getChannels synth) 0)]
      (doseq [note notes]
        (play note channel tempo)))))

(comment
  (perform [(->Note :C 4 131000 125)
            (->Note :D 4 (dur->msec 1/2 88) 64)
            (->Note :E 4 (dur->msec 1/4 88) 64)] :tempo 1/4))

;; Transducer that transforms notes into events with absolute timestamps
(def xevents
  (comp
   ;; remove silences
   (remove #(not (contains? % :pitch)))
   ;; add (absolute pitch)
   (map    #(assoc % :note (kernel/pitch->abs-pitch (map % [:pitch :octave]))))
   ;; split each note into note-on/note-off commands
   (mapcat #(vector (merge % {:cmd :note-on  :ts (:start-ts %)})
                    (merge % {:cmd :note-off :ts (:end-ts   %)})))))

(defn us->tick
  "Given a microsecond timestamp, a tempo and a resolution
  return the MIDI tick equivalent

  Formula from:
  https://docs.oracle.com/javase/tutorial/sound/MIDI-seq-intro.html
  Note that we're assuming cumulative time, which the Java MIDI utilities
  seem to use, instead of delta time, which is more standard MIDI."
  [ts bpm ppqn]
  (let [ticks-per-second (* ppqn
                            (/ bpm 60))
        tick-size (/ 1 ticks-per-second)
        us->s     (/ ts 1E+6)]
    (long (/ us->s tick-size))))

(defn ticks->ms
  "Given a tick count a tempo and a resolution, return how many milliseconds that would take to perform"
  [ticks bpm ppqn]
  (let [ticks-per-second (* ppqn (/ bpm 60))
        tick-size (/ 1 ticks-per-second)]
    (long (* (* ticks tick-size) 1000))))

(defn notes->events
  "Creates a sequence of MIDI events based on a collection of notes and tempo

  It will try to use the notes' duration, or the more accurate start/end timestamps,
  if available.


  Based on: https://docs.oracle.com/javase/tutorial/sound/MIDI-seq-methods.html#124674
  And: https://docs.oracle.com/javase/tutorial/sound/MIDI-seq-intro.html"
  [notes tempo ppqn]
  (let [beginning (:start-ts (first notes))
        xticks (comp
                xevents
                (map #(merge % {:ts (- (:ts %) beginning)}))
                (map #(merge % {:tick (us->tick (:ts %) tempo ppqn)})))]
    ;; could also use sequence or eduction?
    (sequence xticks notes)))



(defn event->short-message
  "Creates a MIDI ShortMessage based on an event"
  [event]
  (doto (ShortMessage.)
    (.setMessage (commands (:cmd event))
                 (get event :chan 0)
                 (event     :note)
                 (event     :velocity))))

(defn events->sequence
  "Create a MIDI sequence given a seq of events. Adds events to
  the sequence's track, returns the sequence.

  Expects the events to be regular clojure maps/records, and instantiates the right MidiEvent
  objects.

  Will create a sequence using Pulses Per Quarter-note as its division type, the given resolution
  and 1 track initially.

  From: https://docs.oracle.com/javase/8/docs/api/javax/sound/midi/Sequence.html
  And: "
  [events ppqn]

  (let [sequence (Sequence. Sequence/PPQ ppqn 1) ;; 1 initial track
        track    (first (.getTracks sequence))]
    (doseq [event events]
      (.add track (MidiEvent. (event->short-message event)
                              (:tick event))))
    sequence))

(defn sequencer-perform
  "Takes a seq of notes, creates a sequence, loads it into the sequencer, and performs.

  From: https://docs.oracle.com/javase/tutorial/sound/MIDI-seq-methods.html"
  [notes tempo ppqn]
  (with-open [sequencer (doto (MidiSystem/getSequencer) .open)]
    (let [sequence  (-> notes (notes->events tempo ppqn) (events->sequence ppqn))
          ;; how many ticks long is the track, in total?
          total-ticks (.ticks (first (.getTracks sequence)))
          duration    (ticks->ms total-ticks tempo ppqn)]
      (.setSequence sequencer sequence)
      (.start sequencer)
      (Thread/sleep duration))))

(comment
  (def beethoven-fminor '({:pitch :C,
                           :octave 4,
                           :duration 122000,
                           :velocity 63,
                           :start-ts 78231356000,
                           :end-ts 78231478000}
                          {:duration 479000}
                          {:pitch :F,
                           :octave 4,
                           :duration 48000,
                           :velocity 82,
                           :start-ts 78231957000,
                           :end-ts 78232005000}
                          {:duration 619000}
                          {:pitch :Gs,
                           :octave 4,
                           :duration 44000,
                           :velocity 62,
                           :start-ts 78232624000,
                           :end-ts 78232668000}))
  ;; will apply the transformations necessary for sequencing,
  ;; and play at 88 BPM with a resolution of 96 pulses per quarter note.
  (sequencer-perform beethoven-fminor 88 96))


;; START OF COPY-PASTE
;; copied some useful functions from the midi-clj library:
;; https://github.com/rosejn/midi-clj/blob/dba91fb8f86e242cbd6e91c60b4dd0a27635314e/src/midi.clj
;; as illustrated in its README:
;; https://github.com/rosejn/midi-clj/blob/dba91fb8f86e242cbd6e91c60b4dd0a27635314e/README.md
;; and further used in overtone:
;; https://github.com/overtone/overtone/blob/f6d414f884f1b6d3166195b49276174efddf2cf2/src/overtone/studio/midi.clj#L277
;; https://github.com/overtone/overtone/blob/f6d414f884f1b6d3166195b49276174efddf2cf2/src/overtone/examples/midi/keyboard.clj#L25

(defn midi-devices []
  "List all the midi devices currently connected"
  (for [info (MidiSystem/getMidiDeviceInfo)]
    (let [device (MidiSystem/getMidiDevice info)]
      (with-meta
        {:name (.getName info)
         :description (.getDescription info)
         :vendor (.getVendor info)
         :version (.getVersion info)
         :sources (.getMaxTransmitters device)
         :sinks (.getMaxReceivers device)
         :info info
         :device device}
        {:type :midi-device}))))

(defn midi-device?
  "Is a given object a midi device?"
  [o]
  (= :midi-device (type o)))

(defn midi-ports
  "Get the available midi I/O ports"
  []
  (filter #(and (not (instance? Sequencer (:device %1)))
                (not (instance? Synthesizer (:device %1))))
          (midi-devices)))

;; For ideas on what to do with sequencers, see:
;; 
(defn midi-sequencers []
  "Get the midi sequencers"
  (filter #(instance? Sequencer (:device %1)) (midi-devices)))

;; For ideas on how to interact with synthesizers, see:
;; https://docs.oracle.com/javase/tutorial/sound/MIDI-synth.html
;; e.g.:
;; (def synth (first (midi/midi-synthesizers)))
;; (.getMaxPolyphony (:device synth))
;; 64
(defn midi-synthesizers []
  "Get available synthesizers"
  (filter #(instance? Synthesizer (:device %1)) (midi-devices)))

(defn midi-sources []
  "Get the midi input sources."
  (filter #(not (zero? (:sources %1))) (midi-ports)))

(defn midi-sinks 
  "Get the midi output sinks."
  []
  (filter #(not (zero? (:sinks %1))) (midi-ports)))

(defn midi-find-device 
  "Takes a set of devices returned from either (midi-sources) or (midi-sinks), and a
  search string.  Returns the first device where either the name or description
  mathes using the search string as a regexp."
  [devs dev-name]
  (first (filter 
          #(let [pat (Pattern/compile dev-name Pattern/CASE_INSENSITIVE)]
             (or (re-find pat (:name %1)) 
                 (re-find pat (:description %1))))
          devs)))

(defn- with-receiver 
  "Add a midi receiver to the sink device info."
  [sink-info]
  (let [dev (:device sink-info)]
    (if (not (.isOpen dev))
      (.open dev))
    (assoc sink-info :receiver (.getReceiver dev))))

(defn- with-transmitter 
  "Add a midi transmitter to the source info."
  [source-info]
  (let [dev (:device source-info)]
    (if (not (.isOpen dev))
      (.open dev))
    (assoc source-info :transmitter (.getTransmitter dev))))

(defn midi-in 
  "Open a midi input device for reading.  If no argument is given then
  a selection list pops up to let you browse and select the midi device."
  #_([] (with-transmitter
          (.get (midi-port-chooser "Midi Input Selector" (midi-sources)))))
  ([in] 
   (let [source (cond
                  (string? in) (midi-find-device (midi-sources) in)
                  (midi-device? in) in)]
     (if source
       (with-transmitter source)
       (do 
         (println "Did not find a matching midi input device for: " in)
         nil)))))

(defn midi-msg 
  "Make a clojure map out of a midi object."
  [obj]
  {:chan (.getChannel obj)
   :cmd  (.getCommand obj)
   :note (.getData1 obj)
   :vel  (.getData2 obj)
   :data1 (.getData1 obj)
   :data2 (.getData2 obj)
   })

(defn midi-handle-events 
  "Specify a single handler that will receive all midi events from the input device."
  [input fun]
  (let [receiver (proxy [Receiver] []
                   (close [] nil)
                   ;; note that the timestamp is in MICROSECONDS, not millis!
                   ;; also, this is a Java nicety and not part of the MIDI wire protocol,
                   ;; which is what this receiver is listening for:
                   ;; https://docs.oracle.com/javase/tutorial/sound/MIDI-messages.html
                   (send [msg timestamp] (fun (midi-msg msg) timestamp)))]
    (.setReceiver (:transmitter input) receiver)))



(defn is-sostenuto-pedal?
  "Was the leftmost pedal pressed?"
  [midi-msg]
  (and (= :control-change (midi-shortmessage-command (:cmd midi-msg)))
       ;; note that according to the MIDI standard, the actual sostenuto
       ;; pedal should be coming in as 66, since 67 is the soft pedal
       ;; but 67 is what my piano reports, even though the pedal does function
       ;; as sostenuto.
       (= 67 (:note midi-msg))))

(defn is-sustain-pedal?
  "Was the sustain pedal pressed?"
  [midi-msg]
  (and (= :control-change (midi-shortmessage-command (:cmd midi-msg)))
       ;; aka the "damper" pedal
       (= 64 (:note midi-msg))))

;; connecting to my piano:

(comment
  (def kdp (midi-in "KDP110")) ;; this works due to the setup described earlier
  ;; creates a separate thread if not careful!
  (midi-handle-events kdp (fn [msg ts] (println [msg ts]))))

;; will print things like:

;; {:chan 0, :cmd 144, :note 69, :vel 43, :data1 69, :data2 43}
;; {:chan 0, :cmd 128, :note 69, :vel 70, :data1 69, :data2 70}
;; {:chan 0, :cmd 144, :note 62, :vel 44, :data1 62, :data2 44}
;; {:chan 0, :cmd 128, :note 62, :vel 99, :data1 62, :data2 99}
;; {:chan 0, :cmd 144, :note 50, :vel 42, :data1 50, :data2 42}
;; {:chan 0, :cmd 128, :note 50, :vel 68, :data1 50, :data2 68}
;; {:chan 0, :cmd 144, :note 45, :vel 23, :data1 45, :data2 23}
;; {:chan 0, :cmd 128, :note 45, :vel 58, :data1 45, :data2 58}
;; {:chan 0, :cmd 144, :note 26, :vel 28, :data1 26, :data2 28}
;; {:chan 0, :cmd 128, :note 26, :vel 86, :data1 26, :data2 86}
;; {:chan 0, :cmd 176, :note 64, :vel 7, :data1 64, :data2 7}

;; note that:

;; harmonielehre.midi> (midi-shortmessage-command 144)
;; :note-on
;; harmonielehre.midi> (midi-shortmessage-command 128)
;; :note-off
;; harmonielehre.midi> (midi-shortmessage-command 176)
;; :control-change

;; FUN WITH SEQUENCERS

;; Note that I _could_ have simply wired an input device directly to a sequencer, like described here:
;; https://docs.oracle.com/javase/tutorial/sound/MIDI-seq-methods.html

;; However, we would like to also be able to produce notes programmatically (from a UI, or simple
;; clojure maps) and also have them end up in a sequence. Thus, doing that heavy work here.

