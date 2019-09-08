(ns harmonielehre.midi
  (:require [harmonielehre.kernel :as kernel])
  (:import  [javax.sound.midi MidiSystem
             Sequence
             Sequencer
             Synthesizer
             Receiver
             ShortMessage
             MidiEvent
             ControllerEventListener]
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
  (play [this midi-channel ppqn tempo]))

(defprotocol Music
  ;; meant to return the single notes representing
  ;; this musical value; can optionally merge any
  ;; "attrs to inherit" into each note.
  (units [this attrs-to-inherit])
  (dur   [this]))

(defn ticks->ms
  "Given a number of ticks, a resolution and a tempo (in BPM), calculate how many miliseconds
  it takes to perform."
  [ticks ppqn bpm]
  (let [ticks-per-second (* ppqn (/ bpm 60))
        ms               (* (/ ticks ticks-per-second) 1000)]
    (float ms)))

(defn ticks
  "Given a resolution and a fractional musical value, how many ticks would that value take
  This assumes that the fractional value is relative to a beat of 1 quarter note."
  [dur ppqn]
  (let [dur-in-qn (/ dur 1/4)]
    (int  (* ppqn dur-in-qn))))

(defn dur->ms
  "Given a fractional duration, a resolution and a tempo, determine how many ms it would take
  to perform it"
  [dur ppqn bpm]
  (-> dur
      (ticks ppqn)
      (ticks->ms ppqn bpm)))

(defrecord Rest [duration]
  MidiNote
  ;; 123 is MIDI for all notes off but... not using it for exactly that
  (key-number [this] -1)
  ;; "playing" a rest is simply blocking the channel's thread doing nothing
  ;; for a few milliseconds (MIDI has no use, or notation, for rests)
  (play [this _ ppqn tempo]
    (Thread/sleep (dur->ms (:duration this) ppqn tempo)))
  Music
  (units [this _] [])
  (dur   [this] (:duration this)))

(defn rest
  "Create a musical rest"
  [dur]
  (->Rest dur))

(defrecord Note [pitch octave duration velocity]
  MidiNote
  (key-number [this]
    (kernel/pitch->abs-pitch [(:pitch this) (:octave this)]))
  (play [this midi-channel ppqn tempo]
    (let [velocity (or (:velocity this) 64)]
      ;; immediately start playing the note
      (.noteOn midi-channel (key-number this) velocity)
      (Thread/sleep (dur->ms (:duration this) ppqn tempo))
      (.noteOff midi-channel (key-number this) velocity)))
  Music
  (units [this _] [this])
  (dur   [this] (:duration this)))

(defn note
  "Create a musical note"
  ([p o d v] (->Note p o d v))
  ([p o d  ] (note p o d 67))
  ([p o    ] (note p o 1/4 67)))

(defrecord Chord [pitches]
  MidiNote
  (key-number [this]
    (map key-number (:pitches this)))
  (play [this midi-channel ppqn tempo]
    (doseq [n (:pitches this)]
      (.noteOn midi-channel (key-number n) (:velocity n)))
    (Thread/sleep (dur->ms (dur this) ppqn tempo))
    (doseq [n (:pitches this)]
      (.noteOff midi-channel (key-number n) 0)))
  Music
  (units [this attrs-to-inherit]
    (let [to-merge (apply hash-map
                          (mapcat #(vector % (get this %)) attrs-to-inherit))]
      (map #(merge % to-merge) (:pitches this))))
  (dur   [this] (apply max (map dur (:pitches this)))))

(defn chord
  "Create a chord, which is just an array of notes that share a duration/velocity"
  ([& ps] (->Chord ps)))

(comment
  (def c-maj (chord (note :C 4) (note :E 4) (note :G 4)))
  (def e-maj (chord (note :E 4) (note :G 4) (note :B 4)))
  (dur c-maj)
  (units (assoc c-maj :swing true) [:swing])
  (perform [c-maj e-maj]))

(defn perform
  "Perform a sequence of notes without creating a sequence, best for interactive development.

  But to get actual MIDI calculations, use sequencer-perform."
  ([notes & {:keys [tempo ppqn] :or {tempo 120 ppqn 96}}]
   (with-open [synth (doto (MidiSystem/getSynthesizer) .open)]
     (let [channel (aget (.getChannels synth) 0)]
       (doseq [note notes]
         (play note channel ppqn tempo))))))

(comment
  (perform [(note :C 4) (note :D 4) (note :E 4)
            (note :C 5 1/16) (note :D 5 1/16) (note :E 5 1/16) (rest 1/4)])
  (perform [(note :C 4) (note :D 4) (note :E 4)
            (note :C 5 1/16) (note :D 5 1/16) (note :E 5 1/16) (rest 1/4)]
           :tempo 32))

(defn add-ticks
  "Stateful transducer that adds cumulative ticks to a given sequence

  My first transducer! The docs do a good job explaining it:
  https://clojure.org/reference/transducers#_creating_transducers"
  ([ppqn]
   (fn [xf]
     (let [prev (volatile! 0)]
       (fn
         ([] (xf))
         ([result] (xf result))
         ([result input]
          (let [prior @prev
                ts (ticks (dur input) ppqn)
                nv (vswap! prev #(+ ts %))]
            (xf result (merge input {:start-tick prior :end-tick nv}))))))))
  ([notes ppqn] (sequence (add-ticks ppqn) notes)))

(comment
  (add-ticks [(note :C 4) (note :D 4)] 96))

(defn notes->events
  "Creates a sequence of maps ready to be sequenced as MIDI.

  Involves calculating the key number (absolute pitch) and ticks of each event,
  and removing any rests (since rests are only useful for timing and not part of
  actual MIDI)"
  [notes ppqn]
  (let [xevents (comp
                 ;; add start and end ticks (all notes
                 ;; in a chord ought to have the same tick)
                 (add-ticks ppqn)
                 ;; remove silences, split chords
                 (mapcat #(units % [:start-tick :end-tick]))
                 ;; add absolute pitch (MIDI key number)
                 (map #(assoc % :note (key-number %)))
                 ;; split each note into on/off events
                 (mapcat #(vector (merge % {:cmd :note-on  :tick (:start-tick %)})
                                  (merge % {:cmd :note-off :tick (:end-tick %)}))))]
    (sequence xevents notes)))

(comment
  (notes->events [(note :C 4) (note :D 4) (rest 1/4) (note :E 4)] 96)
  (notes->events [(note :C 4) (note :E 4) (note :G 4) (rest 1/4)
                  (chord (note :C 4)
                         (note :E 4)
                         (note :G 4))] 96))

(defn event->short-message
  "Creates a MIDI ShortMessage based on an event"
  [event]
  (doto (ShortMessage.)
    (.setMessage (commands (:cmd event))
                 (get event :chan 0)
                 (:note event)
                 (:velocity event))))

(comment
  (event->short-message (first (notes->events [(note :C 4)] 96))))

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

#_(defn normalize-sequence
    "Given a raw MIDI sequence recorded live, normalize (remove control changes, reset ticks)"
    [s]
    (let [new-seq (Sequence. Sequence/PPQ (.getResolution s) 1)
          track   (first (.getTracks new-seq))
          first-tick (.getTick (.get (first .getTracks s) 0))]
      (doseq [])))

(defn sequencer-perform
  "Takes a seq of notes, creates a sequence, loads it into the sequencer, and performs.

  It can either take a single line of music, or multiple lines in an array.
  If given multiple lines, it'll play them in parallel.

  From: https://docs.oracle.com/javase/tutorial/sound/MIDI-seq-methods.html
  And : https://docs.oracle.com/javase/8/docs/api/javax/sound/midi/Sequencer.html#"
  ([notes]
   (sequencer-perform notes {}))
  ([notes {:keys [tempo ppqn] :or {tempo 120 ppqn 96}}]
   (sequencer-perform [notes] tempo ppqn))
  ([lines tempo ppqn]
   (with-open [sequencer (doto (MidiSystem/getSequencer) .open)]
     ;; TODO: support for multiple lists of notes.
     ;; I believe we can already be very close:
     ;; call notes->events on each list of notes
     ;; simply concat (or zip) those events and give to events->sequence: the sequence knows to insert
     ;; at the right place!
     (let [line-events (mapcat #(notes->events % ppqn) lines)
           sequence  (events->sequence line-events ppqn)]
       (.setSequence sequencer sequence)
       (.setTempoInBPM sequencer tempo)
       (.start sequencer)
       (while (.isRunning sequencer) (Thread/sleep 2000))))))

(comment
  (sequencer-perform [(note :C 4) (note :D 4) (rest 1/4) (note :E 4) (rest 1/4) (note :F 4)])
  (sequencer-perform [(note :C 4) (note :D 4) (rest 1/4) (note :E 4) (rest 1/4) (note :F 4)]
                     {:tempo 240})
  (sequencer-perform [(note :C 4) (note :E 4) (note :G 4) (rest 1/4)
                      (chord (note :C 4)
                             (note :E 4)
                             (note :G 4))])
  (def line-1 [(note :C 4) (note :E 4) (note :G 4) (rest 1/4)  (note :E 5)])
  (def line-2 [(note :G 3) (note :A 3) (rest 1/4)  (note :B 4) (note :C 5)])
  (sequencer-perform [line-1 line-2] 240 96))

(defn perform-from-sequence
  [sequenz]
  (with-open [sequencer (doto (MidiSystem/getSequencer) .open)]
    (let [first-tick (.getTick (.get (first (.getTracks sequenz)) 0))]
      (.setSequence sequencer sequenz)
      ;; set to the first tick, since it may have a hilariously high value!
      (.setTickPosition sequencer first-tick)
      (.start sequencer)
      (while (.isRunning sequencer) (Thread/sleep 2000)))))

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

(defn is-soft-pedal?
  "Was the leftmost pedal pressed?"
  [midi-msg]
  (and (= :control-change (midi-shortmessage-command (:cmd midi-msg)))
       ;; aka the "una corda" pedal in grands.
       (= 67 (:note midi-msg))))

(defn is-sustain-pedal?
  "Was the sustain pedal pressed?"
  [midi-msg]
  (and (= :control-change (midi-shortmessage-command (:cmd midi-msg)))
       ;; aka the "damper" pedal
       (= 64 (:note midi-msg))))

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


(defn midi-record-sequence
  "Synchronous method to start recording from an input. Returns the captured MIDI Sequence

  It waits until the damper pedal is pressed to start recording, and will stop recording
  once the sustain pedal is pressed.

  From the 'Recording and Saving Sequences' heading at:
  https://docs.oracle.com/javase/tutorial/sound/MIDI-seq-methods.html"
  [input]
  (with-open [sequencer (doto (MidiSystem/getSequencer) .open)]
    (let [sequence    (Sequence. Sequence/PPQ 960 1)
          receiver    (.getReceiver sequencer)]
      ;; listen for sustain and soft pedal events only
      (.setSequence sequencer sequence)
      (.recordEnable sequencer (first (.getTracks sequence)) -1)
      ;; capture events from the MIDI device into this sequencer's receiver
      (.setReceiver (:transmitter input) receiver)
      (.startRecording sequencer)
      ;; and then wait until we signal we're done recording
      (while (.isRecording sequencer)
        (let [track        (first (.getTracks sequence))
              seq-size     (.size track)
              last-event   (.get track (- seq-size (min seq-size 2)))
              last-message (.getMessage last-event)]
          (if (and (instance? ShortMessage last-message)
                   (= 67 (.getData1 last-message)))
            (.stopRecording sequencer)
            ;; not done recording, wait a bit.
            (Thread/sleep 2000))))
      sequence)))

;; connecting to my piano:

(comment
  (def kdp (midi-in "KDP110")) ;; this works due to the setup described earlier
  ;; creates a separate thread if not careful!
  (midi-handle-events kdp (fn [msg ts] (println [msg ts])))
  (def bach (midi-record-sequence kdp)))

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
