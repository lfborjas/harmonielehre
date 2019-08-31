(ns harmonielehre.midi
  (:require [harmonielehre.kernel :as kernel])
  (:import  [javax.sound.midi MidiSystem Sequencer Synthesizer Receiver ShortMessage]
            [java.util.regex Pattern]))

;; The code here is from the Programming Clojure book:
;; https://github.com/stuarthalloway/programming-clojure/blob/master/src/examples/midi.clj

(defprotocol MidiNote
  (to-msec [this tempo])
  (key-number [this])
  (play [this tempo midi-channel]))

(defn perform [notes & {:keys [tempo] :or {tempo 88}}]
  (with-open [synth (doto (MidiSystem/getSynthesizer) .open)]
    (let [channel (aget (.getChannels synth) 0)]
      (doseq [note notes]
        (play note tempo channel)))))

(defrecord Note [pitch octave duration]
  MidiNote
  (to-msec [this tempo]
    (let [duration-to-bpm {1 240, 1/2 120, 1/4 60, 1/8 30, 1/16 15}]
      (* 1000 (/ (duration-to-bpm (:duration this))
                 tempo))))
  (key-number [this]
    (kernel/pitch->abs-pitch [(:pitch this) (:octave this)]))
  (play [this tempo midi-channel]
    (let [velocity (or (:velocity this) 64)]
      (.noteOn midi-channel (key-number this) velocity)
      (Thread/sleep (to-msec this tempo)))))

;; Connect to a MIDI device via bluetooth.

;; In the particular case of my piano and my computer, this helped!
;; http://www.kawai-global.com/support/bluetooth/


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
                   (send [msg timestamp] (fun (midi-msg msg) timestamp)))]
    (.setReceiver (:transmitter input) receiver)))

(def midi-shortmessage-command 
  {ShortMessage/CHANNEL_PRESSURE :channel-pressure
   ShortMessage/CONTROL_CHANGE :control-change
   ShortMessage/NOTE_OFF :note-off
   ShortMessage/NOTE_ON :note-on
   ShortMessage/PITCH_BEND :pitch-bend
   ShortMessage/POLY_PRESSURE :poly-pressure
   ShortMessage/PROGRAM_CHANGE :program-change})

(defn is-sostenuto-pedal?
  "Was the leftmost pedal pressed?"
  [midi-msg]
  (and (= :control-change (midi-shortmessage-command (:cmd midi-msg)))
       (= 67 (:note midi-msg))))

(defn is-sustain-pedal?
  "Was the sustain pedal pressed?"
  [midi-msg]
  (and (= :control-change (midi-shortmessage-command (:cmd midi-msg)))
       (= 64 (:note midi-msg))))

;; connecting to my piano:

(comment
  (def kdp (midi-in "KDP110")) ;; this works due to the setup described earlier
  ;; creates a separate thread if not careful!
  (midi-handle-events kdp (fn [msg ts] (println msg))))

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

;; END OF COPY-PASTE


;; harmonielehre.midi> (midi-devices)
;; ({:name "Gervill",
;;   :description "Software MIDI Synthesizer",
;;   :vendor "OpenJDK",
;;   :version "1.0",
;;   :sources 0,
;;   :sinks -1,
;;   :info
;;   #object[com.sun.media.sound.SoftSynthesizer$Info 0x61cbd6ed "Gervill"],
;;   :device
;;   #object[com.sun.media.sound.SoftSynthesizer 0x1963426e "com.sun.media.sound.SoftSynthesizer@1963426e"]}
;;  {:name "Real Time Sequencer",
;;   :description "Software sequencer",
;;   :vendor "Oracle Corporation",
;;   :version "Version 1.0",
;;   :sources -1,
;;   :sinks -1,
;;   :info
;;   #object[com.sun.media.sound.RealTimeSequencer$RealTimeSequencerInfo 0x1e87f12 "Real Time Sequencer"],
;;   :device
;;   #object[com.sun.media.sound.RealTimeSequencer 0x73a06f04 "com.sun.media.sound.RealTimeSequencer@73a06f04"]}
;;  {:name "Bluetooth",
;;   :description "KDP110 Bluetooth",
;;   :vendor "Kawai Musical Instruments Manufacturing Co., Ltd.",
;;   :version "2",
;;   :sources 0,
;;   :sinks -1,
;;   :info
;;   #object[com.sun.media.sound.MidiOutDeviceProvider$MidiOutDeviceInfo 0x239726fd "Bluetooth"],
;;   :device
;;   #object[com.sun.media.sound.MidiOutDevice 0x4cd125b "com.sun.media.sound.MidiOutDevice@4cd125b"]}
;;  {:name "Bluetooth",
;;   :description "KDP110 Bluetooth",
;;   :vendor "Kawai Musical Instruments Manufacturing Co., Ltd.",
;;   :version "2",
;;   :sources -1,
;;   :sinks 0,
;;   :info
;;   #object[com.sun.media.sound.MidiInDeviceProvider$MidiInDeviceInfo 0x1530d475 "Bluetooth"],
;;   :device
;;   #object[com.sun.media.sound.MidiInDevice 0x104f4d5a "com.sun.media.sound.MidiInDevice@104f4d5a"]})
