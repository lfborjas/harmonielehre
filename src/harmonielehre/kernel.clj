(ns harmonielehre.kernel
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]))

;; a map of pitch classes and their enharmonic equivalents
(def pitch-classes
  (apply array-map [:C  [:Ass :Bs :Dff]
                    :Cs [:Df]
                    :D  []
                    :Ds [:Ef]
                    :E  []
                    :F  []
                    :Fs []
                    :G  []
                    :Gs []
                    :A  []
                    :As []
                    :B  []]))

(defn lookup-pc
  "Take a pitch class and return its position, accounting for enharmonics"
  [pc]
  (let [indexed (map vector
                     (iterate inc 0)
                     (map #(apply cons (seq %)) pitch-classes))]
    (first (for [[i v] indexed :when (some #{pc} v)] i))))


(comment
  (some #{2} #{1 2 3})
  (lookup-pc :C) ;; 0
  (lookup-pc :B) ;; 11
  (lookup-pc :Ef) ;; 3
)

;; Inspired by:
;; https://github.com/Euterpea/Euterpea2/blob/master/Euterpea/Music.lhs#L244-L260

(defn abs-pitch->pitch
  "Take an absolute pitch, as would come from MIDI,
  and return the equivalent pitch class and octave"
  [ap]
  (let [oct (dec (quot ap 12))
        pos (mod ap 12)
        pc  (nth (keys pitch-classes) pos)]
    [pc, oct]))

(defn pitch->abs-pitch
  "Take a pitch-class,octave combo and return the absolute pitch"
  [[pc, o]]
  (let [oct-scale (* 12 (inc o))
        pos       (lookup-pc pc)]
    (+ oct-scale pos)))

(comment
  (abs-pitch->pitch 60) ; [C, 4]
  (abs-pitch->pitch 0)  ; [C, -1]
  (abs-pitch->pitch 127) ; [G, 9]
  (pitch->abs-pitch [:G 9])
  (pitch->abs-pitch [:C 4])
  (pitch->abs-pitch [:Ass 4])
  (pitch->abs-pitch [:A, 0]) ;; 21, lowest piano note
  (pitch->abs-pitch [:C, 8]) ;; 108, highest piano note
  )

;; LOGIC

;; inspired by: https://github.com/owickstrom/smug/blob/f7a08bdf41fd27a63f548fe857912aa983c23e2f/src/smug/music.clj

;; biased for piano (could use the lowest/highest that MIDI supports instead?)
(def lowest-note  (pitch->abs-pitch [:A, 0]))
(def highest-note (pitch->abs-pitch [:C, 8]))

(defn abs-pitcho [p]
  (fd/in p (fd/interval lowest-note highest-note)))

(defn major-thirdo [a b]
  (l/fresh [d]
    (l/== d 4)
    (abs-pitcho a)
    (abs-pitcho b)

    (l/conde [(fd/- a b d)] ;; is a an ~interval _above_ b?
             [(fd/- b a d)])))


(comment (l/run* [q]
           (major-thirdo (pitch->abs-pitch [:E, 4])
                         (pitch->abs-pitch [:C, 4])))
         (map (fn [[a b]] (vector (abs-pitch->pitch a) (abs-pitch->pitch b)))
              (l/run 10 [q p]
                (major-thirdo q p)))
         (l/run* [p]
           (major-thirdo p (pitch->abs-pitch [:C, 4]))))


