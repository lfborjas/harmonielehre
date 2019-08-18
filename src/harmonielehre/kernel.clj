(ns harmonielehre.kernel
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]))

;; a map of pitch classes and their enharmonic equivalents
;; TODO: fill out the rest of the enharmonics
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

(def intervals
  ;; name -> semitones between notes
  {:unison 0 :diminished-second 0
   :minor-second 1 :augmented-unison 1
   :major-second 2 :diminished-third 2
   :minor-third 3 :augmented-second 3
   :major-third 4 :diminished-fourth 4
   :perfect-fourth 5 :augmented-third 5
   :diminished-fifth 6 :augmented-fourth 6
   :perfect-fifth 7 :diminished-sixth 7
   :minor-sixth 8 :augmented-fifth 8
   :major-sixth 9 :diminished-seventh 9
   :minor-seventh 10 :augmented-sixth 10
   :major-seventh 11 :diminished-octave 11
   :perfect-octave 12 :augmented-seventh 12 :diminished-ninth 12
   :minor-ninth 13  :augmented-octave 13
   :major-ninth 14 :diminished-tenth 14
   :minor-tenth 15 :augmented-ninth 15
   :major-tenth 16 :diminished-eleventh 16
   :perfect-eleventh 17 :augmented-tenth 17
   :diminished-twelfth 18 :augmented-eleventh 18
   :perfect-twelfth 19 :tritave 19 :diminished-thirteenth 19
   :minor-thirteenth 20 :augmented-twelfth 20
   :major-thirteenth 21 :diminished-fourteenth 21
   :minor-fourteenth 22 :augmented-thirteenth 22
   :major-fourteenth 23 :diminished-fifteenth 23
   :double-octave 24 :perfect-fifteenth 24 :augmented-fourteenth 24})

(defn lookup-pc
  "Take a pitch class and return its position, accounting for enharmonics"
  [pc]
  (let [indexed (map vector
                     (iterate inc 0)
                     (map #(apply cons (seq %)) pitch-classes))]
    (first (for [[i v] indexed :when (some #{pc} v)] i))))


(comment
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
  (:major-third intervals)
  )


;; LOGIC

;; inspired by: https://github.com/owickstrom/smug/blob/f7a08bdf41fd27a63f548fe857912aa983c23e2f/src/smug/music.clj

;; biased for piano (could use the lowest/highest that MIDI supports instead?)
(def lowest-note  (pitch->abs-pitch [:A, 0]))
(def highest-note (pitch->abs-pitch [:C, 8]))

(defn abs-pitcho [p]
  (fd/in p (fd/interval lowest-note highest-note)))


(defn intervalo [distance a b]
  (l/all
   (abs-pitcho a)
   (abs-pitcho b)
   (l/conde [(fd/- a b distance)]
            [(fd/- b a distance)])))

(comment
  (l/run* [q]
    (intervalo (:major-third intervals) (pitch->abs-pitch [:C 4]) q)))

;; all possible triads

(defn major-triado [a b c]
  (l/all
    (intervalo (:major-third intervals) a b)
    (intervalo (:perfect-fifth intervals) a c)))

(defn minor-triado [a b c]
  (l/all
   (intervalo (:minor-third intervals) a b)
   (intervalo (:perfect-fifth intervals) a c)))

(defn chordo [n a b c]
  (l/conde
   [(l/== n :maj) (major-triado a b c)]
   [(l/== n :min) (minor-triado a b c)]))


;; all possible inversions

;; all possible chords

(comment (l/run* [q]
           (intervalo (:major-third intervals)
                      (pitch->abs-pitch [:C, 4])
                      (pitch->abs-pitch [:E, 4])))

         (l/run* [q x]
           (chordo  q
                    x
                    (pitch->abs-pitch [:E, 4])
                    (pitch->abs-pitch [:G, 4])))

         (l/run* [q]
           (intervalo q
                      (pitch->abs-pitch [:E, 4])
                      (pitch->abs-pitch [:G, 4])))

         (l/run* [q]
           (apply major-triado (map pitch->abs-pitch [[:C 4] [:E 4] [:G 4]])))
         
         (l/run 5 [x y z]
           (major-triado x y z)) ;; get 5 possible major triads

         (map (fn [[a b c]] (vector (abs-pitch->pitch a) (abs-pitch->pitch b) (abs-pitch->pitch c)))
              (l/run 13 [q p x]
                (major-triado q p x)))

         (map (fn [[b c]] (vector [:C 4] (abs-pitch->pitch b) (abs-pitch->pitch c)))
              (l/run* [p x]
                (major-triado (pitch->abs-pitch [:C 4]) p x))))


