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

(defn noteo [pc o ap]
  (cond
    ;; don't know the pitch
    (not (l/lvar? ap))
    (let [[pc-calc o-calc] (abs-pitch->pitch ap)]
      (l/and* [(l/== pc pc-calc)
               (l/== o  o-calc)]))

    ;; don't know the absolute pitch
    (not (or (l/lvar? pc) (l/lvar? o)))
    (let [ap-calc (pitch->abs-pitch [pc o])]
      (l/== ap ap-calc))

    ;; don't know the octave or absolute pitch (all versions of a given pitch class)
    (not (l/lvar? pc))
    (l/or*
     (for [oct (range 0 8) :let [ap-calc (pitch->abs-pitch [pc oct])]]
       (l/and* [(l/== ap ap-calc) (l/== o oct)])))

    ;; only know the octave: all pitch classes (and their absolute pitches) in an octave
    (not (l/lvar? o))
    (l/or*
     (for [p-calc (keys pitch-classes) :let [ap-calc (pitch->abs-pitch [p-calc o])]]
       (l/and* [(l/== ap ap-calc) (l/== pc p-calc)])))

    ;; don't know anything: all possible notes
    (and (l/lvar? pc) (l/lvar? o) (l/lvar ap))
    (l/or*
     (for [pitches (keys pitch-classes)
           octaves (range 0 8)
           :let [apc (pitch->abs-pitch [pitches octaves])]]
       (l/and*
        [(l/== ap apc)
         (l/== pc pitches)
         (l/== o octaves)])))
    
    :else
    (l/fail o)))

(comment
  (l/run* [q]
    (noteo :C 5 q)) ;; what's the abs pitch of C5?
  (l/run* [q x]
    (noteo :C q x)) ;; all the Cs possible?
  (l/run* [q x]
    (noteo q x 60)) ;; what's the pitch (pc,o) of 60?
  (l/run* [q x]
    (noteo q 4 x)) ;; all the pitches in an octave
  (l/run* [q]
    (noteo :C q 60)) ;; what's the octave of C 60?
  (l/run 12 [q u x]
    (noteo q u x) ;; all the pitches
    )
  )


(defn intervalo [distance [xpc xo xap] [ypc yo yap]]
  (l/all
   (noteo xpc xo xap)
   (noteo ypc yo yap)
   (l/conde [(fd/- xap yap distance)]
            [(fd/- yap xap distance)])))

(comment
  (l/run* [q u x]
    (l/fresh [a]
      ;; get all notes with which C4 has a major third
      (intervalo (:major-third intervals) [:C 4 a] [q u x]))))

;; all possible triads

(defn major-triado [a b c]
  (l/all
    (intervalo (:major-third intervals) a b)
    (intervalo (:perfect-fifth intervals) a c)))

;; TODO: what about voicings and octaves added?
(defn minor-triado [a b c]
  (l/all
   (intervalo (:minor-third intervals) a b)
   (intervalo (:perfect-fifth intervals) a c)))

(defn major-chordo [r a b c]
  (l/conde
   [(l/== a r) (major-triado a b c)]
   [(l/== c r) ;; first inversion
    (intervalo (:minor-third intervals) a b)
    (intervalo (:minor-sixth intervals) a c)]
   [(l/== b r) ;; second inversion
    (intervalo (:perfect-fourth intervals) a b)
    (intervalo (:major-sixth intervals) a c)]))

(defn chordo [n r a b c]
  (l/conde
   [(l/== n :maj) (major-chordo r a b c)]))


;; all possible inversions

;; all possible chords

(comment (l/run* [q]
           (l/fresh [a b]
             (intervalo (:major-third intervals)
                        [:C, 4, a]
                        [:E, 4, b])))

         (l/run* [q x y]
           (l/fresh [a b x1 x2 x3 y1 y2 y3]
             (l/== x [x1 x2 x3])
             (l/== y [y1 y2 y3])
             (chordo  q
                      [x1 x2 x3]
                      [y1 y2 y3]
                      [:E, 4, a]
                      [:G, 4, b]))) ;; what's a chord where E and G are the 2nd and 3rd notes?

         (l/run* [q x]
           (l/fresh [a b c x1 x2 x3]
             (l/== x [x1 x2 x3])
             (chordo  q
                      x
                      [:E, 4, a]
                      [:G, 4, b] 
                      [:C, 5, c]))) ;; what's the chord where E4, G4 and C5 appear in that order?


         (l/run* [c x y z]
           (l/fresh [c2 c3 x1 x3 y1  y3 z1 z3]
             (l/== x [x1 c2 x3])
             (l/== y [y1 c2 y3])
             (l/== z [z1 c2 z3])
             (l/== c [:C c2 c3])
             (l/conde [(l/== 4 c2)] [(l/== 5 c2)])
             (chordo  :maj
                      c
                      [x1 c2 x3]
                      [y1 c2 y3]
                      [z1 c2 z3]))) ;; what are all the inversions of C major in octaves 4 and 5?

         (l/run* [q]
           (l/fresh [x y] (intervalo q
                                     [:E, 4, x]
                                     [:G, 4, y])))

         (l/run 5 [xp xo xa yp yo ya zp zo za]
           (major-triado [xp xo xa] [yp yo ya] [zp zo za])) ;; get 5 possible major triads
)


