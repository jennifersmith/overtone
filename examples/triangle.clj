(ns overtone.examples.triangle
  (:use overtone.live)
  )

(defcgen triangle-osc [freq phase {:default 0.0} harmonics {:default 40}]
  (:ar  (let
            [
             harmonic-numbers (take harmonics (iterate (partial + 2) 1))
             cosines (set (map #(- (* 4 %) 1) (range 1 harmonics))) ;; every 4n -1 is
             ;; there a better way?!
             ]
          (klang [
                  (map #(* freq %) harmonic-numbers ) ;; harmonics
                  (map #(/ 1.0 (* % %)) harmonic-numbers) ;; inverse square ampl
                  (map #(+ phase %) (map #(if (cosines %) (. Math PI) 0.0  ) harmonic-numbers )) ;; conditional phase shift by pi
                  ])
          ))
  )


(definst tune-2 [freq 440 dur 1.0 vol 1.0]
  (*
   vol
   (let
       [
        lp-cutoff (+ 2000 (* 2000 (/ freq 2000)))
        base (triangle-osc freq :phase (. Math PI ))
        lp (rlpf base lp-cutoff 1.0)
        hp (rhpf lp 200 1.0)]
     hp
     )
   (env-gen (adsr 0.1 0.0 1.0 0.5 ) (line:kr 1.0 0.0 dur) :timeScale dur :action FREE)
   )
  )

(definst organ-cornet [freq 440 dur 1.0 vol 1.0]
  (*
   vol
   (env-gen (asr 0.1 1.0 0.5) (line:kr 1.0 0.0 dur) :timeScale dur :action FREE )
   
   (apply +
    (map 
     #(triangle-osc (* freq %)) (range 1 5)) )
   0.25  
   )
  )
;;(tune-2 300 0.5)
;; (stop)
;; (def freq 1000)
;; (def harmonics 5)
;; (def harmonic-numbers [1 2 3 4 5])
(definst tune [freq 440 harmonics 10 dur 0.5]
  (*
   (triangle-osc freq harmonics)
   (env-gen (perc) :timeScale dur :action FREE)
   )
  )

;; (tune-2 800)
;; (stop)
(defn arpeggio [metro chord]
  (let [
        beat (metro)
        expanded-chord (mapcat #(map + chord (repeat %)) (take 4 (iterate #(+ 12 %) 0)))
        notes
        (map midi->hz
             (map #(+ 48 % ) (concat  expanded-chord (rest ( reverse  (rest expanded-chord))))) )
        ]
    (doall
      (map
       #(at (metro %2)
            (tune-2
             %1 0.5 1.0
             )
            )
       notes
       (iterate #(+ 0.25 %) beat)
       )
      )
    (do
      (apply-at (metro (+ (/ (count notes) 4.0 ) beat) ) #'arpeggio [metro chord] ))
    )
  )

(defn arpeggiolate []
  (let [
        ;;chord (vec (rand-nth (vals CHORD)))
        chord (:M CHORD) 
        metro (metronome 120)]
    (arpeggio metro chord)
    )
  )

;;(arpeggiolate)
;;(stop)

;; some limitations here... 
;;Rapidly play the principal note, the note below it, then return to
;;the principal note for the remaining duration. In much music, the
;;mordent begins on the auxiliary note, and the alternation between
;;the two notes may be extended.
;; Defining 'rapidly' as an 8th of the length!
;; using nested arrays to represent this... 

(defn lower-mordent [principal lower]
  [
   [[
     [ [principal lower]
       principal]
     ]
     :tie
     ]
     :tie
   ]
  )

;; this might be a bit hard. Going for duration of whole interval
(defn glissando [chord]
  
  (fn [offset length]
    (let
        [note-length (/ length (count chord))]
     (map (fn [note offset length]
            [note offset length]
            ) chord
              (iterate #(+ note-length %) offset)
              (iterate #(- % note-length ) length )
              ))
    )
  )
;; intro - beating in quavers
(def toccata-intro-phrase-1
  (let [_ nil]
    [ [:D4 [
         (lower-mordent :v :iv) ;; quaver = 1
         [_ [ :iv :iii]]
         [[:ii :i] :vii#-]
         [:i _]
         [_]]]])
  )
(def toccata-intro-phrase-2
  (let [_ nil]
    [ [:D3  [
               (lower-mordent :v :iv)
               [_ :ii]
               [:iii :vii#-]
               [:i]
               ]]]
    )
  )
;; now 1 beat = crotchet
(def toccata-intro-phrase-3
  (let [_ nil]
    [
     [:D2 [
           :i
           :tie
           :tie
           :tie
           ]]
     [:D3
      [
       (glissando
        :D3
        [
         :vii#-
         :ii
         :iv
         :vi
         :vii#
         :ii+]
        )]]]
    
    ))

;; phrase is a tree representing the notes to be played...
;; this just flattens it into a series of tuples - note, start, dur
;; offset from given time
;; so [:i :v [:i :v]] becomes
;; [[:i 0.0 1.0] [:v 2.0 1.0] [:i 3.0 0.5] [:v 3.5 0.5]]
;; [:i :v :ii] becomes [[:i 0.0 1.0] [:v 1.0 1.0] [:ii [2.0 1.0]]]
;; [:i :v :tie] becomes [[:i 0.0 1.0] [:v 1.0 2.0] etc
;; maybe some tests would be good here... also tests to figure
;; out whether we break with stupidly divided bars
;; where length = 3.0 and offset = 0.0
;; using an assload of recursion
(defmacro dbg [x] `(let [x# ~x] (println '~x "=" x#) x#))
;; awkward
(defn tie [note remaining]
  [
   (first note)
   (second note)
   (+ (last note) (remaining note))
   ]
  )
(defn make-ties [exploded-phrase ]
  (loop
      [
       current-note (first exploded-phrase)
       [next-note & remaining-notes] (rest exploded-phrase)
       result []
       ]
    (if (nil? next-note)
      (conj result current-note)
      (if (= :tie (first next-note))
        (recur
         [
          (first current-note)
          (second current-note)
          (+ (last current-note) (last next-note) )
          ]
         remaining-notes
         result
         )
        (recur
         next-note
         remaining-notes
         (conj result current-note)
         )
        )
      )
   ))
(defn phrase-timings [phrase offset length]
  (make-ties
   (let
       [beat-length (/ length (count phrase))]
     (mapcat
      (fn [note-or-subphrase offset]
        (cond
         (coll? note-or-subphrase)
         (phrase-timings note-or-subphrase offset beat-length)
         (fn? note-or-subphrase)
         (note-or-subphrase offset beat-length)
         :else
          [[note-or-subphrase offset beat-length]])
        )
      phrase
      (iterate #(+ beat-length %) offset )
      )))
  )

(defn phrase-to-playable [phrase offset length scale root]
  (let
      [root (note root)]
      (map
       (fn [[degree start dur]]
         [ 
          (midi->hz (+ root (degree->interval degree scale )) )
          start
          dur
          ])
       (remove #(nil? (first %))
               (phrase-timings phrase offset length))))
  )

(defn phrase-to-playable-2 [phrases offset length ]
  (mapcat
   (fn [[root phrase]]
     (phrase-to-playable phrase offset length :minor root)
     )
   phrases
   )
  )

(def toccata-intro
  (concat
   (phrase-to-playable-2 toccata-intro-phrase-1 0.0 4.0)
   (phrase-to-playable-2 toccata-intro-phrase-1 0.0 4.0)
   ;; faking a paused rest by starting a little later
   (phrase-to-playable-2 toccata-intro-phrase-2 5.0 4.0 )
   (phrase-to-playable-2 toccata-intro-phrase-2 5.0 4.0 )
   ;; another paused rest
   (phrase-to-playable-2 toccata-intro-phrase-1 10.0 4.0 )
   (phrase-to-playable-2 toccata-intro-phrase-1 10.0 4.0 )
   ;; and another longer rest then longer bars. Also got two hands here
   (phrase-to-playable-2  toccata-intro-phrase-3 16.0 8.0 )
   )
  )

(defn play-phrase [phrase metro]
  (let
      [start (metro)]
    (doall
        (map
         (fn [[note offset duration]]
           
           (at (metro (+ offset start))
               (organ-cornet :freq note :dur duration)
               ))
         phrase
         ))
   )
  )

