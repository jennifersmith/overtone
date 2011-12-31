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
(def int->degree
  (zipmap (range 1 8) [:i :ii :iii :iv :v :vi :vii] )
  )
(defn transpose [note amount]
  (let
      [new-note (+ amount (degree->int note))]
      (int->degree (mod new-note 7))))
;;Rapidly play the principal note, the note below it, then return to
;;the principal note for the remaining duration. In much music, the
;;mordent begins on the auxiliary note, and the alternation between
;;the two notes may be extended.
;; Defining 'rapidly' as an 8th of the length!
;; using nested arrays to represent this... 

(defn lower-mordent [principal lower]
  [
   [[
     [principal lower]
     principal ;; no more notes means a tie.. i think
     ]
     ]
   ]
  )
;; intro - beating in quavers
(def tocatta-intro-phrase-1
  (let [_ nil]
    [
     (lower-mordent :v :iv) ;; quaver = 1
     [_ [ :iv :iii]]
     [[:ii :i] :vii-]
     [:i _]
     [_]])
  )

;; phrase is a tree representing the notes to be played...
;; this just flattens it into a series of tuples - note, start, dur
;; offset from given time
;; so [:i :v [:i :v]] becomes
;; [[:i 0.0 1.0] [:v 2.0 1.0] [:i 3.0 0.5] [:v 3.5 0.5]]
;; [:i :v :ii] becomes [[:i 0.0 1.0] [:v 1.0 1.0] [:ii [2.0 1.0]]]
;; where length = 3.0 and offset = 0.0
;; using an assload of recursion

(defn phrase-timings [phrase offset length]
  (let
      [beat-length (/ length (count phrase))]
      (map
       (fn [note offset]
         [note offset beat-length]
         )
       phrase
       (iterate #(+ beat-length %) offset )
       ))
  )

(defn play-phrase [phrase scale metro]
  (let
      [start (metro)]
    (at )
   )
  )