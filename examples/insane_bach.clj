(ns examples.insane-bach (:use overtone.live
                          ))

(defcgen triangle-osc [freq phase {:default 0.0} harmonics {:default 40}]
  (:ar  (let
            [
             harmonic-numbers (take harmonics (iterate (partial + 2) 1))
             cosines (set (map #(- (* 4 %) 1) (range 1 harmonics))) ;; every 4n -1 is
             ;; there a better way?!
             ]
          (klang [
                  (map #(* freq %) harmonic-numbers) ;; harmonics
                  (map #(/ 1.0 (* % %)) harmonic-numbers) ;; inverse square ampl
                  (map #(+ phase %) (map #(if (cosines %) (. Math PI) 0.0) harmonic-numbers)) ;; conditional phase shift by pi
                  ]))))

(defcgen organ-env [dur {:default 1.0} vol {:default 1.0}]
  ( :kr
    (* vol
       (env-gen (asr 0.1 1.0 0.5) (line:kr 1.0 0.0 dur) :timeScale dur :action FREE))))

(definst organ-cornet [freq 440 dur 1.0 vol 1.0]
  (*
   (organ-env :dur dur :vol vol)

   (apply +
          (map
           #(triangle-osc (* freq %)) (range 1 5)))
   0.25))

(defn tie [& notes]
  (fn [result offset length]
    (reduce
     (fn [result note] 
       
       (if-let
           [previous-offset
            (first
             
             (filter
              #(get-in result [ % note])
              (sort-by #(- offset %) (keys result)) ))
            ]
         (update-in result [previous-offset note :dur] + length)
         result
         ) )
     result
     notes
     ))
 )
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
     (tie principal)
     ]
     (tie principal)
   ]
  )

;; this might be a bit hard. Going for duration

(defn glissando [chord]
  
  (fn [result offset length]
    (let
        [note-length (/ length (count chord))]
      (reduce
       (fn [result [chord offset length]]
         (assoc-in result [offset chord :dur ] length
                   )
         )
       result
       
       (map vector chord
            (iterate #(+ note-length %) offset)
            (iterate #(- % note-length ) length )
            )))
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
           (tie :i)
           (tie :i)
           (tie :i)
           ]]
     [:D3
      [
       _
       (glissando
        [
         :vii#-
         :ii
         :iv
         :vi
         :vii#
         :ii+]
        )
       (tie
        :vii#-
        :ii
        :iv
        :vi
        :vii#
        :ii+
        )
       #{
         :i+
         :v
         [
          [(tie :iv) :ii ] :iii#]} 
       ]]]
    
    ))
(def toccata-main-theme-1
  (let [_ nil]
    [ [:D3
       [
        [_
         :vii#-]
        [:i :ii :vii#-]
        [:i :ii :vii#-]
        [:i :ii :vii#-]
        [:i :ii]
        [:iii :iv :ii]
        [:iii :iv :ii]
        [:iii :iv :ii]
        [:iii :iv]
        [:v :vi :iv]
        [:v :vi :iv]
        [:v :vi :iv]
        [:v _]
        ]
       ]])
  )
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

;;weeeeeeep
(defn parallel-phrase-timings [result phrase offset length]
  (reduce
   (fn [result note-or-subphrase]
     (dbg (phrase-timings result [ note-or-subphrase] offset length)) ;; differs as does not divide by count
     )
   result
   phrase
   )
  )
(defn phrase-timings [previous phrase offset length]
  
  (let
      [beat-length (/ length (count phrase))]
    (reduce 
     (fn [result [ note-or-subphrase offset]]
       (cond
        (fn? note-or-subphrase)
        (note-or-subphrase result offset beat-length)
        (set? note-or-subphrase)
        ;; dodgy way to represent parallelism
           (parallel-phrase-timings result note-or-subphrase offset beat-length)
        (coll? note-or-subphrase)
        (phrase-timings result note-or-subphrase offset beat-length)
        (nil? note-or-subphrase) result
        :else
        (assoc-in result [offset note-or-subphrase :dur]
                  beat-length ))
       )
     
     previous
     (map vector  phrase
           (iterate #(+ beat-length %) offset ))
     ))
  )



;; coll of tuples of [root phrase] - phrase a seq
(defn phrase-to-playable-with-dictionary [phrase s offset length]
  (reduce
   (fn [result [root phrase]]
     (reduce
      (fn [result [offset notes]]
        (update-in
         result [offset]
         concat
         (map  (fn [[degree attribs]] (merge
                                      {:freq (midi->hz (+ (note root) (degree->interval degree :minor)  ) )}
                                      attribs)) notes)
         )
        )
      result
      (dbg
       (phrase-timings {} phrase offset length))
      )
     
     )
   {}
   phrases)
  )
(def toccata-intro-dictionary
  (merge-with concat
              (phrase-to-playable-with-dictionary toccata-intro-phrase-1 :minor 0.0 4.0)
              (phrase-to-playable-with-dictionary toccata-intro-phrase-2 :minor 4.0 4.0)

              )
  )
(def toccata-intro
  (merge-with concat
   (phrase-to-playable-with-dictionary toccata-intro-phrase-1 0.0 4.0)
   (phrase-to-playable-with-dictionary toccata-intro-phrase-1 0.0 4.0)
   ;; faking a paused rest by starting a little later
   (phrase-to-playable-with-dictionary toccata-intro-phrase-2 5.0 4.0 )
   (phrase-to-playable-with-dictionary toccata-intro-phrase-2 5.0 4.0 )
    ;; another paused rest
   (phrase-to-playable-with-dictionary toccata-intro-phrase-1 10.0 4.0 )
   (phrase-to-playable-with-dictionary toccata-intro-phrase-1 10.0 4.0 )
   ;; and another longer rest then longer bars. Also got two hands here
   (phrase-to-playable-with-dictionary  toccata-intro-phrase-3 16.0 8.0 )
   )
  )

(def toccata-main-1
  (merge-with concat
              (phrase-to-playable-with-dictionary toccata-main-theme-1 0.0 8.0))
  )

(defn play-phrase [phrase metro]
  (let
      [start (metro)]
    (doall
        (map
         (fn [[ offset notes]]
           
           (at (metro (+ offset start))
               (doall
                (map #(organ-cornet (:freq %) (:dur %)) notes)
                )
               ))
         phrase
         ))
   )
  )

