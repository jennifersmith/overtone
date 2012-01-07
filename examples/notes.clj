(ns examples.notes)
(defmacro dbg [x] `(let [x# ~x] (println '~x "=" x#) x#))

(defn phrase-note->midi [ root scale note]
  (if (keyword? note)
    (+ root (degree->interval note scale))
    note
   )
  )
(defn phrase
  ([offset length [root scale] phrase-notes] (phrase offset length (map #(phrase-note->midi root scale %) phrase-notes )) )
  ( [offset length phrase-notes]
      (vector
       offset length
       phrase-notes
       )))
(defn chord [& notes]
  (set (map ))
  )
(def phrase-1
  (phrase 
   0.0 4.0 [60 :minor] [
                        :i :iii :v
                        ( { :i :iii :v}
                        )
                        ]
   (defn assoc-parallel-phrase [tune phrase offset length]
     (reduce
      (fn [result note-or-subphrase]
        (assoc-phrase result [ note-or-subphrase] offset length))
      tune
      phrase))))


(defn assoc-phrase [tune phrase offset length]
  (let
      [beat-length (/ length (count phrase))]
    (reduce 
     (fn [result [ note-or-subphrase offset]]
       (cond
        (fn? note-or-subphrase)
        (note-or-subphrase result offset beat-length)
        (set? note-or-subphrase)
        ;; dodgy way to represent parallelism
           (assoc-parallel-phrase result note-or-subphrase offset beat-length)
        (coll? note-or-subphrase)
        (assoc-phrase result note-or-subphrase offset beat-length)
        (nil? note-or-subphrase) result
        :else
        (assoc-in result [offset note-or-subphrase :dur]
                  beat-length ))
       )
     
     tune
     (map vector  phrase
           (iterate #(+ beat-length %) offset )))))


(defn compile-phrases [ & phrases]
  (reduce
   (fn [result [offset length phrase] ]
     (assoc-phrase result phrase offset length)
     )
   {}
   phrases
   )
  )

(defn play-tune [tune metro]
  (let
      [start (metro)]
    (doall
        (map
         (fn [[ offset notes]]
           
           (at (metro (+ offset start))
               (doall
                (map #(organ-cornet (midi->hz (key %)) :dur (:dur (val % ))) notes)
                )
               ))
         tune
         ))
   )
  )


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