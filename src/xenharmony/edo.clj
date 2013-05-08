(ns xenharmony.edo
  "Functions for dealing with equal divisions of an interval"
  [:require
   [xenharmony.primes :as primes]])

(defn nrt
  [x n]
  (Math/pow x (/ 1 n)))

(def st
  (nrt 2 12))

(defn logx
  [base x]
  (/ (Math/log x)
     (Math/log base)))

(defn log2
  [x]
  (logx 2 x))

(defn get-nlimit-edo-patent-val
  [limit edo-steps]
  (map (fn [p] (Math/round (* edo-steps (log2 p))))
       (primes/primes-up-to limit)))

(defn get-errors-from-edo-patent-val
  [patent-val]
  (into {}
        (map (fn [p pv] [p (- pv
                             (* (first patent-val) (log2 p)))])
             primes/lazy-primes
             patent-val)))

(defn interval-to-cents
  [i]
  (* (logx (nrt 2 12) i)
     100))

(defn edo-interval
  [i steps-per-octave]
  (Math/pow 2 (/ i steps-per-octave)))

(defn midi2freq
  [m]
  (* 440 (edo-interval (- m 69) 12)))

(defn freq2midi
  [f]
  (+ 69 (/ (interval-to-cents (/ f 440))
           100)))

(def chromatic-names
  ["C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"])

(defn midi2note
  [m]
  (nth chromatic-names
       (mod m 12)))

(defn note2midi
  ([n]
     (note2midi n 4))
  ([n oct]
     (+ (* (inc oct) 12)
        (.indexOf chromatic-names n))))
