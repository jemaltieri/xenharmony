(ns xenharmony.core
  "Functions for harmony calculations"
  [:require
   [clojure.math.numeric-tower :as math]
   [xenharmony.primes :as primes]])


(defn add-monzo-maps
  [& ms]
  (apply merge-with + ms))

(defn negate-map-vals
  [m]
  (into {}
        (map (fn [[k v]] [k (* -1 v)])
             m)))

(defn monzo-map
  [ratio] ;;clojure ratio
  (add-monzo-maps (frequencies (primes/prime-factors (numerator ratio)))
                  (negate-map-vals (frequencies (primes/prime-factors (denominator ratio))))))

(defn map-ext [f ext & seqs] ;;lifted from http://stackoverflow.com/questions/9033678/changing-map-behaviour-in-clojure
  (lazy-seq
   (if (some seq seqs)
     (cons (apply f (map #(if (seq %) (first %) ext) seqs))
           (apply map-ext f ext (map rest seqs)))
     ())))

(defn add-vecs
  [& vs]
  (apply map-ext + 0 vs))

(defn monzo-to-ratio
  [monzo-map]
  (apply * (map (fn [pred] (reduce (fn [acc [base exp]] (* acc
                                                         (math/expt base exp)))
                                  1
                                  (filter (fn [[k v]] (pred v))
                                          monzo-map)))
                [pos? neg?])))

(defn monzo-to-vector
  [monzo-map]
  (map #(if-let [x (monzo-map %)] x 0)
       (primes/primes-up-to (apply max
                                   (keys monzo-map)))))

(defn vector-to-monzo
  [v]
  (into {}
        (map (fn [base exp] [base exp])
             primes/lazy-primes
             v)))
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

(defn tenney-height ;;find another source for equation - this from the wiki doesn't work right
  [monzo]
  (apply + (map (fn [[k v]] (* v (log2 k)))
                monzo)))

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

(def chromatic
  ["C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"])

(defn midi2note
  [m]
  (nth chromatic
       (mod m 12)))

(defn note2midi
  ([n]
     (note2midi n 4))
  ([n oct]
     (+ (* (inc oct) 12)
        (.indexOf chromatic n))))

;;an interval is a function.
;;an edo is a recursive sequence of the generating function
;;a scale is a sequence
