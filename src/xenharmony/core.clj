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


(defn tenney-height ;;find another source for equation - this from the wiki doesn't work right
  [monzo]
  (apply + (map (fn [[k v]] (* v (log2 k)))
                monzo)))



;;an interval is a function.
;;an edo is a recursive sequence of the generating function
;;a scale is a lazy sequence using drops

(def mixocycle
  (cycle (map (fn [i] #(+ i %))
              [0 2 4 5 7 9 10])))

(def mixo
  (iterate mixocycle 0))

(defn mixolydian
  ([]
     (mixolydian 0))
  ([n] (concat (map #(+ n %) [0 2 4 5 7 9 10])
               (lazy-seq (mixolydian (+ 12 n))))))
