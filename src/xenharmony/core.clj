(ns xenharmony.core
  "Functions for harmony calculations"
  [:require [clojure.math.numeric-tower :as math]])



(defn fac-map ;; can just use (frequencies (primefactors 42))
  [n]
  (reduce (fn [m fac] (if (not (contains? m fac))
                       (assoc m fac 1)
                       (update-in m [fac] inc)))
          {}
          (primefactors n)))

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
  (add-monzo-maps (frequencies (primefactors (numerator ratio)))
                  (negate-map-vals (frequencies (primefactors (denominator ratio))))))

(defn map-ext [f ext & seqs] ;;lifted from http://stackoverflow.com/questions/9033678/changing-map-behaviour-in-clojure
  (lazy-seq
   (if (some seq seqs)
     (cons (apply f (map #(if (seq %) (first %) ext) seqs))
           (apply map-ext f ext (map rest seqs)))
     ())))

(defn add-vecs
  [& vs]
  (apply map-ext + 0 vs))


(defrecord Ratio [numerator denominator])

(defrecord Monzo [vector])

(defprotocol IJust
  "A protocol defining a JI harmonic relationship between two pitches"
  (to-monzo [this])
  (to-ratio [this])
  (mult [this multratio]))

(extend-type Ratio
  IJust
  (to-monzo [this]
    (Monzo. (apply map-ext - 0 (map (fn [facs] (map (fn [p] (count (filter #(= p %)
                                                                         facs)))
                                                   (primes-below (inc (apply max facs)))))
                                    (map primefactors
                                         [(.numerator this) (.denominator this)])))))
  (to-ratio [this] this)
  (mult [this multratio] (Ratio. (* (.numerator this) (.numerator multratio))
                                 (* (.denominator this) (.denominator multratio)))))

(extend-type Monzo
  IJust
  (to-monzo [this] this)
  (to-ratio [this] this) ;;todo
  (mult [this multratio] this)) ;;todo
