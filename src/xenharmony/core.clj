(ns xenharmony.core)

(defn filter-multiples
  [coll fac]
  (filter #(or (== % fac)
               (pos? (rem % fac)))
          coll))

(defn sieve-of-e
  ([coll]
     (sieve-of-e coll 2))
  ([coll cur]
     (if (> cur (/ (apply max coll) 2))
       coll
       (let [newcoll (filter-multiples coll cur)]
         (recur newcoll (nth newcoll (inc (.indexOf newcoll cur))) )))))

(defn primes-below
  [n]
  (sieve-of-e (range 2 n)))

(defn primefactors ;;lifted from the interweb: http://sudoreadme.blogspot.com/2012/03/prime-numbers-kata-in-clojure.html
  ([n] (primefactors n 2))
  ([n candidate]
    (cond (<= n 1) (list)
          (= 0 (rem n candidate)) (conj (primefactors (/ n candidate)) candidate)
          :else (primefactors n (inc candidate)))))

(defn fac-map
  [n]
  (reduce (fn [m fac] (if (not (contains? m fac))
                       (assoc m fac 1)
                       (update-in m [fac] inc)))
          {}
          (primefactors n)))

(defn map-ext [f ext & seqs] ;;lifted from http://stackoverflow.com/questions/9033678/changing-map-behaviour-in-clojure
  (lazy-seq
   (if (some seq seqs)
     (cons (apply f (map #(if (seq %) (first %) ext) seqs))
           (apply map-ext f ext (map rest seqs)))
     ())))

(defn add-vecs
  [& vs]
  (apply map-ext + 0 vs))


(deftype Ratio [numerator denominator]
  (comment  (numerator [this] numerator)
            (denominator [this] denominator)
            (to-number [this] (/ numerator denominator))))

(deftype Monzo [monzovec]
  (comment
    (to-vector [this] monzovec)))

(defprotocol IJust
  "A protocol defining a JI harmonic relationship between two pitches"
  (to-monzo [this])
  (to-ratio [this]))

(extend-type Ratio
  IJust
  (to-monzo [this]
    (apply map-ext - 0 (map (fn [facs] (map (fn [p] (count (filter #(= p %)
                                                                 facs)))
                                           (primes-below (inc (apply max facs)))))
                            (map primefactors
                                 [numerator denominator]))))
  (to-ratio [this] this))

(extend-type Monzo
  IJust
  (to-monzo [this] this)
  (to-ratio [this] this))