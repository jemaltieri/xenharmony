(ns xenharmony.primes
  "Helper functions for prime number stuff")

(defn is-prime-from-list?
  [primes cand]
  (not-any? (fn is-divisible? [div]
              (= 0 (rem cand div)))
            primes))

(defn next-prime-from-list
  [primes]
  (if (= 2 (last primes))
    3
    (some #(when (is-prime-from-list? primes %) %)
          (iterate #(+ 2 %) (last primes)))))

(def lazy-primes
  (map last (iterate (fn [plist] (concat plist
                                        [(next-prime-from-list plist)]))
                     [2])))

(defn next-prime
  [n]
  (some #(when (> % n) %)
        lazy-primes))

(defn new-primes-up-to
  [n]
  (take-while #(>= n %)
              lazy-primes))

(defn prime-factors
  ([n]
     (prime-factors n []))
  ([n facs]
     (if (= 1 n)
       facs
       (let [fac (some #(when (= 0 (rem n %)) %)
                       lazy-primes)]
         (recur (quot n fac) (cons fac facs))))))

(defmacro defvar
  "Defines a var with an optional intializer and doc string"
  ([name]
     (list `def name))
  ([name init]
     (list `def name init))
  ([name init doc]
     (list `def (with-meta name (assoc (meta name) :doc doc)) init)))

(defvar contrib-lazy-primes
  (concat
   [2 3 5 7]
   (lazy-seq
    (let [primes-from
	  (fn primes-from [n [f & r]]
	    (if (some #(zero? (rem n %))
		      (take-while #(<= (* % %) n) contrib-lazy-primes))
	      (recur (+ n f) r)
	      (lazy-seq (cons n (primes-from (+ n f) r)))))
	  wheel (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6  4  2
			6 4 6 8 4 2 4 2 4 8 6 4 6 2  4  6
			2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
      (primes-from 11 wheel)))))
