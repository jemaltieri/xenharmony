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
