(ns primes.core
  (:gen-class))

(defn square [n] (* n n))

(defn prime?
  "Given a list of all primes less than x, this function
  returns true if x is prime."
  [known-primes x]
  (if (> x 1)
    (if-let [p (first known-primes)]
      ;; Don't need to check more than p, because square is
      ;; a monotonically increasing function.
      (if (<= (square p) x)
        ;; Cut the search short, once we find out that x is not
        ;; prime.
        (if (zero? (rem x p))
          false
          (prime? (rest known-primes) x))
        true)
      true)
    false))

;; (prime? [] 2)
;; (prime? [2] 3)
;; (prime? [2 3 5 7] 10)
;; (prime? [2 3 5 7 11 13 17 19] 97)

(defn primes
  "Returns a list that contains first n primes"
  [n]
  (if (> n 0)
    (loop [known-primes [2]
           ls           (iterate #(+ 2 %) 3)]
      (if (< (count known-primes) n)
        (if (prime? known-primes (first ls))
          (recur (conj known-primes (first ls)) (rest ls))
          (recur known-primes (rest ls)))
        known-primes))
    []))

;; (primes 0)
;; (primes 1)
;; (primes 10)

(defn print-table [n]
  (let[plist  (into [1] (primes n))
       indent (-> plist last square str count inc)]
    (doseq [i plist]
      (doseq [j plist]
        (if (and (= i 1) (= j 1))
          (print (apply str (repeat indent " ")) )
          (print (format (str "%" indent "d") (* i j)))))
      (println))))


;; (print-table 0)
;; (print-table 1)
;; (print-table 15)

(defn parse-int [s]
  (Integer. (re-find  #"\d+" s )))

(defn -main [& args]
  (if-let [f (first args)]
    (-> f parse-int print-table)
    (print-table 10)))
