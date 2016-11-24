(ns primes.core-test
  (:require [clojure.test :refer :all]
            [primes.core :refer :all]))

(deftest test-prime?
  (testing "primality"
    (are [x] x
      (prime? [] 2)
      (prime? [2] 3)
      (not (prime? [] 1))
      (not (prime? [2 3 5 7] 10)) 
      (prime? [2 3 5 7 11 13 17 19] 97))))


(deftest test-primes
  (testing "primes list"
    (are [x y] (= x y)
      (primes 1) [2]
      (primes 0) []
      (primes 2) [2 3]
      (count (primes 100)) 100)))

