(ns fp-oo.oopsie
  [:use [clojure.algo.monads]])
;;10.3 Branching and Looping in Dataflow Style
;;Exercise 1. Convert this into continuation passing style

(defn odd-size-after-concat?
  []
  (let [a (concat '(a b c) '(d e f))
      b (count a)]
  (odd? b)))

(odd-size-after-concat?)

(defn continuation-odd-size-after-concat?
  []
  (-> (concat '(a b c) '(d e f))
      ((fn [step1]
        (-> (count step1)
            ((fn [step2]
              (odd? step2))))))))

(continuation-odd-size-after-concat?)

;;Exercise 2. This code computes the same value as the previous code.
;;Rewrite into continuation passing style.
(odd? (count (concat '(a b c) '(d e f))))

(-> '(a b c)
    ((fn [step1]
       (-> (concat step1 '(d e))
           ((fn [step2]
              (-> (count step2)
                  ((fn [step3]
                     (odd? step3))))))))))

;; Exercise 3. Rewrite this in continuation passing style.
(-> 3 (+ 2) inc)
(-> 3
    ((fn [step1]
      (-> (+ 2 step1)
          ((fn [step2]
             (inc step2)))))))

;;Exercise 10.12
;;2 is a prime number. All multiples of 2 are not primes (except for 1*2).
;;Here’s how you can use range to find all the non-prime multiples of 2
;;between 4 (the first one) and 100 (inclusive).

;;user=> (range (* 2 2) 101 2)
;;(4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46
;;48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80 82 84 86 88 90 92
;;94 96 98 100)

;;Here’s how you do the same thing with 3:

;;user=> (range (* 3 2) 101 3)
;;(6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72
;;75 78 81 84 87 90 93 96 99)

;;We can do the same for 4. That’s wasted work, since any multiple of
;;4 is already a multiple of 2,but what the heck: your CPU spends
;;almost all of its time waiting for you to give it something to do.
;;user=>( range (* 4 2) 101 4 )

;;Write a function multiples that takes a number and returns a
;;sequence of all its non-prime multiples less than 100.

(defn multiples
  [n]
  (range (* n 2) 101 n))

;; Use the sequence monad or for to find all non primes less than 100.
;; Duplicates are OK.

(def non-primes-less-than-100
  (with-monad sequence-m
    (domonad [n (range 2 11)
              non-primes (multiples n)]
             non-primes)))

;;Use sets to calculate all the primes less than 100.
(def primes-less-than-100
  (let [nprimeset (set non-primes-less-than-100)]
    (remove (fn [n] (nprimeset n)) (range 1 101))))
