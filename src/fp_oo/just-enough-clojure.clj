;;Functional Programming for the OOP programmer
;; JUST ENOUGH CLOJURE

;;Exercise 1. given what you know now, can you define a function second that returns the second element of a list?
(def second
  (fn [col]
    (first (rest col))))

;;Exercise 2. give two implementations of third, which returns the third element of a list.
(def third
  (fn [col]
    (nth col 2)))

(def third
  (fn [col]
    (second (rest col))))

;;Exercise 3. add-squares
(def add-squares
   (fn [& args]
     (if (number? (first args))
       (+
         (* (first args) (first args))
         (apply add-squares (rest args)))
       '0)))

;;Exercise 4. factorial
(def factorial
  (fn [n]
    (apply * (rest (range (+ n 1))))))

;;Exercise 5. fizzbuzz
(def fizzbuzz
  (fn [top-number fizz buzz]
    (remove nil?
      (map
        (fn [n]
          (if (and (zero? (mod n 3)) (zero? (mod n 5)))
            (str fizz buzz)
            (if (zero? (mod n 3))
              buzz
              (if (zero? (mod n 5))
                fizz))))
         (rest (range top-number))))))

;;Exercise 6. prefix-of
;;(prefix-of '(1 2) '(1 2 3)) => true
(def prefix-of
  (fn [acol bcol]
    (if (and (empty? acol) (empty? bcol))
      true
      (and (= (first acol) (first bcol)) (prefix-of (rest acol) (rest bcol))))))

;;Exercise 7. tails
;; (tails '(1 2 3 4 5)) =>  ((1 2 3 4 5) (2 3 4 5) (3 4 5) (4 5) (5) ())
(def tails
  (fn [seq]
  (if (empty? seq)
    (list '())
    (cons seq (tails (rest seq))))))
