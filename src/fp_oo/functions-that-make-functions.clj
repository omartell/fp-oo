;; Functions that make Functions

;; Exercise 1
;; In a free country, you could add 2 to each element of a sequence like this:

(map (fn [n] (+ 2 n)) [1 2 3])

;; However, ever since the Point-Free-Programmer's Brigade seized control of the government, fn has
;; been banned. How else could you accomplish your goal? Can you think of more than one way?

(map (partial + 2) [1 2 3])

(map + [1 2 3] (repeat 2))

;; Exercise 2
;; juxt turns n functions into a single function that returns a vector whose first element comes from
;; the first function, the second from the second function, and so on.

((juxt empty? reverse count) [:a :b :c])
;;[false (:c :b :a)]

;; In an earlier chapter, we defined separate like this:

(def separate
  (fn [pred sequence]
    [(filter pred sequence) (remove pred sequence)]))

;; Define it using juxt.
(def separate
  (fn [pred sequence]
    ((juxt (partial filter pred) (partial remove pred)) sequence)))

(separate even? [1 2 3])


;; Exercise 3
;; Consider this code´

(def myfun
  (let [x 3]
    (fn [] x)))

(myfun)

;; Exercise 4
;; If let didn't exist, could you use functions to achieve the same effect as in the previous exercise?
;; That is, what code could you wrap around (fn [] x) to produce an x that was bound to no value outside
;; the function and bound to 3 inside it.

(def myfun
  ((fn [x]
     (fn [] x)) 3))

;; Exercise 5
;; Clojure has datatypes that support mutability in the presence of concurrency. For example, an
;; atom is an object protected from overlapping updates by multiple threads. You update an atom by
;; applying a function to its current value.

(def my-atom (atom 0))

(swap! my-atom inc)

(deref my-atom)

(swap! my-atom (fn [x] 33))

;; Exercise 6
;; In the previous exercise, you probably hand-crafted (with fn) a function that returned a constant
;; value. That meant you wrote a function with a parameter that was ignored in favor of the constant.
;; If anything calls for a point free style, this is it.

;; Write a function always that takes a value and produces a function that returns that value no matter
;; what its arguments are. That is:

(def always
  (fn [arg]
    (fn [& ignored] arg)))


((always 8) 1 'a :foo)

;; Exercise 7
;; To practice for this book, I wrote three earlier ones. Here are their ISBNs: 0131774115, 0977716614 and
;; 1934356190 - except that one of them contains a typo. In the next two exercises, you're to find which one.

;; First, use map to write a function check-sum that performs the following calculation on the sequence:
[4 8 9 3 2]
(+ (* 1 4)
   (* 2 8)
   (* 3 9)
   (* 4 3)
   (* 5 2))

(def check-sum
  (fn [& args]
    (reduce +
            (map *
                 args
                 (map inc (range))))))

(check-sum 4 8 9 3 2)

;; Exercise 8
;; A valid ISBN has a check sum that can be divided by 11 to leave a remainder of 0. Write a function
;; isbn? that checks if a stirng is a valid ISBN. You can use (rem N 11) to find the remainder. In the
;; source, sources/higher-order-fucntions.clj, you'll find the three strings, as well as a function,
;; reversed-digits, that converts them into sequences appropriate for check-sum.

(def reversed-digits
     (fn [string]
       (map (fn [digit-char]
              (-> digit-char
                  str
                  Integer.))
            (reverse string))))

(def isbn?
  (fn [string]
    (zero?
     (rem
      (apply
       check-sum
       (reversed-digits string))
      11))))

(def isbn?
  (fn [string]
    (->
     string
     reversed-digits
     ((fn [x] (apply check-sum x)))
     (rem 11)
     zero?)))


(apply check-sum (reversed-digits "0131774115"))

(isbn? "0131774115")
(isbn? "0977716614")
(isbn? "1934356190")

;; Exercise 9
;; Universal Product Codes need a slightly more complicated check-sum. Here's an example of the
;; calculation for [4 8 9 3 2]

(+ (* 1 4)
   (* 3 8)
   (* 1 9)
   (* 3 3)
   (* 1 2))

(def upc-check-sum
  (fn [seqn]
    (reduce +
            (map *
                 seqn
                 (flatten (repeat [1 3]))))))

(upc-check-sum [4 8 9 3 2])


(def upc?
  (fn [string]
    (->
     string
     reversed-digits
     upc-check-sum
     (rem 10)
     zero?)))

(upc? "074182265830")
(upc? "731124100023")
(upc? "722252601404")

;; Exercise 10
;; The same general shape of cuntion will work for checking credit cards, money orders, and so on.
;; Extract the commonality o isbn? and upc? (and their respective check-sums) into a function
;; number-checker that can be used to create either of them. Like this.


(def number-checker
  (fn [checksum-pairs remainder-operator]
    (let [check-sum
          (fn [seqn]
            (reduce +
                    (map *
                         seqn
                         checksum-pairs)))]

      (fn [string]
        (->
         string
         reversed-digits
         check-sum
         (rem remainder-operator)
         zero?)))))

(def upc? (number-checker (flatten (repeat [1 3])) 10))
(def isbn? (number-checker (map inc (range)) 11))
