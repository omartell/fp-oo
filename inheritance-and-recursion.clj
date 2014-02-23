(def apply-message-to
  (fn [instance message args]
    (let [class (eval (:__class_symbol__ instance))
          method (message (:__instance_methods__ class))]
      (apply method instance args))))

(def make
  (fn [class & args]
    (let [allocated {}
         seeded (assoc allocated
                  :__class_symbol__ (:__own_symbol__ class))]
      (apply-message-to seeded :add-instance-values args))))

(def Point
  {
   :__own_symbol__ 'Point
   :__superclass_symbol__ 'Anything
   :__instance_methods__
   {
    :class-name (fn [this] 'Point)
    :class (fn [this] Point)
    :add-instance-values
    (fn [this x y]
      (assoc this :x x :y y))

    :shift
    (fn [this xinc yinc]
      (make Point
            (+ (:x this) xinc)
            (+ (:y this) yinc)))}})

(def class-from-instance
  (fn [instance]
    (assert (map? instance))
    (eval (:__class_symbol__ instance))))

(def Anything
{
 :__own_symbol__ 'Anything
 :__instance_methods__
 {
  ;; default-constructor
  :add-instance-values identity

  ;;these two methods have been pulled up from Point
  :class-name :__class_symbol__
  :class (fn [this] (class-from-instance this))
  }
})

(def lineage
  (fn [class]
    (let
      [superclass (:__superclass_symbol__ (eval class))]
      (if (nil? superclass)
        (list class)
        (concat
         (lineage superclass)
         (list class))))))

(def class-instance-methods
  (fn [class]
    (:__instance_methods__ (eval class))))

(def maps
  (map class-instance-methods (lineage 'Point)))


(def merged (apply merge maps))

((:shift merged) (make Point 1 2) 100 200)

(def point (make Point 1 1))

(def method-cache
  (fn [class]
    (let [class-symbol (:__own_symbol__ class)
          method-maps (map class-instance-methods
                           (lineage class-symbol))]
      (apply merge method-maps))))

(:shift (method-cache Point))


;; Exercises
;; Exercise 1
;; The factorial function is a classic example of recursion. Factorial can fit our first recursive
;; pattern, where the sequence of descending numbers is the structure to make smaller.
(def factorial
  (fn [n]
    (if (or (= n 1) (= n 0))
      1
      (* n (factorial (- n 1))))))

(factorial 4)

;; Now using the second pattern
(def factorial-1
  (fn [n so-far]
    (if (or (= n 1) (= n 0))
      so-far
      (factorial-1 (- n 1) (* n so-far)))))

(factorial-1 4 1)

;; Use the second pattern to make a recursive function that can add a sequence of numbers.
;; Like this:
;;(recursive-function [1 2 3 4] 0)

(def sum-all
  (fn [numbers so-far]
    (if (empty? numbers)
      so-far
      (sum-all (rest numbers) (+ (first numbers) so-far)))))

(sum-all [1 2 3 4 5 6] 0)

(def multiply-all
  (fn [numbers so-far]
    (if (empty? numbers)
      so-far
      (multiply-all (rest numbers) (* (first numbers) so-far)))))

(multiply-all [1 2 3 4 5 6] 1)

;; Now make a generic function for those operations

(def algebraic-operation
  (fn [operator seed]
   (let [computation
         (fn [so-far numbers]
           (if (empty? numbers)
             so-far
             (recur (operator (first numbers) so-far) (rest numbers))))]
  (partial computation seed))))

(def sum-all
  (algebraic-operation + 0))

(def multiply-all
  (algebraic-operation * 1))

(def substract-all
  (algebraic-operation - 0))

(def divide-all
  (algebraic-operation / 1))

(divide-all [1 2 3 5])

;; Exercise 5
;; Without changing recursive-function, choose starting values for the two wildcard
;; parameters below that will cause it to convert a sequene of keywords into this rather
;; silly map

(def map-with
  (fn [combiner keywords so-far]
    (if (empty? keywords)
      so-far
      (map-with combiner
                (rest keywords)
                (combiner (first keywords) so-far)))))

(map-with
 (fn [kword so-far]
   (assoc so-far kword (count so-far)))
 [:a :b :c]
 {})
