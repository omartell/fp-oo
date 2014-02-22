;;Moving the Class out of the constructor
;; Make will allocate, seed and call constructor
(def make
  (fn [class & args]
    (let [allocated {}
         seeded (assoc allocated
                  :__class_symbol__ (:__own_symbol__ class))
         constructor (:add-instance-values
                      (:__instance_methods__ class))]
      (apply constructor seeded args))))

;; Class definitions are just maps
(def Point
  {
   :__own_symbol__ 'Point
   :__instance_methods__
   {
    :add-instance-values
    (fn [this x y]
      (assoc this :x x :y y))

    :shift
    (fn [this xinc yinc]
      (make Point
            (+ (:x this) xinc)
            (+ (:y this) yinc)))}})

;; Send-to looks up the class for an instance and evaluates the message
(def send-to
  (fn [instance message & args]
    (let [class (eval (:__class_symbol__ instance))
          method (message (:__instance_methods__ class))]
      (apply method instance args))))

(def point
  (make Point 1 1))

(send-to point :shift 1 1)

;; Exercises
;; Exercise 1
;; The last two steps of make and send-to are very similar. Both look up an instance method in a class,
;; then apply that method to an object and arguments. Extract a common function apply-message-to that
;; takes a class, an instance, a message and a sequence of arguments. Next use apply-message-to within both
;; make and send-to.

(def apply-message-to
  (fn [instance message args]
    (let [class (eval (:__class_symbol__ instance))
          method (message (:__instance_methods__ class))]
      (apply method instance args))))

(apply-message-to point :shift [1 1])

(def send-to
  (fn [instance message & args]
    (apply-message-to instance message args)))

(def make
  (fn [class & args]
    (let [allocated {}
         seeded (assoc allocated
                  :__class_symbol__ (:__own_symbol__ class))]
      (apply-message-to seeded :add-instance-values args))))

(send-to (make Point 1 2) :shift 4 3)

;; Exercise 2
;; Up until now, the :class message has returnted the symbol naming the class. That was OK
;; while an object's class was nothing but a symbol. But now it'd be more approriate to have
;; class-name return the symbol and class return the actual class map. Implement those methods
;; so that this code works.

(def Point
  {
   :__own_symbol__ 'Point
   :__instance_methods__
   {
    :class-name (fn [this] 'Point)
    :class (fn [this] Point)
    :add-instance-values
    (fn [this x y]
      (assoc this :x x :y y))

    :shift2
    (fn [this xinc yinc]
      (make Point
            (+ (:x this) xinc)
            (+ (:y this) yinc)))}})

(def point (make Point 1 2))

(send-to point :class-name)

(send-to point :class)

;; Exercise 3
;; Examine the effect of a redefined class on existing instances. Let's suppose you desire
;; to redefine Point so that it has an instance method that returns the origin.

(def Point
  {
   :__own_symbol__ 'Point
   :__instance_methods__
   {
    :class-name (fn [this] 'Point)
    :class (fn [this] Point)
    :add-instance-values
    (fn [this x y]
      (assoc this :x x :y y))
    :origin (fn [this] (make Point 0 0))}})

(send-to point :origin)

;; Exercise 4
;; Some languages (or development environments) make it easy to define accessor (getter or setter)
;; methods at the same time you define instance variables. Let's do something like that.
(def apply-message-to
  (fn [instance message args]
    (let [class (eval (:__class_symbol__ instance))
          method (or (message (:__instance_methods__ class) (message instance)))]
      (apply method instance args))))

(def Holder
  {
   :__own_symbol__ 'Holder
   :__instance_methods__
   {
    :add-instance-values (fn [this held]
                           (assoc this :held held))}})

(or (:a {:a 2}) (:a {:a 1}))
