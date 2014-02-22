;; All the class in a constructor

(def make
  (fn [& args]
    (apply (first args) (rest args))))


(def Point
  (fn [x y]
    {;; initializing instance variables
     :x x
     :y y
     ;; Metadata
     :__class_symbol__ 'Point
     :__methods__ {
                   :class :__class_symbol__

                   ;; Not implementing getters for 'x' and 'y' yet.

                   :shift
                   (fn [this xinc yinc]
                     (make Point
                           (+ (:x this) xinc)
                           (+ (:y this) yinc)))}}))


((:shift (:__methods__ point)) point -2 -3)

(def send-to
  (fn [object message & args]
    (apply (message (:__methods__ object)) object args)))

(send-to point :shift -2 -3)

;; Change the point constructor to add x and y accessors (getters). Use them in shift.
;; Implement add and have it use shift.
(def Point
  (fn [x y]
    {;; initializing instance variables
     :x x
     :y y
     ;; Metadata
     :__class_symbol__ 'Point
     :__methods__ {
                   :class :__class_symbol__
                   :x
                   (fn [this] x)
                   :y
                   (fn [this] y)
                   :add
                   (fn [this that]
                     (make Point
                           (+ (send-to this :x) (send-to that :x))
                           (+ (send-to this :y) (send-to that :y))))
                   :shift
                   (fn [this xinc yinc]
                     (make Point
                           (+ (send-to this :x) xinc)
                           (+ (send-to this :y) yinc)))}}))

(def point (make Point 1 2))
(send-to point :shift -2 -3)
(send-to point :add point)
