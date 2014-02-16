;; Implementing a very simple object system

(def Point
  (fn [x y]
    {:x x :y y :__class_symbol__ 'Point}))

(def class-of :__class_symbol__)

(def x :x)

(def y :y)

(def shift
  (fn [this xinc yinc]
    (Point (+ (x this) xinc) (+ (y this) yinc))))

(def Triangle
  (fn [point1 point2 point3]
    {:point1 point1, :point2 point2, :point3 point3 :__class_symbol__ 'Triangle}))


(def right-triangle (Triangle (Point 0 0)
                              (Point 0 1)
                              (Point 1 0)))

(def equal-right-triangle (Triangle (Point 0 0)
                                    (Point 0 1)
                                    (Point 1 0)))

(def different-triangle (Triangle (Point 0 0)
                                  (Point 0 10)
                                  (Point 10 0)))


;;Exercise 5. Implement add

(def add
  (fn [p1 p2]
    (Point (+ (x p1) (x p2)) (+ (y p1) (y p2)))))

(def add-shift
  (fn [p1 p2]
    (shift p1 (x p2) (y p2))))

;;Exercise 6. Write a function make that builds new objects

(def make
  (fn [& args]
    (apply (first args) (rest args))))

(make Triangle
      (make Point 1 2)
      (make Point 1 2)
      (make Point 1 2))

;;Exercise 3. equal-triangles?

(def equal-triangles?
  (fn [t1 t2]
    (= t1 t2)))

;;Exercise 4. Change equal-triangles? so that it can compare more than two triangles.

(def equal-triangles?
  (fn [& args]
    (apply = args)))

;;Exercise 5. Implement valid-triangle?
;;Different than this one

(def valid-triangle?
  (fn [p1 p2 p3]
    (and (not= p1 p2) (not= p1 p3) (not= p2 p3))))

(def valid-triangle?
  (fn [p1 p2 p3]
    (= (count (distinct [p1 p2 p3])) 3)))

