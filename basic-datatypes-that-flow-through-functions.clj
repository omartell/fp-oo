;; Basic Datatypes that Flow through Functions
;; The problem
;; Your company has decided to have a big day of training. They've secured three
;; instructors, any of whom can teach any of seven courses. Courses last half a day
;; and any given course can be repeated in the afternoon...

(ns fp-oo
  (:use clojure.pprint))

(def answer-annotations
  (fn [courses registrants-courses]
  (let [checking-set (set registrants-courses)]
    (map (fn [course]
           (assoc course
             :spaces-left
             (- (:limit course) (:registered course))
             :already_in?
             (contains? checking-set (:course-name course))))
         courses))))

(answer-annotations [{:course-name "zigging" :limit 4,
                             :registered 3}
                            {:course-name "zagging" :limit 1,
                             :registered 1}]
                           ["zagging"])

(def domain-annotations
  (fn [courses]
    (map (fn [course]
           (assoc course
             :empty? (zero? (:registered course))
             :full? (zero? (:spaces-left course))))
    courses)))


(domain-annotations [{:registered 1 :spaces-left 0}])

(def note-unavailability
  (fn [courses instructor-count]
    (let [out-of-instructors?
          (= instructor-count
             (count
              (filter
               (fn [course] (not (:empty? course)))
               courses)))]
      (map (fn [course]
             (assoc course
                   :unavailable? (or (:full? course)
                                     (and out-of-instructors?
                                          (:empty? course)))))
           courses))))

(note-unavailability [{:empty? true :full? false}] 1)

;; WTF is going on here?
(def annotate
  (fn [courses registrants-courses instructor-count]
        (note-unavailability (domain-annotations
                              (answer-annotations courses registrants-courses))
                             instructor-count)))

;; Arrow Operator!!!
(def annotate
  (fn [courses registrants-courses instructor-count]
    (-> courses
        (answer-annotations registrants-courses)
        domain-annotations
        (note-unavailability instructor-count))))

(annotate [{:course-name "zigging" :limit 4, :registered 3}
           {:course-name "zagging" :limit 3 :registered 1}]
          ["zagging"]
          3)

;; Exercises
;; Exercise 1
;; Use -> to process [1] by removing the number from the vector, incrementing it, and wrapping it in a list.
;; The sequence of values would be this:
;; [1] 1 2 (2)
(->
  [1]
  first
  inc
  list)

;; Exercise 2
;;Add this step to the previous example. After incremitng the value, multiply it by 3, for this sequence of values
;; [1] 1 2 6 (6)
(->
  [1]
  first
  inc
  (* 3)
  list)

;; Exercise 3
;; Here's a function that doubles a number:
(fn [n] (* 2 n))
;; Use that function instead of (* 2) in this chain:N

(-> 3
    (* 2)
    inc)

(-> 3
    ((fn [n] (* 2 n)))
    inc)

;; Exercise 4
;; Convert the following into a three stage computation using ->
(+ (* (+ 1 2) 3) 4)

(->
 (+ 1 2)
 (* 3)
 (+ 4))

(def separate
  (fn [predicate col]
    [(filter predicate col) (remove predicate col)]))

(def visible-courses
  (fn [courses]
    (let [[guaranteed possibles] (separate :already_in? courses)]
      (concat guaranteed (remove :unavailable? possibles)))))

(def final-shape
  (fn [courses]
    (let [desired-keys [:course-name :morning? :registered :spaces-left :already_in?]]
      (map (fn [course]
             (select-keys course desired-keys)) courses))))

(def half-day-solution
  (fn [courses registrants-courses instructor-count]
    (-> courses
        (annotate registrants-courses instructor-count)
        visible-courses
        ((fn [courses] (sort-by :course-name courses)))
        final-shape)))

(def solution
  (fn [courses registrants-courses instructor-count]
    (map (fn [courses]
           (half-day-solution courses registrants-courses instructor-count))
         (separate :morning? courses))))

;;Exercise 1
;; Managers are not allowed to take afternoon courses beacuse that would make them put in too many
;; hours in a day. Implement that restriction.

(def solution
  (fn [courses registrant instructor-count]
    (let [[morning-courses possible-afternoon-courses] (separate :morning? courses)
          afternoon-courses (if (:manager registrant) '() possible-afternoon-courses)]
      (map (fn [courses]
             (half-day-solution courses (:courses registrant) instructor-count))
           [morning-courses afternoon-courses]))))

(solution [{:course-name "zigging"
            :limit 4,
            :registered 3
            :prerequisites ["zagging"]
            :morning? false}
           {:course-name "zagging"
            :limit 1,
            :registered 1
            :prerequisites []
            :morning? false}]
          { :courses ["zagging"]
            :manager true}
          3)

;;Exercise 2
;;Any course can have one or more prerequisite courses. Make it so that a registrant can't see the course if she
;;doesn't have the prerequisites

(def half-day-solution
  (fn [courses registrants-courses instructor-count]
    (-> courses
        (annotate registrants-courses instructor-count)
        visible-courses
        (meeting-prerequisites registrants-courses)
        ((fn [courses] (sort-by :course-name courses)))
        final-shape)))



(def meeting-prerequisites
  (fn [courses registrants-courses]
    (filter (fn [course]
              (contains? registrants-courses (:prerequisites course)))
            courses)))
