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
             :already-in?
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