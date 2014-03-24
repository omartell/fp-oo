(ns fp-oo.zipper)

(require '[clojure.zip :as zip])

(def atree
  (zip/seq-zip '(1 (2 3) 4)))

(def afunc
  (zip/seq-zip '(fn [a] * 2 inc a)))

;; This is a handy function for inserting into a flow:
;; (-> zipper zlog zip/right zlog...)
(def zlog
     (fn [z] (println "LOG:" (pr-str (zip/node z))) z))

;; This prints the tree above the current node.
(def zuplog
     (fn [z] (zlog (zip/up z)) z))

;; Write a function similar to flattenize that collects all the
;; vectors in a tree
(def flattenize
     (fn [tree]
       (letfn [(flatten-zipper [so-far zipper]
                 (cond (zip/end? zipper)
                       so-far

                       (zip/branch? zipper)
                       (flatten-zipper so-far (zip/next zipper))

                       :else
                       (flatten-zipper (cons (zip/node zipper) so-far)
                                       (zip/next zipper))))]
         (reverse (flatten-zipper '() (zip/seq-zip tree))))))

(defn all-vectors
  [tree]
  (letfn [(collector [so-far zipper]
            (cond (zip/end? zipper)
                  so-far

                  (vector? (zip/node zipper))
                  (collector (cons (zip/node zipper) so-far) (zip/next zipper))

                  :else
                  (collector so-far (zip/next zipper))))]
    (reverse (collector '() (zip/seq-zip tree)))))

(defn all-vectors-flattenize
  [tree]
  (filter (fn [node] (vector? node)) (flattenize tree)))

(defn first-vector
  [tree]
  (letfn [(collector [zipper]

            (cond (zip/end? zipper)
                  nil

                  (vector? (zip/node zipper))
                  (zip/node zipper)

                  :else
                  (collector (zip/next zipper))))]
    (collector (zip/seq-zip tree))))


;; For the second set of exercises

(def tumult
  (fn [form]
       (letfn [(helper [zipper]
                       (cond (zip/end? zipper)
                             zipper

                             (= (zip/node zipper) '+)
                             (-> zipper
                                 (zip/replace 'PLUS)
                                 zip/next
                                 helper)

                             (and (zip/branch? zipper)
                                  (= (-> zipper zip/down zip/node) '-))
                             (-> zipper
                                 (zip/append-child 55555)
                                 zip/next
                                 helper)

                             (and (zip/branch? zipper)
                                  (= (-> zipper zip/down zip/node) '*))
                             (-> zipper
                                 (zip/replace '(/ 1 (+ 3 (- 0 9999))))
                                 zip/next
                                 helper)

                             (= (zip/node zipper) '/)
                             (-> zipper
                                 zip/right
                                 zip/remove
                                 zip/right
                                 zip/remove
                                 (zip/insert-right (-> zipper zip/right zip/node))
                                 (zip/insert-right (-> zipper zip/right zip/right zip/node))
                                 zip/next
                                 helper)

                             :else
                             (-> zipper zip/next helper)))]
         (-> form zip/seq-zip helper zip/root))))

;;If you’re like me, you understand code better after editing it.
;There’s a lot of duplication in tumult. Factor it out into helper
;functions.
;;I’m rather pleased by my solution, so I suggest you take a look.

(defn branch-root-eq?
  [zipper form]
  (and (zip/branch? zipper)
       (= (-> zipper zip/down zip/node) form)))

(defn refactored-tumult
  [form]
  (letfn [(helper [zipper]
            (let [updated-zipper
                  (cond
                   (= (zip/node zipper) '+)
                   (-> zipper
                       (zip/replace 'PLUS))

                   (branch-root-eq? zipper '-)

                   (-> zipper
                       (zip/append-child 55555))

                   (branch-root-eq? zipper '*)

                   (-> zipper
                       (zip/replace '(/ 1 (+ 3 (- 0 9999)))))

                   (= (zip/node zipper) '/)
                   (-> zipper
                       zip/right
                       zip/remove
                       zip/right
                       zip/remove
                       (zip/insert-right (-> zipper zip/right zip/node))
                       (zip/insert-right (-> zipper zip/right zip/right zip/node)))

                   :else
                   zipper)]
              (if (zip/end? (zip/next updated-zipper))
                updated-zipper
                (helper (zip/next updated-zipper)))))]
    (-> form zip/seq-zip helper zip/root)))

(tumult '(- 3 (+ 6 (+ 3 4) (* 2 1) (/ 8 3))))
