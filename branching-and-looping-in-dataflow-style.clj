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

