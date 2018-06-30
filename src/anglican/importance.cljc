(ns anglican.importance
  "Importance sampling"
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:use [anglican.state :only [initial-state]]
        #?(:clj anglican.inference
          :cljs [anglican.inference :only [infer]])))

;;; Importance samping

;; Runs every particle independently and outputs the predicts
;; along with their weights. Random choices are sampled
;; from conditional prior distributions.

(derive ::algorithm :anglican.inference/algorithm)

(defmethod infer :importance [_ prog value & {}]
  (letfn [(sample-seq []
            (lazy-seq
              (cons
                (:state (exec ::algorithm prog value initial-state))
                (sample-seq))))]
    (sample-seq)))
