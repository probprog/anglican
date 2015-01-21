(ns embang.importance
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:use embang.state
        embang.inference))

;;; Importance samping

;; Runs every particle independently and outputs the predicts
;; along with their weights. Random choices are sampled
;; from conditional prior distributions.

(derive ::algorithm :embang.inference/algorithm)

(defmethod infer :importance [_ prog & {}]
  (letfn [(sample-seq []
            (lazy-seq
              (cons (:state (exec ::algorithm prog nil initial-state))
                    (sample-seq))))]
            (sample-seq)))
