(ns embang.importance
  (:use embang.state
        embang.inference))

;;; Importance samping

;; Runs every particle independently and outputs the predicts
;; along with their weights. Random choices are sampled
;; from conditional prior distributions.

(derive ::algorithm :embang.inference/algorithm)

(defmethod infer :importance [_ prog & {:keys [number-of-samples
                                               output-format]}]
  (loop [i 0]
    (when-not (= i number-of-samples)
      (print-predicts (:state (exec ::algorithm prog nil initial-state))
                      output-format)
      (recur (inc i)))))
