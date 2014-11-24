(ns embang.importance
  (:use [embang.state :only [initial-state]]
        embang.inference))

;;; Importance samping

;; Runs every particle independently and outputs the predicts
;; along with their weights. Random choices are sampled
;; from conditional prior distributions.

(derive ::algorithm :embang.inference/algorithm)

(defmethod infer :importance [_ prog & {:keys [number-of-samples output-format]
                                        :or {number-of-samples -1
                                             output-format :clojure}}]
  (loop [i 0]
    (when-not (= i number-of-samples)
      (print-predicts (exec prog ::algorithm) output-format)
      (recur (inc i)))))
