(ns embang.importance
  (:use embang.state
        embang.inference
        [embang.runtime :only [observe]]))

;;; Importance samping

;; Runs every particle independently and outputs the predicts
;; along with their weights. Random choices are sampled
;; from conditional prior distributions.

(derive ::algorithm :embang.inference/algorithm)

(defmethod checkpoint [::algorithm embang.trap.observe] [algorithm obs]
  #((:cont obs) nil (add-log-weight (:state obs)
                                    (observe (:dist obs) (:value obs)))))

(defmethod infer :importance [_ prog & {:keys [number-of-samples output-format]
                                        :or {number-of-samples -1
                                             output-format :clojure}}]
  (binding [*algorithm* ::algorithm]
    (loop [i 0]
      (when-not (= i number-of-samples)
        (print-predicts (:state (exec ::algorithm prog nil initial-state))
                        output-format)
        (recur (inc i))))))
