(ns embang.smc
  (:use [embang.state :only [initial-state]]
        embang.inference))

;;; SMC

(derive ::algorithm :embang.inference/algorithm)

(defmethod checkpoint [embang.trap.observe ::algorithm] [obs algorithm]
  (update-in obs [:state]
             add-log-weight (observe (:dist obs) (:value obs)))

(defmethod infer :smc [_ prog & {:keys [number-of-samples output-format]
                                 :or {number-of-samples -1
                                      output-format :clojure}}]
  (loop [i 0]
    (when-not (= i number-of-samples)
      (print-predicts (exec prog ::algorithm) output-format)
      (recur (inc i)))))
