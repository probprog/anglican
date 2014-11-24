(ns embang.importance
  (:use [embang.state :only [initial-state]]
        embang.inference))

;;; Importance samping

;; The simplest sampling algorithm, runs every 
;; particle independently and outputs the predicts
;; along with their weights. Random choices are sampled
;; from conditional prior distributions.

(derive ::algorithm :embang.inference/algorithm)

(defn exec [prog]
  (loop [step (trampoline prog nil initial-state)]
    (let [next (checkpoint step ::algorithm)]
      (if (fn? next)
        (recur (trampoline next))
        next))))

(defmethod infer :importance [_ prog & {:keys [number-of-samples output-format]
                                        :or {number-of-samples -1
                                             output-format :clojure}}]
  (loop [i 0]
    (when-not (= i number-of-samples)
      (print-predicts (exec prog) output-format)
      (recur (inc i)))))
