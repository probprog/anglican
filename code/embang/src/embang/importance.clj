(ns embang.importance
  (:use embang.state
        [embang.runtime :only [sample observe]]
        [embang.inference :only [print-predicts]]))

(defmulti cpt type)

(defmethod cpt embang.trap.observe [obs]
  #((:cont obs) nil (add-weight (:state obs)
                                (observe (:dist obs) (:value obs)))))

(defmethod cpt embang.trap.sample [smp]
  (let [value (sample (:dist smp))]
    #((:cont smp) value (add-weight (:state smp)
                                    (observe (:dist smp) value)))))

(defmethod cpt embang.trap.result [res]
  (:state res))

(defn exec [prog]
  (loop [step (trampoline prog nil (map->state {:log-weight 1. 
                                                :predicts []
                                                :mem {}}))]
    (let [next (cpt step)]
      (if (fn? next)
        (recur (trampoline next))
        next))))

(def run-inference [prog & {:keys [number-of-samples output-format]
                            :or {number-of-samples -1
                                 output-format :clojure}}]
  (loop [i 0]
    (when-not (= i number-of-samples)
      (print-predicts (exec prog) output-format)
      (loop (inc i)))))
