(ns embang.importance
  (:use embang.state
        [embang.runtime :only [sample observe]]))

(defmulti checkpoint type)

(defmethod checkpoint embang.trap.observe [obs]
  #((:cont obs) nil (add-weight (:state obs)
                                (observe (:dist obs) (:value obs)))))

(defmethod checkpoint embang.trap.sample [smp]
  (let [value (sample (:dist smp))]
    #((:cont smp) value (add-weight (:state smp)
                                    (observe (:dist smp) value)))))

(defmethod checkpoint embang.trap.result [res]
  (:state res))

(defn run-particle [prog]
  (loop [step (trampoline prog nil (map->state {:log-weight 1. 
                                                :predicts []
                                                :mem {}}))]
    (let [next (checkpoint step)]
      (if (fn? next)
        (recur (trampoline next))
        next))))
