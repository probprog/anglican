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

(defmethod checkpoint embang.trap.mem [mem]
  (if (in-mem? (:state mem) (:id mem) (:args mem))
    #((:cont mem) (get-mem (:state mem) (:id mem) (:args mem)))
    #(apply (:proc mem) 
            (fn [value $state]
              ((:cont mem) value (set-mem (:state mem)
                                          (:id mem) (:args mem) value)))
            (:state mem)
            (:args mem))))

(defmethod checkpoint embang.trap.result [res]
  (:state res))

(defn run-particle [prog]
  (loop [next (trampoline prog nil (map->state :log-weight 1. 
                                               :predicts []
                                               :mem {}))]
    (if (fn? next)
      (recur (checkpoint (trampoline next)))
      next)))
