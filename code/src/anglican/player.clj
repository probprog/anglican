(ns anglican.player
    "Replaying choices through the program"
    (:use [anglican.state :exclude [initial-state]]
          anglican.inference 
          [anglican.runtime only [observe]]))

(derive ::algorithm :anglican.inference/algorithm)

;; Initial state

(def initial-state
  "Initial state for replay"
  (into anglican.state/initial-state
        {::log-prior 0.0
         ::trace []}))

;; Managing log prior, just log weight in the state

(defn set-log-prior
  "rests the prior to the specified value"
  [state log-prior]
  (assoc state ::log-prior log-prior))

(defn add-log-prior
  "add log-prior to the accumulated log-prior
  in the state"
  [state log-prior]
  (update-in state [:log-prior] + log-prior))

(defn get-log-prior
  "get accumulated log-prior"
  [state]
  (state :log-prior))

;; Inference

(defmethod checkpoint [::algorithm anglican.trap.sample] [_ smp]
  (let [[[id val] trace] ((:state smp) ::trace)
        state (assoc state ::trace trace)]
    (assert (= (:id smp) id) "inconsistent replay trace")
    #((:cont smp) nil (add-log-prior state
                                     (observe (:dist smp) val)))))

(defun replay [prog value trace]
  )
