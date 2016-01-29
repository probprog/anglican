(ns anglican.player
    "Replaying choices through the program"
    (:refer-clojure :exclude [rand rand-int rand-nth])
    (:use [anglican.state :exclude [initial-state]]
          anglican.inference 
          [anglican.runtime :only [observe]]))

(derive ::algorithm :anglican.inference/algorithm)

;;; Initial state

(def initial-state
  "Initial state for replay"
  (into anglican.state/initial-state
        {::log-prior 0.0
         ::trace []}))

;;; Managing log prior, just like log weight

(defn set-log-prior
  "rests the prior to the specified value"
  [state log-prior]
  (assoc state ::log-prior log-prior))

(defn add-log-prior
  "add log-prior to the accumulated log-prior
  in the state"
  [state log-prior]
  (update-in state [::log-prior] + log-prior))

(defn get-log-prior
  "get accumulated log-prior"
  [state]
  (state ::log-prior))

;;; Inference

(defmethod checkpoint [::algorithm anglican.trap.sample] [_ smp]
  (let [state (:state smp)
        [[id val] & trace] (state ::trace)
        state (assoc state ::trace trace)]
    (println id val trace)
    (assert (= (:id smp) id) 
            (format "inconsistent replay trace id %s, should be %s"
                    id (:id smp)))
    #((:cont smp) val (add-log-prior state
                                     (observe (:dist smp) val)))))

;; A trace is a sequence of 2-tuples [sample-id sampled-value]

(defn replay
  "replays trace through the probabilistic program,
  returns a tuple of log-prior and log-weight"
  [prog value trace]
  (let [state (assoc initial-state ::trace trace)
        state (:state (exec ::algorithm prog value state))]
    [(get-log-prior state) (get-log-weight state)]))
