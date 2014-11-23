(ns embang.state)

;;; Running state

(defrecord state [log-weight predicts mem])

(def initial-state
  "initial program state"
  (map->state {:log-weight 0.0
               :predicts []
               :mem {}}))

;; The weight is not read or written by the 
;; deterministic computation, and can be
;; maintained outside the state; however,
;; keeping it inside the same state is convenient.

(defn set-weight
  "resets the weight to the specified value"
  [state log-weight]
  (assoc-in state [:log-weight] log-weight))

(defn add-weight
  [state log-weight]
  (update-in state [:log-weight] + log-weight))

(defn add-predict
  "add predict label and value to the list of predicts"
  [state label value] ; on predict
  (update-in state [:predicts] conj [label value]))

;; The following three methods are used by the `mem'
;; form. The memoized values are kept in the state,
;; independently for each particle.

(defn in-mem?
  "true when the function call is memoized"
  [state id args] 
  (and (contains? (:mem state) id)
       (contains? ((:mem state) id) args)))

(defn get-mem
  "retrieves memoized function call"
  [state id args]
  (get-in state [id args]))

(defn set-mem
  "stores memoized result of function call"
  [state id args value]
  (assoc-in state [id args] value))
