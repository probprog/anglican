(ns anglican.state
  "Inference state")

;;; Running state

;; Attributes with namespace-qualified fields can be added, and
;; this is the way to extend the state.

(def initial-state
  "initial program state"
  {:log-weight 0.0
   :predicts []
   :result nil
   ::mem {}
   ::store nil})

;; The weight is not read or written by the deterministic
;; computation, and can be maintained outside the state;
;; however, keeping it inside the same state is convenient.

(defn set-log-weight
  "resets the weight to the specified value"
  [state log-weight]
  (assoc state :log-weight log-weight))

(defn add-log-weight
  "add log-weight to the accumulated log-weight
  in the state"
  [state log-weight]
  (update-in state [:log-weight] + log-weight))

(defn get-log-weight
  "get accumulated log-weight"
  [state]
  (state :log-weight))

(defn add-predict
  "add predict label and value to the list of predicts"
  [state label value] ; on predict
  (update-in state [:predicts] conj [label value]))

(defn get-predicts
  "returns collected predicts as an array map"
  [state]
  ;; `array' is to preserve the order on sequential traversal.
  ;; `map' to allow access by key (predict identifier).
  ;; array-map must be created in one shot to preserve the order,
  ;; `into' turns it into a hash-map eventually.
  (apply array-map (apply concat (state :predicts))))

(defn clear-predicts
  "clear predicts"
  [state]
  (update-in state [:predicts] empty))

(defn get-result
  "Returns the result value of the state." 
  [state]
  (:result state))

(defn set-result
  "Sets the result value for the state."
  [state value]
  (assoc state :result value))

;; The following three methods are used by the `mem' form. The
;; memoized values are kept in the state, independently for each
;; particle.

(defn in-mem?
  "true when the function call is memoized"
  [state id args] 
  (contains? ((state ::mem) id) args))

(defn get-mem
  "retrieves memoized function call"
  [state id args]
  (get-in (state ::mem) [id args]))

(defn set-mem
  "stores memoized result of function call,
  returns the update state"
  [state id args value]
  (assoc-in state [::mem id args] value))

;; A probabilistic program has a store. The code can get the 
;; value currently in the store, and update the store by
;; putting a new value into it. 

(defn store
  "updates the store with a new value"
  [state & args]
  (let [keys (butlast args)
        value (last args)]
    (assoc-in state `[::store ~@keys] value)))

(defn retrieve
  "retrieves store contents"
  [state & keys]
  (get-in (state ::store) keys))
