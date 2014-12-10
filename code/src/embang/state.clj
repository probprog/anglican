(ns embang.state)

;;; Running state

;; Attributes with namespace-qualified fields can be added, and
;; this is the way to extend the state.

(def initial-state
  "initial program state"
  {::log-weight 0.0
   ::predicts []
   ::mem {}})

;; The weight is not read or written by the deterministic
;; computation, and can be maintained outside the state;
;; however, keeping it inside the same state is convenient.

(defn set-log-weight
  "resets the weight to the specified value"
  [state log-weight]
  (assoc state ::log-weight log-weight))

(defn add-log-weight
  "add log-weight to the accumulated log-weight
  in the state"
  [state log-weight]
  (update-in state [::log-weight] + log-weight))

(defn get-log-weight
  "get accumulated log-weight"
  [state]
  (state ::log-weight))

(defn add-predict
  "add predict label and value to the list of predicts"
  [state label value] ; on predict
  (update-in state [::predicts] conj [label value]))

(defn get-predicts
  "get collected predicts"
  [state]
  (state ::predicts))

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
