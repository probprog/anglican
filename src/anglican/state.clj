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
   ::store nil
   ::catch-stack nil})

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

;; Methods for manipulating the catch stack
(defn get-catch-stack
  "returns the catch stack"
  [state]
  (::catch-stack state))

(defn get-catch-cont
  "returns continuation of a catch"
  [catch]
  (first catch))

(defn get-catch-tag
  "returns tag of a catch"
  [catch]
  (second catch))

(defn empty-catch-stack?
  "check whether catch stack is empty"
  [state]
  (empty? (::catch-stack state)))

(defn push-catch
  "pushes catch continuation and catch tag to the catch stack,
  returns updated state"
  [state cont tag]
  (update state ::catch-stack conj [cont tag]))

(defn pop-catch
  "pops catch from the catch stack. if tag is given, pops catch until (and
  including) a tag is matched;
  returns updated state"
  [state]
  (update state ::catch-stack rest))

(defn peek-catch
  "observes top-most catch of catch stack without removing it. if tag is given,
  returns the top-most catch that matches this tag; returns nil if such catch
  doesn't exist."
  [state]
  (first (::catch-stack state)))

(defn pop-catch-until
  "recursively peeks and pops catch from the catch stack until (pred catch) is
  true,
  returns [catch updated-state] where
  - catch is the first catch for which (pred catch) is true (nil if
  such catch doesn't exist) and
  - updated-state contains the state whose catch stack is popped until (and
  including the case when) (pred catch) is true (updated-state contains an
  empty catch stack if a matching catch doesn't exist)."
  [state pred]
  (loop [state state]
    (if (empty-catch-stack? state)
      [nil state]
      (if (pred (peek-catch state))
        [(peek-catch state) (pop-catch state)]
        (recur (pop-catch state))))))

(defn pop-catch-until-tag
  "recursively peeks and pops catch from the catch stack until either one of
  the tags matches with the catch's tag,
  returns [catch updated-state] where
  - catch is the first catch whose tag matches either one of tags (nil if
  such catch doesn't exist) and
  - updated-state contains the state whose catch stack is popped until (and
  including the case when) catch's tag matches either one of tags
  (updated-state contains an empty catch stack if a matching catch doesn't
  exist)."
  [state & tags]
  (pop-catch-until state #((set tags) (get-catch-tag %))))
