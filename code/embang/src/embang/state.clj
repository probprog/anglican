(ns embang.state)

;;; Running state

;; protocol

(defprotocol anglican-state
  "state operations required to implement 
  the checkpoints"
  (add-weight [state log-weight]
    "add log-weight to the state, after sample or observe")
  (add-predict [state label value]
    "add predict [label value] pair to the list of predicts")
  (in-mem? [state id args]
    "whether there is a memoized value")
  (get-mem [state id args]
    "gets memoized value")
  (set-mem [state id args value]
    "set memoized value"))

;; basic implementation

(defrecord state [log-weight predicts mem]
  anglican-state
  (add-weight 
    [state log-weight]
    (update-in state [:log-weight] + log-weight))

  (add-predict [state label value]
    (update-in state [:predicts] conj [label value]))

  (in-mem? [state id args]
    (and (contains? (:mem state) id)
         (contains? ((:mem state) id) args)))

  (get-mem [state id args]
    (get-in state [id args]))

  (set-mem [state id args value]
    (assoc-in state [id args] value)))
