(ns embang.emit
  (:use [embang.xlat :only [program]])
  (:use [embang.trap :only [cps-of-expr result-cont]]))

;;; Code manipulation

(defn anglican->fn
  "converts anglican source code to a
  trampoline-ready clojure function"
  [source]
  `(~'fn [~'_ ~'$state]
     ~(cps-of-expr (program source) `result-cont)))

(defmacro anglican 
  "macro for embedding anglican programs"
  [& source]
  (anglican->fn source))

;;; Running state

;; protocol

(defprotocol anglican-state
  "state operations required to implement 
  the checkpoints"
  (add-weight [state log-weight]
    "add log-weight to the state, after sample or observe")
  (add-predict [state label value]
    "add predict [label value] pair to the list of predicts"))

;; basic implementation

(defrecord state [log-weight predicts]
  anglican-state
  (add-weight 
    [state log-weight]
    (update-in state [:log-weight] + log-weight))

  (add-predict [state label value]
    (update-in state [:predicts] conj [label value])))

;;; Executing a step between checkpoints

(defn run-step
  "a wrapper that runs the program until the
  next checkpoint (one of sample, observe, mem, result)"
  ([prog value state] ; any intermediate step
    (trampoline prog value state))
  ([prog]             ; first step
   (prog nil (map->state {:log-weight 1.
                          :predicts []}))))
