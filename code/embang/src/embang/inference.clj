(ns embang.inference
  (:require [clojure.string :as str]
            [clojure.data.json :as json]
            embang.trap)
  (:use embang.state
        [embang.runtime :only [sample observe]]))

;;; Inference multimethod

(defmulti infer (fn [algorithm & _] algorithm))

;;; Checkpoints

(defmulti checkpoint (fn [alg cpt] [alg (type cpt)]))

;; default method implementations

(defmethod checkpoint [::algorithm embang.trap.observe] [algorithm obs]
  #((:cont obs) nil (add-log-weight (:state obs)
                                    (observe (:dist obs) (:value obs)))))

(defmethod checkpoint [::algorithm embang.trap.sample] [algorithm smp]
  #((:cont smp) (sample (:dist smp)) (:state smp)))

(defmethod checkpoint [::algorithm embang.trap.result] [algorithm res]
  res)

;;; Running a single particle until checkpoint

(defn exec
  "executes the program until a checkpoint"
  [algorithm prog & args]
  (loop [step (apply trampoline prog args)]
    (let [next (checkpoint algorithm step)]
      (if (fn? next)
        (recur (trampoline next))
        next))))

;;; Output

(defmulti print-predict (fn [_ _ _ format] format))

(defmethod print-predict :anglican [label value weight format]
  (println (str/join "," [label value weight])))

(defmethod print-predict :clojure [label value weight format]
  (prn [label value weight]))

(defmethod print-predict :json [label value weight format]
  (json/pprint [(str label) value weight])
  (prn))
  
(defn print-predicts
  "print predicts as returned by a probabilistic program
  in the specified format"
  [state output-format]
  (let [log-weight (get-log-weight state)]
    (doseq [[name value] (get-predicts state)]
      (print-predict name value (Math/exp log-weight) output-format))))
