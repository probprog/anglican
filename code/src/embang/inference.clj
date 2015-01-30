(ns embang.inference
  (:refer-clojure :exclude [rand rand-nth rand-int])
  (:require [clojure.string :as str]
            [clojure.data.json :as json]
            embang.trap)
  (:use embang.state
        [embang.runtime :only [sample observe
                               uniform-continuous
                               discrete]]))

;;; Inference multimethod

(defmulti infer
  "main inference procedure, accepts algorithm, program, and
  options; returns a lazy sequence of states"
  (fn [algorithm & _] algorithm))

;;; Checkpoints

(defmulti checkpoint
 "execution checkpoints, processed differently
 depending on the inference algorithm"
 (fn [alg cpt] [alg (type cpt)]))

;; default checkpoint handling --- return the checkpoint

(defmethod checkpoint :default [_ cpt] cpt)

;; fallback method implementations

(defmethod checkpoint [::algorithm embang.trap.observe] [_ obs]
  #((:cont obs) nil (add-log-weight (:state obs)
                                    (observe (:dist obs) (:value obs)))))

(defmethod checkpoint [::algorithm embang.trap.sample] [_ smp]
  #((:cont smp) (sample (:dist smp)) (:state smp)))

(defmethod checkpoint [::algorithm embang.trap.result] [_ res]
  res)

;;; Running a single particle until checkpoint

(defn exec
  "executes the program, calling checkpoint handlers
  at the checkpoints and stopping when the handler
  returns a non-callable value"
  [algorithm prog value state]
  (loop [step (trampoline prog value state)]
    (let [next (checkpoint algorithm step)]
      (if (fn? next)
        (recur (trampoline next))
        next))))

;;; Warmup --- runnning until the first checkpoint

;; All particles will run the same way.

(defn warmup
  "runs until the first checkpoint and returns
  a continuation that starts with that checkpoint"
  [prog]
  (let [cpt (exec ::warmup prog nil initial-state)]
    (fn [value initial-state]
      (update-in cpt [:state]
                 ;; Predict sequence in state overrides
                 ;; predict sequence in initial-state.
                 (fn [state] (merge initial-state state))))))

;;; Random functions for inference algorithms

;; Random functions in inference algorithm should use
;; the random source as the runtime for consistency.

(let [dist (delay (uniform-continuous 0. 1.))]
  (defn rand
    "Returns a random floating point number
    between 0 (inclusive) and n (default 1) (exclusive)"
    ([] (rand 1.))
    ([n] (* n (sample @dist)))))

(defn rand-int
   "Returns a random integer between 0 (inclusive)
   and n (exclusive)"
   [n]
   (int (rand n)))

(defn rand-nth
  "Return a random element of the (sequential) collection.
  Will have the same performance characteristics as nth
  for the given collection."
  [coll]
  (nth coll (rand-int (count coll))))

(defn rand-roulette
  "random roulette selection,
  accepts unnormalized weights"
  [weights] (sample (discrete weights)))

;;; Output

(defmulti print-predict
  "prints a predict, accepts label, value, weight, and
  output format"
  (fn [_ _ _ format] format))

(defmethod print-predict :anglican [label value weight format]
  (println (str/join "," (map pr-str [label value weight]))))

(defmethod print-predict :clojure [label value weight format]
  (prn [label value weight]))

(defmethod print-predict :json [label value weight format]
  (json/write [(str label) value weight] *out*)
  (prn))

(defmethod print-predict :default [label value weight format]
  (print-predict label value weight :anglican))

(defn print-predicts
  "print predicts as returned by a probabilistic program
  in the specified format"
  [state output-format]
  (let [log-weight (get-log-weight state)]
    (doseq [[name value] (get-predicts state)]
      (print-predict name value (Math/exp log-weight) output-format))))
