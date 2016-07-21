(ns anglican.inference
  "Inference abstractions"
  (:refer-clojure :exclude [rand rand-nth rand-int])
  (:require [clojure.string :as str]
            [clojure.data.json :as json]
            anglican.trap
            anglican.stat)
  (:use anglican.state
        [anglican.runtime :only [sample* observe*
                                 uniform-continuous
                                 discrete log-sum-exp]]))

;;; Inference multimethod

(defmulti infer
  "main inference procedure, accepts algorithm, program, and
  options; returns a lazy sequence of states"
  (fn [algorithm prog value & _] algorithm))

;;; Checkpoints

(defmulti checkpoint
 "execution checkpoints, processed differently
 depending on the inference algorithm"
 (fn [alg cpt] [alg (type cpt)]))

;; Default checkpoint handling --- return the checkpoint.
(defmethod checkpoint :default [_ cpt] cpt)

;; Fallback method implementations.

(defmethod checkpoint [::algorithm anglican.trap.observe] [_ obs]
  #((:cont obs) nil (add-log-weight (:state obs)
                                    (observe* (:dist obs) (:value obs)))))

(defmethod checkpoint [::algorithm anglican.trap.sample] [_ smp]
  #((:cont smp) (sample* (:dist smp)) (:state smp)))

(defmethod checkpoint [::algorithm anglican.trap.result] [_ res]
  res)

;;; Identifying checkpoints uniquely.

;; Checkpoint identifier is a tuple
;;   [id number-of-previous-occurences]
;; so that different checkpoints and different occurences
;; of the same checkpoint during a single run get different
;; identifiers.

(defn checkpoint-id
  "returns checkpoint identifier and updated state;
  `checkpoint-counts' and `checkpoint-last-id' should be
  keys in the state"
  [cpt state checkpoint-counts checkpoint-last-id]
  (let [checkpoint-id [(:id cpt)
                       ((state checkpoint-counts) (:id cpt) 0)]
        state (-> state
                  (update-in
                    [checkpoint-counts (:id cpt)]
                    ;; If the count is positive but the last
                    ;; sample-id is different, pad the count
                    ;; to decrease the probability of derailing.
                    (fn [count]
                      (inc (cond
                             (nil? count) 0
                             (not= (state checkpoint-last-id)
                                   (:id cpt)) (bit-or count 15)
                             :else count))))
                  (assoc-in [checkpoint-last-id] (:id cpt)))]
    [checkpoint-id state]))

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

;;; Inference prolog and epilog

;; Warmup --- runnning until the first checkpoint.

(defn warmup
  "runs until the first checkpoint and returns
  a continuation that starts with that checkpoint"
  ([prog] (warmup nil))
  ([prog value]
   (let [cpt (exec ::warmup prog value initial-state)]
     (fn [value initial-state]
       (update-in cpt [:state]
                  ;; Predict sequence in state overrides
                  ;; predict sequence in initial-state.
                  (fn [state] (merge initial-state state)))))))

;; Strip-down --- the state is stripped of algorithm-specific
;; entries and contains only predicts and log-weight.

(defn stripdown
  "removes algorithm-specific entries from the state"
  [state]
  (select-keys state [:log-weight :result :predicts]))

;;; Random functions for inference algorithms

;; Random functions in inference algorithm should use
;; the random source as the runtime for consistency.

(let [dist (delay (uniform-continuous 0. 1.))]
  (defn rand
    "Returns a random floating point number
    between 0 (inclusive) and n (default 1) (exclusive)"
    ([] (rand 1.))
    ([n] (* n (sample* @dist)))))

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
  [weights] (sample* (discrete weights)))

;;; Equalizing output

(defn equalize
  "equalizes samples;
  - accepts a lazy sequence of samples;
  - returns a sequence from the same distribution
    with equal weights"
  [samples]
  (letfn [(sample-seq [[fst snd & samples]]
            (lazy-seq
              ;; Metropolis-Hastings
              (let [sample (if (> (- (get-log-weight snd)
                                     (get-log-weight fst))
                                  (Math/log (rand)))
                             snd fst)]
                (cons (set-log-weight sample 0.)
                      (sample-seq (cons sample samples))))))]
    (sample-seq samples)))

(def ^:deprecated collect-by 
  "alias to anglican.stat/collect-by, provided for backwards
  compatibility previous releases" 
  anglican.stat/collect-by)

(defn log-marginal
  "calculates the empirical estimate of the log marginal likelihood
  (i.e. the log mean importance weight)"
  [samples]
  (- (reduce log-sum-exp (sort (map get-log-weight samples)))
     (Math/log (count samples))))

;;; Output

(defmulti print-predict
  "prints a predict, accepts label, value, weight, and
  output format"
  (fn [format _ _ _] format))

(defmethod print-predict :anglican [_ label value log-weight]
  (println (str/join "," (map pr-str [label value log-weight]))))

(defmethod print-predict :clojure [_ label value log-weight]
  (prn [label value log-weight]))

(defmethod print-predict :json [_ label value log-weight]
  (json/write [(str label) value log-weight] *out*)
  (println))

(defmethod print-predict :default [_ label value log-weight]
  (print-predict label value log-weight :anglican))

(defn print-predicts
  "print predicts as returned by a probabilistic program
  in the specified format"
  [format state]
  (let [log-weight (get-log-weight state)]
    (doseq [[name value] (get-predicts state)]
      (print-predict format name value log-weight))))
