(ns embang.map
  (:require [embang.colt.distributions :as dist])
  (:use [embang.state :exclude [initial-state]]
        embang.inference))

;;;; Maximum a Posteriori Estimation Through Sampling

;; Uses MCTS to find maximum a posteriori estimate of
;; program trace

;; Unlike inference sampling algorithms, has two
;; execution modes:
;;  - the exploration mode is used to discover 
;;    the most probable choices
;;  - the selection mode is used to simulate
;;    a trace with the highest MAP estimate

(derive ::explore :embang.inference/algorithm)
(derive ::select :embang.inference/algorithm)

;;; Particle state

(def initial-state
  "initial state for MAP estimation"
  (into embang.state/initial-state
        {}))

(defn maximum-a-posteriori
  "given the state, returns a vector of
  maximum a posteriori samples"
  [])

;;; Bayesian updating, for randomized probability matching

(defprotocol bayesian-belief
  "Bayesian belief"
  (bb-update [belief evidence]
    "updates belief based on the evidence")
  (bb-sample [belief]
    "returns a random sample from the belief distribution"))

;;; Mean reward belief, via Gamma distribution

(defn mean-reward-belief
  [shape rate]
  ;; We use log-Gamma belief for log-rewards and impose 
  ;; Gamma prior on the rate of the reward distribution.
  (let [distribution (dist/gamma-distribution shape rate)]
    (reify bayesian-belief
      (bb-update [mr reward]
        (mean-reward-belief (+ shape 1.) (+ rate (Math/exp reward))))
      (bb-sample [mr]
        ;; Since samples from the mean reward distribution
        ;; are used for comparison, the absolute value of
        ;; the shape of the reward distribution does not matter.
        (Math/log (/ (dist/draw distribution)))))))

(def initial-mean-reward-belief
  "initial belief about mean reward" 
  ;; Uninformative prior --- we know nothing about
  ;; the absolute values of rewards in general.
  (mean-reward-belief Double/MIN_NORMAL Double/MIN_NORMAL))

;;; Bandit

;; selects arms using randomized probability matching


(defn best-bet-arm
  "select an arm with the best bet"
  ([arms] (best-bet-arm arms Double/NEGATIVE_INFINITY nil))
  ([arms best-bet best-arm]
   (if-let [[[sample belief :as arm] & arms] (seq arms)]
     (let [bet (bb-sample belief)]
       (if (>= bet best-bet)
         (recur arms bet arm)
         (recur arms best-bet best-arm)))
     best-arm)))

(defn select-arm
  "returns the sample of an existing arm
  or generates a new sample"
  [arms select-new-arm]
  (if (seq arms)
    ;; First, select an arm which belief is used
    ;; as the new arm belief:
    (let [[_ belief] (best-bet-arm arms)
          new-arm-bet (bb-sample belief)]
      ;; Then, select an arm with the best bet;
      ;; if the bet of the arm selected above is the best
      ;; add a new arm.
      (if-let [[sample _] (best-bet-arm arms new-arm-bet nil)]
        sample
        (select-new-arm)))
    (select-new-arm)))

(defn update-arm 
  "updates the belief about arm in arms"
  [arms arm reward]
  (update-in arms [arm]
             (fnil bb-update initial-mean-reward-belief) reward))

;;; MAP inference

(defmethod infer :map [_ prog & {:keys [number-of-samples
                                        output-format]
                                 :or {output-format :clojure}}]
  (loop [i 0
         state initial-state]
    (if-not (= i number-of-samples)
      (recur (inc i) (:state (exec ::explore prog nil state)))
      (let [state (exec ::select prog nil state)]
        (print-predicts state output-format)
        ;; return a vector of MAP sample choices
        (maximum-a-posteriori state)))))
