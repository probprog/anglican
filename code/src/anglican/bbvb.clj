(ns anglican.bbvb
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:require [anglican.math :refer [isfinite?]]
            [anglican.stat :as stat]
            [anglican.gradients :refer :all]
            [clojure.core.matrix :as m :refer [add sub mul div mmul]])
  (:use [anglican.state :exclude [initial-state]]
        anglican.inference
        [anglican.runtime :only [sample observe log-sum-exp normal]]
        [anglican.smc :exclude [make-initial-state]]))

;;; Black-box variational bayes (BBVB)

(derive ::algorithm :anglican.inference/algorithm)

(defn make-initial-state
  []
  (into anglican.state/initial-state
        {::q-dist (atom {}) ;; variational distribution (proposal) per address
         ::gradient-log-q {}
         ::choice-counts {}
         ::choice-last-id nil}))

;;; Addressing here extends the addressing of LMH.
;;; The LMH addressing is of the form [name count].
;;;
;;; The BBVI addressing additionally includes the type
;;; and dimensionality of the distribution at the sample
;;; point, [name count dist-type dim].

(defmulti dim (fn [dist] (type dist)))

(defmethod dim :default [dist] 1)

(defmethod dim anglican.runtime.discrete-distribution [dist]
  (count (:weights dist)))

(defmethod dim anglican.runtime.dirichlet-distribution [dist]
  (count (:alpha dist)))

(defmethod dim anglican.runtime.mvn-distribution [dist]
  (count (:mean dist)))


(defn get-address
  "returns a unique identifier for sample checkpoint
  and the updated state"
  [smp]
  (let [[choice-id state] (checkpoint-id smp
                                         (:state smp)
                                         ::choice-counts
                                         ::choice-last-id)]
    [(conj choice-id (type (:dist smp)) (dim (:dist smp))) state]))

;;; Helper methods

(defn safe-div
  "elementwise division, but treats 0.0/0.0 = 0.0"
  [numerator denominator]
  (let [eps 1e-100
        mask (m/emap #(if (zero? %) 1.0 0.0) numerator)]
    (div numerator (add (mul mask eps) denominator))))

(defn optimal-scaling
  "given two vectors f=wg and g, estimate Cov(f,g)/Var(g).
  this gives the optimal scaling for the variance reduction term."
  [f g]
  (assert (= (count f) (count g)))
  (if (= (count f) 1)
    0.0
    (let [f-bar (stat/mean f)
          g-bar (stat/mean g)
          g-centered (sub g g-bar)
          numerator (reduce add (mul (sub f f-bar) g-centered))
          denominator (reduce add (mul g-centered g-centered))]
      ;;(println "scaling:" f g)
      ;;(println "f-bar:" f-bar)
      ;;(println "(/ " numerator denominator ")")
      (safe-div numerator denominator))))

(defn proposal-gradient
  "compute the gradient (and appropriate weightings) at each address"
  [gradient-samples raw-weights]
  (loop [weight-map {}
         gradient-map {}
         address-set (reduce clojure.set/union
                             (map #(set (keys %))
                                  gradient-samples))]
    (if (empty? address-set)
      [weight-map gradient-map]
      (let [address (first address-set)
            valid (filter #(and (not (nil? (first %)))
                                (isfinite? (second %)))
                          (map list
                               (map #(get % address) gradient-samples)
                               raw-weights))
            gradient-values (map first valid)
            log-weights (map second valid)
            num-samples (count gradient-values)
            log-of-weight-sum (Math/log num-samples)]
        (if (= num-samples 0)
          (recur weight-map
                 gradient-map
                 (rest address-set))
          (recur (assoc weight-map
                   address (map #(/ % num-samples) log-weights))
                 (assoc gradient-map
                   address gradient-values)
                 (rest address-set)))))))

(defn add-gradient-to-proposal
  "update proposals via gradient step"
  [q-dist gradients weights stepsize use-adagrad]
  (let [rms-prop (number? use-adagrad)
        gamma use-adagrad]
    (reduce conj
            q-dist
            (for [[address [dist adagrad]] q-dist]
              (let [g (get gradients address)]
                (when g
                  (let [w (get weights address)
                        unscaled (map mul w g)
                        a-star (optimal-scaling unscaled g)
                        grad (reduce add (map sub unscaled (map #(mul % a-star) g)))
                        adagrad-update (map #(m/pow % 2) grad)
                        adagrad (if (nil? adagrad)
                                  adagrad-update
                                  (if rms-prop
                                    (add (mul gamma adagrad)
                                         (mul (- 1.0 gamma) adagrad-update))
                                    (add adagrad adagrad-update)))
                        rho (if use-adagrad
                              (map #(div stepsize %)
                                   (map #(add % 1e-100)
                                        (map m/sqrt adagrad)))
                              stepsize)]
                    [address `(~(grad-step dist grad rho) ~adagrad)])))))))

(defn update-proposals!
  [particles stepsize use-adagrad]
  (let [gradient-log-q (map #(::gradient-log-q (:state %)) particles)
        particles (map #(assoc-in % [:state ::gradient-log-q] {}) particles)
        raw-weights (map #(get-log-weight (:state %)) particles)
        [weights gradients] (proposal-gradient gradient-log-q
                                               raw-weights)]
      (swap! (::q-dist (:state (first particles)))
             add-gradient-to-proposal
             gradients
             weights
             stepsize
             use-adagrad)))

(defn get-or-create-q! [state address prior]
  (let [proposals (::q-dist state)]
    (if (contains? @proposals address)
      (get @proposals address)
      (get (swap! proposals assoc address `(~prior nil)) address))))

(defn assoc-gradient [state address proposal value]
  (if (contains? (::gradient-log-q state) address)
    (update-in state [::gradient-log-q address] add ((grad-log proposal) value))
    (assoc-in state [::gradient-log-q address] ((grad-log proposal) value))))

(defn predict-proposals [particles]
  (let [dists @(-> particles first :state ::q-dist)
        proposals (zipmap (keys dists) (map first (vals dists)))]
    (map
     (fn [particle]
       (update-in particle [:state :predicts] #(conj % [:dists proposals])))
     particles)))


;;; Sample and Observe implementations

(defmethod checkpoint [::algorithm anglican.trap.sample] [_ smp]
  (let [[address state] (get-address smp)
        prior (:dist smp)
        ignore (->> prior meta :ignore)]
    (if ignore
      #((:cont smp) (sample prior) state)
      (let [use-mixture (->> prior meta :mog)
            proposal (first (get-or-create-q! state address (:dist smp)))
            value (sample proposal)
            has-gradient (contains? (::gradient-log-q state) address)
            state (assoc-gradient state address proposal value)
            log-p (observe prior value)
            log-weight-increment (if (> log-p (/ -1. 0.))
                                   (- log-p (observe proposal value))
                                   (/ -1. 0.))]
        #((:cont smp) value (add-log-weight state log-weight-increment))))))

(defmethod checkpoint [::algorithm anglican.trap.observe] [_ obs]
  ;; update the weight and return the observation checkpoint
  ;; for possible resampling
  (update-in obs [:state]
             add-log-weight (observe (:dist obs) (:value obs))))

(defmethod infer :bbvb [_ prog value
                        & {:keys [number-of-particles
                                  base-stepsize
                                  adagrad
                                  robbins-monro
                                  resample-predicts]
                           :or {number-of-particles 100
                                base-stepsize 1.0
                                adagrad true
                                robbins-monro 0.0
                                resample-predicts false}}]
  (assert (>= number-of-particles 1)
          ":number-of-particles must be at least 1")
  (let [initial-state (make-initial-state)]
    (letfn
      [(sample-seq
        [steps]
        (lazy-seq
         ;; Run a new sweep.
         (let [next-particles
               (loop [particles (repeatedly
                                 number-of-particles
                                 #(exec ::algorithm prog value initial-state))]
                 (if (every? #(instance? anglican.trap.result %) particles)
                   (do
                     (update-proposals! particles
                                        (/ base-stepsize (Math/pow (inc steps) robbins-monro))
                                        adagrad)
                     (predict-proposals
                      (if resample-predicts
                        (resample particles number-of-particles)
                        particles)))
                   (let [results (filter #(instance? anglican.trap.result %) particles)
                         observes (filter #(instance? anglican.trap.observe %) particles)]
                     (assert (= (count particles)
                                (+ (count results)
                                   (count observes))))
                     (recur (concat results
                                    (map #(exec ::algorithm
                                                (:cont %) nil
                                                (:state %))
                                         observes))))))]
           (concat (map :state next-particles)
                   (sample-seq (inc steps))))))]
      ;; initialize
      (sample-seq 1))))
