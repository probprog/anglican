(ns anglican.bbvb
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:require [anglican.stat :refer [mean]]
            [anglican.gradients :refer :all]
            [clojure.core.matrix :as m :refer [add sub mul div mmul]])
  (:use [anglican.state :exclude [initial-state]]
        anglican.inference
        [anglican.runtime :only [sample observe log-sum-exp normal get-tag finite?]]
        anglican.smc))

;;; Black-box variational bayes (BBVB)

(derive ::algorithm :anglican.inference/algorithm)

(defn make-initial-state
  [only]
  (assert (and (not (nil? only)) (> (count only) 0))
          ":only must be a collection of at least one tag (otherwise no inference will be performed)")
  (into anglican.state/initial-state
        {::q-dist (atom {}) ;; {[id count type] (dist grad-squared)}
         ::gradient-log-q {} ;; {[id count type] [grad]}
         ::only (into #{} only)
         ::choice-counts {}
         ::choice-last-id nil}))

;;; Accessor method (public)

(defn get-variational
  "return the learned approximating distributions at each address"
  ;; return format: {id {[count type] dist}}
  ([state]
   (let [dist-list (for [[addr distinfo] @(::variational (meta state))]
                     [(first addr) (rest addr) (first distinfo)])]
     (loop [dist-list dist-list
            learned {}]
       (let [[label id dist] (first dist-list)]
         (if (empty? dist-list)
           learned
           (recur (rest dist-list)
                  (assoc-in learned [label id] dist))))))))

;;; Helper methods

(defn get-address
  "returns a unique identifier for sample checkpoint
  and the updated state"
  [smp]
  (let [[choice-id state] (checkpoint-id smp
                                         (:state smp)
                                         ::choice-counts
                                         ::choice-last-id)]
    [(conj choice-id (type (:dist smp))) state]))

(def ^:dynamic *epsilon-one* 1e-12)
(def ^:dynamic *epsilon-two* 1e-100)

(defn optimal-scaling
  "given two vectors f=wg and g, estimate Cov(f,g)/Var(g).
  this gives the optimal scaling for the variance reduction term."
  [f g]
  (assert (= (count f) (count g)))
  (let [f-bar (mean f)
        g-bar (mean g)
        g-centered (sub g g-bar)
        numerator (reduce add *epsilon-one* (mul (sub f f-bar) g-centered))
        denominator (reduce add *epsilon-one* (mul g-centered g-centered))]
    (div numerator denominator)))

(defn proposal-gradient
  "compute the gradient (and appropriate weightings) at each address"
  [gradient-samples log-weights]
  (loop [weight-map {}
         gradient-map {}
         address-set (reduce clojure.set/union
                             (map #(set (keys %))
                                  gradient-samples))]
    (if (empty? address-set)
      [weight-map gradient-map]
      (let [address (first address-set)
            valid (filter (fn [[grad log-weight]]
                            (and (not (nil? grad))
                                 (finite? log-weight)))
                          (map list
                               (map #(get % address) gradient-samples)
                               log-weights))
            gradient-values (map first valid)
            valid-log-weights (map second valid)
            num-samples (count gradient-values)]
        (if (= num-samples 0)
          (recur weight-map
                 gradient-map
                 (rest address-set))
          (recur (assoc weight-map
                   address (map #(/ % num-samples) valid-log-weights))
                 (assoc gradient-map
                   address gradient-values)
                 (rest address-set)))))))

(defn add-gradient-to-proposal
  "update proposals via gradient step"
  [q-dist gradient-map weights stepsize use-adagrad]
  (let [rms-prop (number? use-adagrad)
        gamma use-adagrad]
    (reduce conj
            q-dist
            (for [[address [dist adagrad]] q-dist]
              (let [g (get gradient-map address)]
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
                                   (map #(add % *epsilon-two*)
                                        (map m/sqrt adagrad)))
                              stepsize)]
                    [address (list (grad-step dist grad rho) adagrad)])))))))

(defn update-proposals!
  [particles stepsize use-adagrad]
  (let [gradient-log-q (map #(::gradient-log-q (:state %)) particles)
        particles (map #(assoc-in % [:state ::gradient-log-q] {}) particles)
        raw-weights (map #(get-log-weight (:state %)) particles)
        [weight-map gradient-map] (proposal-gradient gradient-log-q raw-weights)]
      (swap! (::q-dist (:state (first particles)))
             add-gradient-to-proposal
             gradient-map
             weight-map
             stepsize
             use-adagrad)))

(defn get-or-create-q! [state address prior]
  (let [proposals (::q-dist state)]
    (if (contains? @proposals address)
      (get @proposals address)
      (get (swap! proposals assoc address (list prior nil)) address))))

(defn merge-q!
  "force specific proposal distributions"
  [state proposals]
  (doall
   (for [[addr dist-map] proposals
         [id dist] dist-map]
     (get-or-create-q! state (cons addr id) dist))))

(defn assoc-gradient [state address proposal value]
  (assert (not (contains? (::gradient-log-q state) address))
          (str "[error] updating gradient for existing address" address
               "... which should not be possible. A bug exists."))
  (assoc-in state [::gradient-log-q address] ((grad-log proposal) value)))

(defn ignore? [state dist]
  (not (contains? (::only state) (get-tag dist))))

;;; Sample and Observe implementations

(defmethod checkpoint [::algorithm anglican.trap.sample] [_ smp]
  (let [[address state] (get-address smp)
        ;;_ (when (keyword? (first address)) (println "sampling @" address))
        prior (:dist smp)
        ignore (ignore? state prior)]
    (if ignore
      #((:cont smp) (sample prior) state)
      (let [proposal (first (get-or-create-q! state address (:dist smp)))
            ;;_ (when (keyword? (first address)) (println "dist" prior "->" proposal))
            value (sample proposal)
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

(defmethod checkpoint [::algorithm anglican.trap.result] [_ res]
  ;; include in state metadata a pointer to the learned approximations
  (update-in res [:state] #(vary-meta % assoc ::variational (::q-dist %))))

(defmethod infer :bbvb [_ prog value
                        & {:keys [only
                                  number-of-particles
                                  base-stepsize
                                  adagrad
                                  initial-proposals
                                  robbins-monro]
                           :or {only nil
                                number-of-particles 100
                                base-stepsize 1.0
                                adagrad true
                                initial-proposals nil
                                robbins-monro 0.0}}]
  (assert (>= number-of-particles 1)
          ":number-of-particles must be at least 1")
  (let [initial-state (make-initial-state only)]
    (when initial-proposals
      (merge-q! initial-state initial-proposals))
    (letfn
      [(sample-seq
        [steps]
        (lazy-seq
         ;; Run a new sweep.
         (let [next-particles
               (loop [particles (repeatedly
                                 number-of-particles
                                 #(exec ::algorithm
                                        prog value
                                        initial-state))]
                 (if (every? #(instance? anglican.trap.result %) particles)
                   (do
                     (update-proposals! particles
                                        (/ base-stepsize (Math/pow (inc steps) robbins-monro))
                                        adagrad)
                     particles)
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
