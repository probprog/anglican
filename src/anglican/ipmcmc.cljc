(ns anglican.ipmcmc
  "Interacting particle Markov chain Monte Carlo
   Options:
     :number-of-particles (2 by default)
       - Number of particles per sweep
     :number-of-nodes (32 by default)
       - Number of nodes running SMC and CSMC.
     :number-of-csmc-nodes (nil by default)
       - Number of nodes running as CSMC.  Must be between
         1 and (- :number-of-nodes 1). Defaults to
         (/ :number-of-nodes 2) when not specified.
     :all-particles? (true by default)
       - Return all particles, instead of 1 particle per sweep
         on each node.  Note that even when :all-particles? is
         false, particles are still weighted due to Rao-
         Blackwellization of the Gibbs updates for the CSMC indices.
     :pool (:builtin by default)
       - Threadpool argument for pmap operation over nodes.
         Defaults to creating a pool of containing (+ (ncpus) 2)
         threads. See com.climate.claypoole/pmap for further info."
  (:require [clojure.core.matrix :as mat]
            #?(:clj [com.climate.claypoole :as cp])
            [anglican.inference :refer [infer exec]]
            [anglican.pgibbs :as pgibbs
             :refer [initial-state release-retained-state
                     retained-initial-state]]
            [anglican.runtime :refer [sample* discrete]]
            [anglican.smc :as smc]
            [anglican.state :refer [set-log-weight]]))

(derive ::algorithm :anglican.pgibbs/algorithm)

(defn sweep
  "Performs a sequential Monte Carlo or conditional sequential
  Monte Carlo sweep. Returns a pair [results log-Z] in which results
  is a sequence of result records and log-Z is an estimate of the
  log marginal likelihood."
  [algorithm prog value
   number-of-particles all-particles?
   retained-state]
  (loop [checkpoints
         (if retained-state
           ;; perform first step of CSMC
           (conj
            (repeatedly (- number-of-particles 1)
                        #(exec algorithm prog value initial-state))
            (exec algorithm prog value retained-state))
           ;; perform first step of SMC
           (repeatedly number-of-particles
                        #(exec algorithm prog value initial-state)))
         log-Z 0.0]
    (cond
      (every? #(instance? anglican.trap.observe %) checkpoints)
      (let [resampled (if retained-state
                        ;; perform CSMC resampling step
                        (conj (smc/resample
                               (conj (rest checkpoints)
                                     (update-in (first checkpoints)
                                                [:state]
                                                release-retained-state))
                               (- number-of-particles 1))
                              (first checkpoints))
                        ;; perform SMC resampling
                        (smc/resample checkpoints
                                      number-of-particles))
            log-mean-weight (get-in (second resampled)
                                    [:state :log-weight])]
        (recur (map #(exec algorithm (:cont %) nil
                           ;; Set weights of all particles (including
                           ;; the retained one) to the same value.
                           (set-log-weight (:state %) 0.))
                    resampled)
               (+ log-Z log-mean-weight)))
      (every? #(instance? anglican.trap.result %) checkpoints)
      (if all-particles?
        ;; return all particles
        [checkpoints log-Z]
        ;; select one particle at random
        [(list (rand-nth checkpoints)) log-Z])
      :else (throw (AssertionError.
                    "some `observe' directives are not global")))))

(defn norm-exp
  "Normalized exponential. Accepts a collection of log weights.
  Returns a pair [ps log-Z] in which ps is a sequence of
  normalized probabilities and log-Z is the log mean weight.
  If all weights are -infinity then they are assumed to be
  equal."
  [log-weights]
  (let [max-log-weight (reduce max log-weights)]
    (if (= (/ -1. 0.) max-log-weight)
      (let [M (count log-weights)]
        [(repeat M (/ 1 M)) (/ -1. 0.)])
      (let [weights (map #(Math/exp (- % max-log-weight))
                         log-weights)
            total-weight (reduce + weights)
            probabilities (map #(/ % total-weight) weights)
            log-mean-weight (+ (Math/log (/ total-weight
                                            (count log-weights)))
                               max-log-weight)]
        [probabilities log-mean-weight]))))

(defn gibbs-update-csmc-indices
  "Performs a Gibbs sweep on the indices of conditional nodes by
  sampling each index conditioned on the values of the other indices.

  Returns a pair [csmc-indices zeta-sums] in which the csmc-indices is a
  vector of indices for newly selected conditional nodes and zeta-sums is
  a vector of weights for each node (needed when returning all
  particles)."
  [log-Zs number-of-csmc-nodes]
  (loop [i 0
         csmc-indices (vec (range number-of-csmc-nodes))
         smc-indices (vec (range number-of-csmc-nodes
                                 (count log-Zs)))
         zeta-sums (vec (repeat (count log-Zs) 0.0))]
    (if (= i number-of-csmc-nodes)
      [csmc-indices zeta-sums]
      (let [proposal-indices (conj smc-indices i)
            [ps _] (norm-exp (map log-Zs proposal-indices))
            zeta-sums (reduce (fn [zs [i p]]
                            (assoc zs i (+ (zs i) p)))
                            zeta-sums
                            (map vector proposal-indices ps))
            k (sample* (discrete ps))]
        (if (= k (count smc-indices))
          (recur (inc i)
                 csmc-indices
                 smc-indices
                 zeta-sums)
          (recur (inc i)
                 (assoc csmc-indices i
                        (smc-indices k))
                 (assoc smc-indices k
                        (csmc-indices i))
                 zeta-sums))))))

(defmethod infer :ipmcmc
  [_ prog value
   & {:keys [number-of-particles
             number-of-nodes
             number-of-csmc-nodes
             all-particles?
             pool]
      :or {number-of-particles 2
           number-of-nodes 32
           pool :builtin
           all-particles? true}}]
  (assert (> number-of-particles 1)
          ":number-of-particles must be larger than 1")
  (assert (> number-of-nodes 1)
          ":number-of-nodes must be larger 1")
  (let [number-of-csmc-nodes
          (or number-of-csmc-nodes
              (/ number-of-nodes 2))
        number-of-smc-nodes (- number-of-nodes
                               number-of-csmc-nodes)]
    (assert (< number-of-csmc-nodes
               number-of-nodes)
            (str ":number-of-csmc-nodes must be smaller "
                 "than :number-of-nodes"))
    (letfn [(sample-seq
              [retained-states]
              (lazy-seq
               (let [;; run CSMC and SMC sweeps
                     task (partial sweep ::algorithm prog value
                                   number-of-particles all-particles?)
                     sweeps (#?(:clj cp/pmap :cljs map)
                             pool task
                             (concat retained-states
                                     (repeat number-of-smc-nodes
                                             nil)))
                     resultss (mapv first sweeps)
                     log-Zs (mapv second sweeps)
                     ;; sample indices for new CSMC nodes
                     [cs zeta-sums] (gibbs-update-csmc-indices
                                 log-Zs number-of-csmc-nodes)
                     ;; construct new retained states
                     retained-results (map rand-nth (map resultss cs))
                     retained-states (map retained-initial-state
                                          retained-results)
                     ;; get states for samples
                     states (mapcat
                             (fn [results zeta-sum]
                               (let [log-weight
                                     (Math/log
                                      (/ zeta-sum
                                         number-of-particles))]
                                 (map
                                  #(set-log-weight % log-weight)
                                  (map :state results))))
                             resultss
                             zeta-sums)]
                 ;; continue to next sweep
                 (concat states
                         (sample-seq retained-states)))))]
      ;; initialize by running SMC for all nodes
      (sample-seq (repeat number-of-csmc-nodes nil)))))

