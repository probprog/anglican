(ns embang.map
  (:require [embang.colt.distributions :as dist])
  (:require [clojure.data.priority-map
             :refer [priority-map-keyfn-by]])
  (:use [embang.state :exclude [initial-state]]
        [embang.runtime :only [sample observe]]
        embang.inference))

;;;;; Maximum a Posteriori Estimation through Sampling

;; Uses MCTS and best-first search to find maximum a
;; posteriori estimate of program trace.

(derive ::algorithm :embang.inference/algorithm)

;;;; Particle state

(def initial-state
  "initial state for MAP estimation"
  (into embang.state/initial-state
        {::bandits {}
         ::trace []}))

;;;; Bayesian updating, for randomized probability matching

(defprotocol bayesian-belief
  "Bayesian belief"
  (bb-update [belief evidence]
    "updates belief based on the evidence")
  (bb-sample [belief]
    "returns a random sample from the belief distribution")
  (bb-as-prior [belief]
    "returns a belief for use as a prior belief"))

;;;; Mean reward belief

(defn mean-reward-belief
  [sum sum2 count]
  ;; Bayesian belief about the mean reward (log-weight)
  (let [dist (delay 
              (let [mean (/ sum count)
                    sd (Math/sqrt (/ (- (/ sum2 count) (* mean mean))
                                     count))] ; Var(E(X)) = Var(X)/n
                (dist/normal-distribution mean sd)))]
    (reify bayesian-belief
      (bb-update [mr reward]
        (mean-reward-belief
         (+ sum reward) (+ sum2 (* reward reward)) (+ count 1)))
      (bb-sample [mr] {:pre [(pos? count)]}
        (dist/draw @dist))
      (bb-as-prior [mr]
        (mean-reward-belief sum sum2 1)))))

(def initial-mean-reward-belief
  "uninformative mean reward belief"
  (mean-reward-belief 0 0 0))

;;;; Bandit

(defrecord multiarmed-bandit [arms new-arm-belief count])

(def fresh-bandit
  "bandit with no arm pulls"
  (->multiarmed-bandit {} initial-mean-reward-belief 0))

;; selects arms using randomized probability matching

(defn select-arm
  "selects an arm with the best core,
  returns the arm value"
  [bandit]
  (loop [arms (:arms bandit)
         best-score (bb-sample (:new-arm-belief bandit))
         best-value nil]
    (if-let [[[value belief] & arms] (seq arms)]
      (let [score (bb-sample belief)]
        (if (>= score best-score)
          (recur arms score value)
          (recur arms best-score best-value)))
      best-value)))

(defn update-bandit
  "updates bandit's belief"
  [bandit value reward]
  (-> bandit
      (update-in [:arms value]
                 (fnil bb-update
                       (bb-as-prior (:new-arm-belief bandit)))
                 reward)
      (update-in [:new-arm-belief] bb-update reward)
      (update-in [:count] (fnil inc 0))))

;;;; MAP inference

;;; State transformations

(defn backpropagate
  "back propagate reward to bandits"
  [state]
  (let [reward (get-log-weight state)]
    (loop [trace (state ::trace)
           bandits (state ::bandits)]
      (if (seq trace)
        (let [[[id value past-reward] & trace] trace]
          (recur trace
                 (update-in bandits [id]
                            (fnil update-bandit fresh-bandit)
                            value (- reward past-reward))))
        (assoc initial-state
          ::bandits bandits)))))

;;; Trace

;; The trace is a vector of tuples
;;   [bandit-id value past-reward]
;; where past reward is the reward accumulated 
;; before reaching this random choice.

;; Bandit id: different random choices should get different
;; ids, ideally structurally similar random choices should
;; get the same id, just like addresses in Random DB

(defn preceding-occurences
  "number of preceding occurences of the same
  random choice in the trace"
  [smp trace]
  (count 
   (filter (fn [[[smp-id]]] (= smp-id (:id smp)))
           trace)))

(defn bandit-id [smp trace]
  "returns bandit id for the checkpoint"
  [(:id smp) (preceding-occurences smp trace)])

;;; Building G_prog subgraph 

(defmethod checkpoint [::algorithm embang.trap.sample] [_ smp]
  (let [state (:state smp)
        id (bandit-id smp (state ::trace))
        bandit ((state ::bandits) id)
        ;; Past reward is the reward collected by the particle
        ;; until the checkpoint. To make rewards collected by
        ;; arms commensurate, past-reward is subtracted from
        ;; the final reward.
        past-reward (get-log-weight state)
        ;; select a value
        value (or (and bandit (select-arm bandit))
                  (sample (:dist smp)))
        ;; update the state
        state (-> state
                  ;; add the log-weight of the sample
                  (add-log-weight (observe (:dist smp) value))
                  ;; store the sampled value in the trace
                  (update-in [::trace] conj [id value past-reward]))]
    ;; Finally, continue the execution.
    #((:cont smp) value state)))

;;; Best-first search

;; A node is a delayed computation.  Nodes are inserted
;; into the open list ordered by the distance estimate.
;; When a node is removed from the open list, it is
;; forced, and then dispatched according to its type,
;; sample or result.

(defrecord node [comp f g])

(def node-key 
  "node ordering key"
  (juxt :f :g))

(defn node-less
  "node order"
  [[fa ga] [fb gb]]
  (or (< fa fb) (= fa fb) (> ga gb)))

;; The open list is a priority queue; all nodes are
;; unique because edge costs are functions of path
;; prefix.

(defrecord open-list [key queue])

(def empty-open-list
  "empty open list"
  (->open-list 0 (priority-map-keyfn-by node-key node-less)))

(defn ol-insert
  "inserts node to the open list"
  [ol node]
  (-> ol
      (assoc-in [:queue (:key ol)] node)
      (update-in [:key] inc)))

(defn ol-pop
  "removes first node from the open list,
  returns the node and the list without the node,
  or nil if the open list is empty"
  [ol]
  (when (seq (:queue ol))
    (let [res [(peek (:queue ol)) (update-in ol [:queue] pop)]]
      (prn "res" res)
      (prn "ol" ol)
      (prn "(pop (:queue ol))" (pop (:queue ol)))
      res)))

;; On sample, the search continues.
;; On result, a sequence starting with the state
;; and followed by a lazy sequence of states of future
;; found estimates is returned.
;;
;; When the open list is empty, nil is returned.

(derive ::search :embang.inference/algorithm)

(defmethod checkpoint [::search embang.trap.sample] [_ smp]
  smp)

(defmulti expand 
  "expands checkpoint nodes"
  (fn [cpt ol] (type cpt)))

(defn next-node
  "pops and advances the next node in the open list"
  [ol]
  (when-let [[node ol] (ol-pop ol)]
    #(expand ((:comp node)) ol)))

(def number-of-h-draws
  "atom containing the number of draws from
  the belief to compute distance heuristic"
  (atom 1))

(defn distance-heuristic
  "returns distance heuristic given belief"
  [belief]
  (let [h (- (reduce max
                     (repeatedly @number-of-h-draws
                                 #(bb-sample belief))))]
    (if (Double/isNaN h) 0 h)))
        

(defmethod expand embang.trap.sample [smp ol]
  (let [state (:state smp)
        id (bandit-id smp (state ::trace))
        bandit ((state ::bandits) id)
        ol (reduce (fn [ol [value belief]]
                     (let [c (max 0. (- (observe (:dist smp) value)))
                           h (max 0. (- (distance-heuristic belief) c))
                           g (+ (- (get-log-weight state)) c)
                           f (+ g h)]
                       (ol-insert ol (->node #((:cont smp)
                                               value
                                               (-> state
                                                   (add-log-weight c)))
                                             f g))))
                   ol (seq (:arms bandit)))]
    (next-node ol)))

(defmethod expand embang.trap.result [res ol]
  (cons (:state res)                    ; return the first estimate
        (lazy-seq (next-node ol))))     ; and a lazy sequence of 
                                        ; future estimates
(defn maximum-a-posteriori
  "returns a sequence of end states
  of maximum a posteriori estimates"
  [prog begin-state]
  (trampoline
   (expand (exec ::search prog nil begin-state)
           empty-open-list)))

(defmethod infer :map [_ prog & {:keys [number-of-passes
                                        number-of-samples
                                        number-of-maps
                                        number-of-h-draws
                                        output-format]
                                 :or {number-of-passes 1
                                      number-of-maps 1}}]

  ;; allows to change number-of-h-draws from the command line;
  ;; useful for experimenting
  (when number-of-h-draws
    (swap! embang.map/number-of-h-draws (fn [_] number-of-h-draws)))
    
  (dotimes [_ number-of-passes]
    (loop [isamples 0
           begin-state initial-state]

      ;; After each sample, the final rewards are
      ;; back-propagated to the bandits representing subsets
      ;; of random choices.
      (let [end-state (:state (exec ::algorithm prog nil begin-state))
            begin-state (backpropagate end-state)]
        (if-not (= isamples number-of-samples)
          (recur (inc isamples) begin-state)

          ;; The program graph is ready for MAP search.
          ;; Consume the sequence of end-states of MAP
          ;; estimates and print the predicts.
          (loop [imaps 0
                 end-states (maximum-a-posteriori prog begin-state)]
            (when-not (= imaps number-of-maps)
              (let [[end-state end-states] end-state]
                (when end-state  ; Otherwise, all paths were visited.
                  (print-predicts end-state output-format)
                  (recur (inc imaps) end-states))))))))))
