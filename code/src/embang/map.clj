(ns embang.map
  (:require [embang.colt.distributions :as dist])
  (:require [clojure.data.priority.map
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
    "returns a random sample from the belief distribution"))

;;;; Mean reward belief

(defn mean-reward-belief
  [shape rate]
  ;; Bayesian belief about the mean reward  (log-weight).
  (let [distribution ]
    (reify bayesian-belief
      (bb-update [mr reward]
        )
      (bb-sample [mr]
        ))))

;;;; Bandit

(defrecord multiarmed-bandit [arms count])

(def fresh-bandit
  "bandit with no arm pulls"
  (->multiarmed-bandit {} 0))

;; selects arms using randomized probability matching

(defn best-arm
  "select an arm with the best core"
  [arms best-score best-arm]
  (if-let [[[_ belief :as arm] & arms] (seq arms)]
    (let [score (bb-sample belief)]
      (if (>= score best-score)
        (recur arms score arm)
        (recur arms best-score best-arm)))
    best-arm))

(defn update-arm 
  "updates the belief about arm in arms,
  uses prior-belief for a new arm"
  [arms prior-belief arm reward]
  (update-in arms [arm]
             (fnil bb-update prior-belief) reward))

;;;; MAP inference

;;; Sampling bandit

(defn select-arm
  "select a bandit arm"
  [bandit]
  ;; TODO
  nil)

(defn update-bandit
  "updates bandit's belief"
  [bandit sample reward]
  (-> bandit
      (update-in [:arms] (fnil update-arm fresh-bandit) sample reward)
      (update-in [:count] (fnil inc 0))))

;;; State transformations

(defn backpropagate
  "back propagate reward to bandits"
  [state]
  (let [reward (get-log-weight state)]
    (loop [trace (state ::trace)
           bandits (state ::bandits)]
      (if (seq trace)
        (let [[[id sample past-reward] & trace] trace]
          (recur trace
                 (update-in bandits [id]
                            update-bandit sample
                            (- reward past-reward))))
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

(defn preceding-count
  "number of preceding draws from the same random choice"
  [smp trace]
  (count 
   (filter (fn [[[smp-id]]] (= smp-id (:id smp)))
           trace)))

(defn bandit-id [smp trace]
  "returns bandit id for the checkpoint"
  [(:id smp) (preceding-count smp trace)])

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
        value (or (select-arm bandit) (sample (:dist smp)))

        ;; update the state
        state (-> state
                  ;; add the log-weight of the sample
                  (add-log-weight (observe (:dist smp) value))

                  ;; store the sampled value in the trace
                  (update-in [::trace] conj [id value past-reward]))]

    ;; Finally, continue the execution.
    #((:cont smp) value state)))

;;; Best-first search, passive and functional

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

(defn ol-put
  "adds node to the open list"
  [ol node]
  (-> ol
      (assoc-in [:queue] (:key ol) node)
      (update-in [:key] inc)))

(defn ol-pop
  "removes first node from the open list,
  returns the node and the list without the node,
  or nil if the open list is empty"
  [ol]
  (when (seq (:queue ol))
    [(peek (:queue ol)) (update-in ol [:queue] pop)]))

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
    #(expand @(:comp node) ol)))

(defmethod expand embang.trap.sample [smp ol]
  ;; push children into the open list
  ;; pop node from open list, realize the node
  (next-node ol))

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
                                        output-format]
                                 :or {number-of-passes 1
                                      number-of-maps 1}}]
  (dotimes [_ number-of-passes]
    (loop [isamples 0
           begin-state initial-state]

      ;; After each sample, the final rewards are
      ;; back-propagated to the bandits representing subsets
      ;; of random choices.
      (let [end-state (:state (exec ::algorithm prog nil begin-state))
            begin-state (backpropagate end-state)]
        (if-not (= isamples number-of-samples)
          (recur (inc i) begin-state)

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
