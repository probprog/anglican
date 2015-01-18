(ns embang.map
  (:require [clojure.data.priority-map
             :refer [priority-map-keyfn-by]])
  (:use [embang.state :exclude [initial-state]]
        embang.inference
        [embang.runtime :only [sample observe normal]]))

;;;;; Maximum a Posteriori Estimation through Sampling

;; Uses MCTS and best-first search to find
;; maximum a posteriori estimate of program trace.

(derive ::algorithm :embang.inference/algorithm)

;;;; Particle state

(def initial-state
  "initial state for MAP estimation"
  (into embang.state/initial-state
        {::bandits {}     ; multi-armed bandits
         ::trace []       ; random choices
         ::counts {}      ; counts of occurences of `sample' checkpoints
         ::last-id nil})) ; last sample id

;;;; Bayesian updating, for randomized probability matching

(defprotocol bayesian-belief
  "Bayesian belief"
  (bb-update [belief evidence]
    "updates belief based on the evidence")
  (bb-sample [belief]
    "returns a random sample from the belief distribution")
  (bb-as-prior [belief]
    "returns a belief for use as a prior belief")
  (bb-mode [belief]
    "returns the mode of belief"))

;;;; Mean reward belief

(defn mean-reward-belief
  "returns reification of bayesian belief
  about the mean reward of an arm"
  [sum sum2 cnt]
  ;; Bayesian belief about the mean reward (log-weight).
  ;; Currently, the normal distribution with empirical
  ;; mean and variance is used.
  (let [dist (delay 
               ;; The distribution object is lazy because 
               ;; the parameters are updated many times,
               ;; but the object itself is only used when
               ;; a value is sampled.
               (let [mean (/ sum cnt)
                     sd (Math/sqrt (/ (- (/ sum2 cnt) (* mean mean))
                                      cnt))] ; Var(E(X)) = Var(X)/n
                 (normal mean sd)))]
    (reify bayesian-belief
      (bb-update [mr reward]
        (mean-reward-belief
          (+ sum reward) (+ sum2 (* reward reward)) (+ cnt 1.)))
      (bb-sample [mr] {:pre [(pos? cnt)]}
        (sample @dist))
      (bb-as-prior [mr]
        ;; The current belief is converted to a prior belief
        ;; by setting the sample count to 1 (another small value
        ;; may give better results, and the best value can be 
        ;; allegedly derived; however, this is beyond the scope
        ;; of this research).
        (if (<= cnt 1) mr
          (mean-reward-belief (/ sum cnt) (/ sum2 cnt) 1.)))
      (bb-mode [mr] (/ sum cnt)))))

(def initial-mean-reward-belief
  "uninformative mean reward belief"
  (mean-reward-belief 0. 0. 0.))

;;;; Bandit

(defrecord multiarmed-bandit [arms
                              new-arm-belief
                              new-arm-count
                              new-arm-drawn])

(def fresh-bandit
  "bandit with no arm pulls"
  (map->multiarmed-bandit
   {:arms {}
    :new-arm-belief initial-mean-reward-belief
    :new-arm-count 0}))

;; Selects arms using open randomized probability matching.

(defn select-arm
  "selects an arm with the best core,
  returns the arm value"
  [bandit]
  ;; If the best arm happens to be a new arm,
  ;; return nil. checkpoint [::algorithm sample]
  ;; accounts for this and samples a new value
  ;; from the prior.

  ;; Draw a new arm with a decreasing rate.
  (when (> (rand (:new-arm-count bandit)) 1.)
    ;; Otherwise, select a new arm with the probability
    ;; that a randomly drawn new arm has the highest
    ;; mean reward.
    (loop [arms (:arms bandit)
           best-score (bb-sample (:new-arm-belief bandit))
           best-value nil]
      (if-let [[[value belief] & arms] (seq arms)]
        (let [score (bb-sample belief)]
          (if (>= score best-score)
            (recur arms score value)
            (recur arms best-score best-value)))
        best-value))))

(defn update-bandit
  "updates bandit's belief"
  [bandit value reward]
  (let [bandit (if (:new-arm-drawn bandit)
                 ;; A new arm was drawn, which may or may not
                 ;; coincide with an existing arm.
                 (-> bandit
                     (update-in [:new-arm-belief] bb-update reward)
                     (update-in [:new-arm-count] inc))
                 bandit)]
    ;; Update the belief about the mean reward of the sampled arm.
    (update-in bandit [:arms value]
               (fnil bb-update
                     ;; If the arm is new, derive the belief
                     ;; from the belief about a randomly
                     ;; drawn arm.
                     (bb-as-prior (:new-arm-belief bandit)))
               reward)))

;;; Trace

;; The trace is a vector of tuples
;;   [bandit-id value past-reward]
;; where past reward is the reward accumulated before
;; reaching this random choice. 

(defn record-random-choice
  "records random choice in the state"
  [state bandit-id value past-reward]
  (let [sample-id (first bandit-id)]
    (-> state
        (update-in [::trace] conj [bandit-id value past-reward])
        (update-in [::counts sample-id]
                   ;; If the count is positive but the last sample-id
                   ;; is different, pad the count to decrease
                   ;; the probability of address derailing.
                   (fn [count]
                     (inc (cond
                            (nil? count) 0
                            (not= sample-id
                                  (state ::last-id)) (bit-or count 15)
                            :else count))))
        (assoc-in [::last-id] sample-id))))

;; Different random choices
;; should get different ids, ideally structurally similar
;; random choices should get the same id, just like
;; addresses in Random DB. A bandit id a tuple:
;;   [sample-id number-of-previous-occurences]

(defn bandit-id [smp state]
  "returns bandit id for the checkpoint"
  [(:id smp) ((state ::counts) (:id smp) 0)])

;;;; MAP inference

;;; Building G_prog subgraph 

;; Generating rollouts

(defmethod checkpoint [::algorithm embang.trap.sample] [_ smp]
  (let [state (:state smp)
        bandit-id (bandit-id smp state)
        bandit ((state ::bandits) bandit-id fresh-bandit)

        ;; Select a value as a bandit arm.
        arm (select-arm bandit)
        ;; Remember whether a new arm was drawn;
        ;; new arm belief is updated during back-propagation.
        bandit (assoc bandit :new-arm-drawn (nil? arm))
        ;; Sample a new value if a new arm was drawn.
        value (or arm (sample (:dist smp)))

        ;; Past reward is the reward collected upto
        ;; the current sampling point.
        past-reward (get-log-weight state)
        ;; Edge reward is the probability of the sample
        ;; conditioned on the prefix
        edge-reward (observe (:dist smp) value)
        ;; Update the state:
        state (-> state
                  ;; Increment the log weight by the probability
                  ;; of the sampled value.
                  (add-log-weight edge-reward)
                  ;; Re-insert the bandit, the bandit may be fresh,
                  ;; and the new-arm-drawn flag may have been updated.
                  (assoc-in [::bandits bandit-id] bandit)
                  ;; Insert an entry for the random choice into the trace.
                  (record-random-choice bandit-id value past-reward))]
    ;; Finally, continue the execution.
    #((:cont smp) value state)))

;; Backpropagating rewards

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
                            ;; Bandit arms grow incrementally.
                            update-bandit value (- reward past-reward))))
        (assoc initial-state ::bandits bandits)))))

;;; Best-first search

;; A node is a thunk.  Nodes are inserted into the open list
;; ordered by the distance estimate.  When a node is removed
;; from the open list, it is executed, and then dispatched
;; according to its type --- sample or result.

(defrecord node [comp f])

(def node-key "node ordering key" :f)

(def node-less "node order" <)

;; The open list is a priority queue; all nodes are
;; unique because edge costs are functions of path
;; prefix.

(defrecord open-list [next-key queue])

(def ^:dynamic *beam-width*
  "best-first search beam width"
  nil)

(def empty-open-list
  "empty open list"
  (->open-list 0 (priority-map-keyfn-by node-key node-less)))

(defn refocus-beam
  "if the open list queue length exceeds twice beam-width,
  cut the queue down to *beam-width*"
  [queue]
  (if (and *beam-width*
           (> (count (:queue queue)) (* 2 *beam-width*)))
    (into empty-open-list (take *beam-width* queue))
    queue))

(defn ol-insert
  "inserts node to the open list"
  [ol node]
  (-> ol
      (update-in [:queue] conj  [(:next-key ol) node])
      (update-in [:queue] refocus-beam)
      (update-in [:next-key] inc)))

(defn ol-pop
  "removes first node from the open list,
  returns the node and the list without the node,
  or nil if the open list is empty"
  [ol]
  (when (seq (:queue ol))
    (let [[key node] (peek (:queue ol))
          ol (update-in ol [:queue] pop)]
      [node ol])))

;; On sample, the search continues.
;; On result, a sequence starting with the state
;; and followed by a lazy sequence of states of future
;; found estimates is returned.
;;
;; When the open list is empty, nil is returned.

(derive ::search :embang.inference/algorithm)

(def ^:dynamic *search*
  "atom holding the keyword to dispatch search methods on;
  for comparing different search algorithms"
  ::search)

(defmethod checkpoint [::search embang.trap.sample] [_ smp]
  smp)

(defmulti expand 
  "expands checkpoint nodes"
  (fn [cpt ol] (type cpt)))

(defn next-node
  "pops and advances the next node in the open list"
  [ol]
  #(when-let [[node ol] (ol-pop ol)]
     ;; The result of the computation is either a sample
     ;; or a result node. `expand' is a multimethod that 
     ;; dispatches on the node type.
     (expand ((:comp node)) ol)))

(def ^:dynamic *number-of-h-draws*
  "the number of draws from the belief
  to compute distance heuristic"
  1)
:e
(defmulti distance-heuristic
  "heuristic used by search"
  (fn [smp value belief] *search*))

(defmethod distance-heuristic ::search
  [_ _ belief]
  ;; Number of draws controls the properties of the
  ;; heuristic.
  (cond
    ;;  When the number of draws is positive,
    ;; increasing the number makes heuristic more
    ;; conservative, that is the heuristic approaches
    ;; admissibility.
    (pos? *number-of-h-draws*)
    (- (reduce max (repeatedly *number-of-h-draws*
                               #(bb-sample belief))))

    ;;  When the number is 0, 0. is always
    ;; returned, so that best-first becomes Dijkstra search
    ;; and will always return the optimal solution first
    ;; if the edge costs are non-negative (that is, if
    ;; nodes are discrete, or continuous but the distributions
    ;; are not too steep). 
    (zero? *number-of-h-draws*) 0.

    ;; A negative number of draws triggers
    ;; computing the heuristic as the mode of the belief
    ;; rather than by sampling.
    (neg? *number-of-h-draws*) (bb-mode belief)))

(defmethod expand embang.trap.sample [smp ol]
  ;; A sample node is expanded by inserting all of the
  ;; child nodes into the open list. The code partially
  ;; repeats the code of checkpoint [::algorithm sample].
  (let [state (:state smp)
        id (bandit-id smp state)
        bandit ((state ::bandits) id)
        ol (reduce
             ;; For every child of the latent variable
             ;; in the constructed subgraph of G_prog:
             (fn [ol [value belief]]
               ;; Update the state and the trace.
               (let [past-reward (get-log-weight state)
                     ;; The edge-reward is truncated at 0
                     ;; to avoid divergence.
                     edge-reward (min 0. (observe (:dist smp) value))
                     state (-> state
                               (add-log-weight edge-reward)
                               (record-random-choice id value past-reward))
                     ;; Compute cost estimate till the termination.
                     f (+ (- past-reward)
                          (max (- edge-reward)
                               (distance-heuristic smp value belief)))]
                 ;; If the distance estimate is 
                 ;; a meaningful number, insert the node
                 ;; into the open list.
                 (if (Double/isFinite f)
                   (ol-insert
                     ol (->node
                          #(exec ::search (:cont smp) value state)
                          f))
                   ol)))
             ol (seq (:arms bandit)))]
    ;; Finally, remove and expand the next node 
    ;; from the open list.
    (next-node ol)))

(defmethod expand embang.trap.result [res ol]
  (cons (:state res)                    ; return the first estimate
        (lazy-seq                       ; and a lazy sequence of 
         (trampoline (next-node ol))))) ; future estimates

(defn maximum-a-posteriori
  "returns a sequence of end states
  of maximum a posteriori estimates"
  [prog begin-state]
  (trampoline
   (expand (exec *search* prog nil begin-state)
           empty-open-list)))

;;; Inference method 

;; Putting everything together: building G_prog gradually and
;; searching for MAP estimates on subgraphs.

(defmethod infer :map
  [_ prog & {:keys [number-of-passes  ; times to branch G_prog
                    number-of-samples ; samples before branching
                    number-of-maps    ; MAP estimates per branch
                    number-of-h-draws ; random draws to compute h
                    beam-width        ; search beam width
                    output-format    
                    results           ; a set of :predicts, :trace
                    increasing-maps]  ; consider MAP only if better
             :or {number-of-passes 1
                  number-of-maps 1
                  results #{:predicts :trace}
                  increasing-maps false}}]

  (binding [*number-of-h-draws* (or number-of-h-draws
                                    *number-of-h-draws*)
            *beam-width* (or beam-width
                             *beam-width*)]
    (dotimes [_ number-of-passes]
      ;; Every pass extends G_prog by running a fixed number of samples.
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
                   end-states (maximum-a-posteriori prog begin-state)
                   max-map-weight Double/NEGATIVE_INFINITY]
              (when-not (= imaps number-of-maps)
                (let [[end-state & end-states] end-states]
                  (when end-state  ; Otherwise, all paths were visited.
                    (let [map-weight (Math/exp (get-log-weight end-state))]
                      (if (or (> map-weight max-map-weight)
                              (not increasing-maps))
                        (do
                          (when (contains? results :predicts)
                            (print-predicts end-state output-format))
                          (when (contains? (set results) :trace)
                            ;; Print the trace as a special predict.
                            (print-predict '$trace
                                           (map second (::trace end-state))
                                           map-weight
                                           output-format))
                          (recur (inc imaps) end-states map-weight))
                        (recur imaps end-states max-map-weight)))))))))))))
