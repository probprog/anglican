(ns ctp-pp 
  (require embang.state)
  (use [embang runtime emit]
       ctp-data))

;;;; Canadian Traveller Problem

;; The graph is attributed by two probabilities for each
;; edge --- that the edge is open, and that the traveller
;; [dis]likes the edge.
;;
;; The objective is to learn a policy of traversal order
;; that maximizes the probability of arriving alive. 
;;
;; The stochastic policy is represented by vector of
;; probabilities of selecting each edge.

;;; Default values for parameters. 
;; Parameters can be passed via the initial value as
;;   (p-open cost).

(def P-OPEN "probability that the edge is open" 0.5)
(def COST "multiplier for edge costs" 1)
(def NINSTANCE "problem instance" :50a)

(def-cps-fn travel [graph s t p-open policy]
  ;; All edges are open or blocked with the same probability.
  (let [open? (mem (fn [u v] 
                     (sample* (flip p-open))))

        ;; Used to compute the walk distance.
        edge-weight (fn [u v]
                      (some (fn [child]
                               (if (= (first child) v)
                                 (second child)))
                             (nth graph u)))

        ;; Returns true when t is reachable from u.
        ;; Updates the distance to the goal as a side effect,
        ;; via observing edges.
        dfs (fn dfs [u t]
              (if (= u t)
                [true 0.]
                ((fn loop [policy passed]
                   ;; On every step of the loop, filter visited
                   ;; edges from the policy.
                   (let [policy
                         (filter
                           (fn [choice]
                             (not (contains?
                                    (retrieve ::visited)
                                    (sort [u (first choice)]))))
                           policy)]

                     (if (empty? policy)
                       [false passed]
                       (let [dist (categorical policy)
                             ;; We implement here stochastic policy
                             ;; and do not want to learn the best
                             ;; path, but rather to win on average.
                             v (sample* dist)]
                         ;; Search for the goal in the subtree.
                         (store ::visited
                                (conj (retrieve ::visited)
                                      (sort [u v])))
                         (let [res (dfs v t)
                               passed (+ passed
                                         (edge-weight u v)
                                         (second res))]
                           (if (first res)
                             ;; Goal found in the subtree.
                             [true passed]
                             ;; Continue the search in another
                             ;; subtree.
                             (loop  
                               policy
                               ;; Add the weight of the edge
                               ;; through which we return.
                               (+ passed (edge-weight v u)))))))))

                 ;; Initialize policy for the node by transition
                 ;; weights for all open edges.
                 (filter
                   (fn [choice]
                     (open? u (first choice)))
                   (policy u))

                 ;; Start with zero passed distance.
                 0.)))]

    (store ::visited (set ()))
    (dfs s t)))

(def-cps-fn predict-policy
  "predict the memoized policy"
  [graph policy]
  ((fn for-nodes [nodes]
     (when (seq nodes)
       (let [u (first nodes)]
         ((fn for-choices [choices]
            (when (seq choices)
              (predict (list 'T u (first (first choices)))
                       (second (first choices)))
              (for-choices (rest choices))))
          (policy u)))
       (for-nodes (rest nodes))))
   (range (count graph))))

(defquery ctp-pp
  "predicting policy for CTP"
  [p-open cost ninstance] 

  (let [p-open (or p-open P-OPEN)
        cost (or cost COST)
        ninstance (or ninstance NINSTANCE)]

    (let [instance (get ctp-data ninstance)
          graph (get instance :graph)
          s (get instance :s)
          t (get instance :t)

          ;; Fix policy in every node.
          transitions 
          (reduce
            (fn [transitions u]
              (assoc transitions u
                     (let [children (map first (nth graph u))]
                       ;; Build argument for the categorical distribution:
                       ;; selection probability for each child.
                       (map list
                            children
                            (sample (dirichlet
                                      (repeat (count children)
                                              1.)))))))
            {} (range (count graph)))

          ;; Policy function for the agent.
          policy (fn [u] (get transitions u))

          ;; Compute the predictions
          result (travel graph s t p-open policy)
          connected (first result)
          distance (second result)]

      ;; Reject disconnected items.
      (observe (flip 1.0) connected)
      ;; Observe how the agent liked the journey.
      (observe (flip (exp (- (* cost distance)))) true)
      (predict distance)
      (predict-policy graph policy))))
