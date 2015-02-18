(ns ctp 
  (require embang.state)
  (use [embang runtime emit]
       ctp-data))

;;; Canadian Traveller Problem

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
(def NINSTANCE "problem instance" :20a)
(def NITER "number of iterations" 100)
(def PRPOL "predict the policy" false)

(def-cps-fn travel [graph s t p-open cost policy]
  ;; All edges are open or blocked with the same probability.
  ;; The results are conditioned on this random choice, hence
  ;; the choice is hidden (*) from the inference algorithm,
  ;; and re-considered at each invocation of travel.
  (let [open? (mem (fn [u v] 
                     (let [is-open (sample* (flip p-open))]
                       (if is-open
                         ;; Keep counts of open and closed
                         ;; explored edges for predicting.
                         (store ::nopen
                                (inc (or (retrieve ::nopen) 0)))
                         (store ::nblocked
                                (inc (or (retrieve ::nblocked) 0))))
                       is-open)))

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
                             ;; Again, sampling is hidden from MH.
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

(defquery ctp "expected path cost" [p-open cost ninstance niter prpol]

  (let [p-open (or p-open P-OPEN)
        cost (or cost COST)
        ninstance (or ninstance NINSTANCE)
        niter (or niter NITER)
        prpol (or prpol PRPOL)]

    (let [instance (get ctp-data ninstance)
          graph (get instance :graph)
          ;; Fix policy for all iterations.  Policy is conditioned on the
          ;; parent node p and current node u.
          policy (mem (fn [u]
                        (let [children (map first (nth graph u))]
                          (map list
                               children
                               ;; This is what we want to learn,
                               ;; expose it to MH.
                               (sample u (dirichlet
                                           (repeat (count children)
                                                   1.)))))))]

      ((fn loop [n sum]
         (if (= n niter)
           ;; Found niter connected items.
           (do
             ;; Policy at every node.
             (when prpol
               (predict-policy graph policy))

             ;; The average distance should decrease with convergence.
             (let [distance (/ sum n)]
               ;; Observe how we liked the journey.
               (observe (flip (exp (- (* cost distance)))) true)
               (predict distance)))

           ;; Continue to next iterations.
           (let [res (travel (get instance :graph)
                             (get instance :s) (get instance :t)
                             p-open cost
                             policy)]
             (if (first res) 
               ;; Connected instance.
               (loop (inc n) (+ sum (second res)))
               ;; Disconnected instance.
               (loop n sum)))))
       0 0.))))
