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
(def INSTANCE "problem instance" 20)

(defun travel (graph s t p-open cost)
  ;; All edges are open or blocked with the same probability.
  ;; The results are conditioned on this random choice, hence
  ;; the choice is hidden (*) from the inference algorithm.
  (let ((open? (mem (lambda (u v) 
                      (let ((is-open (sample* (flip p-open))))
                        (if is-open
                          ;; Keep counts of open and closed
                          ;; explored edges for predicting.
                          (store ::nopen (inc (or (retrieve ::nopen) 0)))
                          (store ::nblocked (inc (or (retrieve ::nblocked) 0))))

                        is-open))))

        ;; Policy is conditioned on the parent node p and
        ;; current node u.
        (policy (mem (lambda (p u)
                       (let ((children (map first (nth graph u))))
                         (map list
                              children
                              ;; This is what we want to learn,
                              ;; expose it to MH.
                              (sample (dirichlet
                                        (repeat (count children)
                                                1.))))))))

        ;; Probability distribution that the traveller
        ;; `likes' the edge.
        (likes (lambda (u v)
                 (loop ((children (nth graph u)))
                   (let ((child (first children)))
                     (if (= (first child) v)
                       ;; As the cost of travelling goes up,
                       ;; it makes more sense to invest into
                       ;; learning the policy.
                       (flip (exp (- (* cost (second child)))))
                       (begin (assert (seq (rest children)))
                              (recur (rest children))))))))

        ;; Returns true when t is reachable from u.
        ;; Updates the distance to the goal as a side effect,
        ;; via observing edges.
        (dfs (lambda (p u t)
               (or (= u t)
                   ;; Initialize policy for the node by transition
                   ;; weights for all open edges.
                   (loop ((policy (filter
                                    (lambda (choice)
                                      (open? u (first choice)))
                                    (policy p u))))

                     ;; On every step of the loop, filter visited
                     ;; edges from the policy.
                     (let ((policy (filter
                                     (lambda (choice)
                                       (not (contains?
                                              (retrieve ::visited)
                                              (sort (list u (first choice))))))
                                     policy)))

                       (and (seq policy)
                            (let ((dist (categorical policy))
                                  ;; We implement here stochastic policy
                                  ;; and do not want to learn the best
                                  ;; path, but rather to win on average.
                                  ;; Again, sampling is hidden from MH.
                                  (v (sample* dist)))
                              ;; left through [u v]
                              (store ::visited
                                     (conj (retrieve ::visited)
                                           (sort (list u v))))
                              ;; Observe the node. The probability
                              ;; that we like the node decreases
                              ;; with node weight.
                              (observe (likes u v) true) 
                              (or (dfs u v t)
                                  (begin
                                    ;; came back through [v u]
                                    (observe (likes v u)  true)
                                    (recur policy)))))))))))
    (store ::visited (set ()))
    (let ((res (dfs nil s t)))

      ;;; Debugging predicts.
      ;; Policy at start node.
      (loop ((s-trans (policy nil s)))
        (if (seq s-trans)
          (begin
            (predict (list 'T s (first (first s-trans))) (second (first s-trans)))
            (recur (rest s-trans)))))
      ;; Counts of open and blocked nodes.
      (predict 'nopen (retrieve ::nopen))
      (predict 'nblocked (retrieve ::nblocked))

      res)))

(defn get-distance
  "computes distance from log-weight"
  ;; Purposefully defined as a regular clojure function
  ;; to access the state.
  [cont $state cost]
  (cont (/ (- (embang.state/get-log-weight $state)) cost) $state))

(defanglican ctp "expected path cost" parameters

  (let ((parameters (if (seq parameters)
                      parameters
                      (list parameters)))
        (p-open (or (first parameters) P-OPEN))
        (cost (or (second parameters) COST))
        (instance (get ctp-data (or (second (rest parameters)) INSTANCE))))

    (observe (flip 1.) ; drop disconnected instances
             (travel (get instance :graph) (get instance :s) (get instance :t)
                     p-open cost))

    (predict 'distance (get-distance cost))))
