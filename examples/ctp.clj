(ns ctp
  (require embang.state)
  (use [embang runtime emit]
       ctp-data))

;;; Canadian Traveller Program

;; The graph is attributed by two probabilities for each
;; edge --- that the edge is open, and that the traveller
;; [dis]likes the edge.
;;
;; The objective is to learn a policy of traversal order
;; that maximizes the probability of arriving alive. 
;;
;; The output is whether a path was found and the policy.

;; Blocking of edges is sampled.  Probability of death is
;; observed.

;; Two options for the policy are stochastic and
;; deterministic.  Deterministic policy is represented by a
;; recursive permutation vector --- every choice bears an
;; index whith which it is swapped.
;;
;; For example, for 4 choices
;;   0 1 2 --- unpermuted order     0 1 2 3
;;   3 1 3 --- corresponds to order 3 1 0 2
;;   3 3 3 ---  --- // ---          3 0 1 2
;;
;; Stochastic policy is represented by vector of
;; probabilities of selecting each edge, and should be
;; easier to infer.

;; A policy specified for each directed edge: i j -> choose
;; node.

;; DFS

(defun travel (graph s t p-open)
  ;; All edges are open or blocked with the same probability.
  ;; The results are conditioned on this random choice, hence
  ;; the choice is hidden (*) from the inference algorithm.
  (let ((visited (gensym "ctp"))
        (open? (mem (lambda (u v) (sample* (flip p-open)))))
        ;; Policy is conditioned on the parent node p and
        ;; current node u.
        (policy (mem (lambda (p u)
                       (let ((children (map first (nth graph u))))
                         (map list
                              children
                              (sample (dirichlet
                                        (repeat (count children)
                                                1.))))))))

        ;; Probability distribution that the traveller
        ;; `likes' the edge.
        (likes (lambda (u v)
                 (loop ((children (nth graph u)))
                   (let ((child (first children)))
                     (if (= (first child) v)
                       (flip (exp (- (second child))))
                       (begin (assert (seq (rest children)))
                              (recur (rest children))))))))

        ;; True when t is reachable from u.
        (dfs (lambda (p u t)
               (or (= u t)
                   (loop ((policy (filter
                                    (lambda (choice)
                                      (open? u (first choice)))
                                    (policy p u))))

                     (let ((policy (filter
                                     (lambda (choice)
                                       (not (contains?
                                              (retrieve visited)
                                              (sort (list u (first choice))))))
                                     policy)))

                       (and (seq policy)
                            (let ((dist (categorical policy))
                                  (v (sample dist)))
                              ;; left through [u v]
                              (store visited
                                     (conj (retrieve visited)
                                           (sort (list u v))))
                              (observe (likes u v) true) 
                              (or (dfs u v t)
                                  (begin
                                    ;; came back through [v u]
                                    (observe (likes v u)  true)
                                    (recur policy)))))))))))

    (store visited (set ()))
    (dfs nil s t)))

(defun connected? (graph s-t p-open)
  (travel graph (first s-t) (second s-t) p-open))

(defn get-distance
  "retrieves distance as the log-weight"
   [cont $state]
   (cont (- (embang.state/get-log-weight $state)) $state))

(defanglican ctp "expected path cost" p-open
  (let ((p-open (if (nil? p-open) 0.5 p-open)))
    (observe (flip 1.) ; drop disconnected instances
             (connected? graph-20 s-t-20 p-open))
    (predict 'distance (get-distance))))
