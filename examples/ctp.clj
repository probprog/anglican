(ns ctp
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

;;; Bounded horizon random walk

(defn dirichlet-uniform
  "Uniform diriclhet distribution with pdf == 1"
  [n]
  (reify distribution
    (sample [this]
      (let [g (repeatedly n #(sample (gamma 1 1)))
            t (reduce + g)]
        (map #(/ % t) g)))
    (observe [this value] 0.)))

(with-primitive-procedures [dirichlet-uniform]
  (defun travel (graph s t p-open)
    ;; All edges are open or blocked with the same probability.
    ;; The results are conditioned on this random choice, hence
    ;; the choice is hidden (*) from the inference algorithm.
    (let ((open? (mem (lambda (u v) (sample* (flip p-open)))))
          ;; Policy is conditioned on the parent node p and
          ;; current node u.
          (policy (mem (lambda (p u)
                         ;; I want to see the path cost in map
                         ;; estimation output, but dirichlet has
                         ;; pdf which depends on dimensionality.
                         (let ((children (map first (nth graph u))))
                           (map list
                                children
                                (sample
                                  (dirichlet-uniform 
                                    (count children))))))))

          ;; Probability distribution that the traveller
          ;; `likes' the edge.
          (likes (lambda (u v)
                   (loop ((children (nth graph u)))
                     (let ((child (first children)))
                       (if (= (first child) v)
                         (flip (exp (- (second child))))
                         (begin (assert (seq (rest children)))
                                (recur (rest children))))))))

          ;; True when t is reachable from u  in at most n steps;
          ;; u was entered from p.
          (reachable? 
            (lambda (p u t n)
              (cond
                ((= u t) true)
                ((= n 0) false)
                (else
                  (let ((policy-p-u (filter (lambda (choice)
                                              (open? u (first choice)))
                                            (policy p u))))
                    (if (seq policy-p-u)
                      (let ((dist (categorical policy-p-u))
                            (v (sample dist)))
                        (observe (likes u v) true)
                        (reachable? u v t (dec n))))))))))

      (reachable? nil s t (* 50 (count graph)))))

  (defun connected? (p-open)
    (some?
      (travel graph-10 (first s-t-10) (second s-t-10)
              p-open)))

  (def p-open 0.5)

  (defanglican ctp
    (predict (connected? p-open)))
  
  (defanglican path-cost
    (observe (flip 1.) (connected? p-open))
    (predict true)))
