(ns ctp
  (use [embang runtime emit]))

;;; Canadian Traveller Program

;; The graph is attributed by two probabilities for each
;; edge --- that the edge is open, and that the traveller
;; dies if he passes through the edge.
;;
;; The objective is to learn a policy of traversal order
;; that maximizes the probability of arriving alive. 
;;
;; The output is wether a path was found and the policy.

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

(defanglican stochastic)

(defanglican deterministic)
