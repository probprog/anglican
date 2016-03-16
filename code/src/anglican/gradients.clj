(ns anglican.gradients
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:require [clojure.core.matrix :as m :refer [mul div add sub mmul]]
            [clojure.core.matrix.linear :as linalg])
  (:use [anglican.runtime :refer :all]))

;;; Math functions required by gradients

(defn digamma
  "digamma function psi(x): derivative of gammaln(x),
  apparently bizarrely missing from all Clojure libraries.
  Not yet implemented for negative values of x.
  source: http://en.wikipedia.org/wiki/Digamma_function"
  [x]
  (assert (>= x 0.0))
  (if (<= x 0.0)
    (Math/log 0.0)
    (let [partial-sum (if (< x 1) (/ -1. x) 0.0)
          x (if (< x 1) (+ x 1.0) x)]
      (+ partial-sum 
         (Math/log x)
         (/ -1. (* 2 x))
         (/ -1. (* 12 (Math/pow x 2)))
         (/ 1. (* 120 (Math/pow x 4)))
         (/ -1. (* 252 (Math/pow x 6)))
         (/ 1. (* 240 (Math/pow x 8)))
         (/ -5. (* 660 (Math/pow x 10)))
         (/ 691. (* 32760 (Math/pow x 12)))
         (/ -1. (* 12 (Math/pow x 14)))))))

(defn positive-and-finite?
  "is the numeric value x finite? is it also strictly positive?"
  [x] (and (finite? x) (> x 0.)))

;;; Gradient protocol
;;; 
;;; Note that grad-log may take its gradients with respect to different
;;; parameters than those used to define the distribution --- for example,
;;; the gradient for `anglican.runtime.normal-distribution` is with respect
;;; to the mean and the log of the standard deviation.

(defprotocol DistGradient
  (grad-log [dist] 
    "returns a function for the gradient of the log density.")
  (grad-step [dist grad rho]
    "updates distribution parameters by performing one gradient step."))

(extend-protocol DistGradient
  anglican.runtime.normal-distribution
  (grad-log [dist]
    (let [mu (:mean dist)
          sigma (:sd dist)
          z-sigma (log sigma)]
      ;; parameterized by log-sd
      (fn [x]
        [(/ (- x mu) (pow sigma 2))
         (+ (- 1.) 
            (/ (pow (- x mu) 2) 
               (exp (* 2. z-sigma))))])))
  (grad-step [dist grad rho]
    (let [[mu z-sigma] (add [(:mean dist) (log (:sd dist))]
                            (mul rho grad))
          sigma (exp z-sigma)]
      (normal mu sigma))))

(extend-protocol DistGradient
  anglican.runtime.gamma-distribution
  (grad-log [dist]
    (let [alpha (:shape dist)
          beta (:rate dist)]
      (when (not (>= alpha 0.0))        
        (println "Invalid:" dist))
      (fn [x]
        [(* alpha (+ (log beta) (log x) (- (digamma alpha))))
         (* beta (- (/ alpha beta) x))])))
  (grad-step [dist grad rho]
    (let [[a b] (add [(log (:shape dist)) (log (:rate dist))]
                     (mul rho grad))
          alpha (exp a)
          beta (exp b)]
      (if (not (and (positive-and-finite? alpha) (positive-and-finite? beta)))
        (do
          (println "gradient step not reasonable:" dist grad rho)
          (grad-step dist grad (div rho 10.0)))
        (gamma alpha beta)))))

(extend-protocol DistGradient
  anglican.runtime.beta-distribution 
  (grad-log [dist]
    (let [a (:alpha dist)
          b (:beta dist)
          digamma-term (digamma (+ a b))]
      ;; parameterized by log of standard params
      (fn [x]
        (mul [a b]
             (add digamma-term
                  [(- (log x) (digamma a))
                   (- (log (- 1. x)) (digamma b))])))))
  (grad-step [dist grad rho]
    (let [z-alpha (add (m/log [(:alpha dist) (:beta dist)]) (mul rho grad))
          [a b] (m/exp z-alpha)]
      (if (not (and (positive-and-finite? a) (positive-and-finite? b)))
        (do
          (println "gradient step not reasonable:" dist grad rho)
          (if (finite? rho)
            (grad-step dist grad (div rho 10.0))
            nil))
        (beta a b)))))

(extend-protocol DistGradient
  anglican.runtime.flip-distribution 
  (grad-log [dist]
    (let [p (:p dist)
          q (- 1. p)
          z (log (/ p q))]
      (fn [x]
        [(* (* p q)
            (if x (/ 1. p) (/ -1. q)))])))
  (grad-step [dist grad rho]
    (let [p (:p dist)
          z (first (add [(log (/ p (- 1 p)))] (mul rho grad)))]
      (assoc dist :p (/ 1. (+ 1 (exp (- z))))))))

(extend-protocol DistGradient
  anglican.runtime.dirichlet-distribution 
  (grad-log [dist]
    (let [alpha (:alpha dist)
          sum-alpha (reduce + alpha)]
      ;; parameterized by log-alpha
      ;; log-pdf:
      ;; gammaln(sum alpha_i) - (sum gammaln(alpha_i) + (alpha_i-1) ln x_i)
      (assert (every? positive-and-finite? alpha))
      (fn [x]
        ;;(assert (every? positive-and-finite? x) (str (into [] x)))
        (mul alpha
             ;; "normal" gradient:
             (add (digamma sum-alpha)
                  ;; note: gamma rngs can return numerically zero samples.
                  ;; in order to avoid diverging gradients, we clip evaluation
                  ;; of the gradient at (>= x 1e-12)
                  (map #(- (log (max %1 1e-12)) 
                           (digamma %2)) 
                       x alpha))))))
  (grad-step [dist grad rho]
    (let [z-alpha (add (m/log (:alpha dist)) (mul rho grad))
          alpha (m/exp z-alpha)]
      (assert (every? finite? rho) (str rho))
      (if (not (every? positive-and-finite? alpha))
        (do
          (println "gradient step not reasonable:" dist grad rho alpha)
          (grad-step dist grad (div rho 10.0)))
        (dirichlet alpha)))))

(extend-protocol DistGradient
  anglican.runtime.discrete-distribution 
  (grad-log [dist]
    (let [w (vec (:weights dist))
          p (div w (reduce + w))
          K (count w)
          empty (repeat K 0.0)]
      (fn [x]
        (let [wx (m/set-indices empty [x] 1.)]
          (sub wx p)))))
  (grad-step [dist grad rho]
    (let [z (m/log (:weights dist))
          z (into [] (add z (mul rho grad)))]
      (discrete (m/exp z)))))

;;; TODO: eventually, extend DistGradient for all random procedures

(def implemented-gradients
  "set of distribution types for which gradients are implemented"
  (into #{}  (extenders DistGradient)))

(defn adaptable?
  "test whether gradients are implemented for a distribution type"
  [dist-type]
  (extends? DistGradient dist-type)) 
