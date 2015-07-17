(ns anglican.gradients
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:require [clojure.core.matrix :as m :refer [mul div add sub mmul]]
            [clojure.core.matrix.linear :as linalg]
            [anglican.math :refer [digamma isfinite?]])
  (:use [anglican.runtime :refer :all]))


;;; Variational distribution details (need implemented per-ERP)

;; Return a function which computes the gradient at a point
(defmulti grad-log (fn [dist] (type dist)))

;; Return a distribution after a gradient step with stepsize rho
(defmulti grad-step (fn [dist grad rho] (type dist)))


;;; Per-distribution implementations

;; Normal

(defmethod grad-log anglican.runtime.normal-distribution [dist]
  (let [mu (:mean dist)
        sigma (:sd dist)
        z-sigma (log sigma)]
    ;; parameterized by log-sd
    (fn [x]
      [(/ (- x mu) (pow sigma 2))
       (+ (- 1.) (/ (pow (- x mu) 2) (exp (* 2. z-sigma))))])))

(defmethod grad-step anglican.runtime.normal-distribution [dist grad rho]
  (let [[mu z-sigma] (add [(:mean dist) (log (:sd dist))]
                           (mul rho grad))
        sigma (exp z-sigma)]
    (normal mu sigma)))

;; Gamma

(defmethod grad-log anglican.runtime.gamma-distribution [dist]
  (let [alpha (:shape dist)
        beta (:rate dist)]
    (when (not (>= alpha 0.0))
      (println "Invalid:" dist))
    (fn [x]
      [(* alpha (+ (log beta) (log x) (- (digamma alpha))))
       (* beta (- (/ alpha beta) x))])))

(defmethod grad-step anglican.runtime.gamma-distribution [dist grad rho]
  (let [[a b] (add [(log (:shape dist)) (log (:rate dist))]
                   (mul rho grad))
        alpha (exp a)
        beta (exp b)]
    (if (not (and (isfinite? alpha) (isfinite? beta)))
      (do
        (println "gradient step not reasonable:" dist grad rho)
        (grad-step dist grad (/ rho 10.0)))
      (gamma alpha beta))))

;; Beta

(defmethod grad-log anglican.runtime.beta-distribution [dist]
  (let [a (:alpha dist)
        b (:beta dist)
        digamma-term (digamma (+ a b))]
    ;; parameterized by log of standard params
    (fn [x]
      (mul [a b]
           (add digamma-term
                [(- (log x) (digamma a))
                 (- (log (- 1. x)) (digamma b))])))))

(defmethod grad-step anglican.runtime.beta-distribution [dist grad rho]
  (let [z-alpha (add (m/log [(:alpha dist) (:beta dist)]) (mul rho grad))
        [a b] (m/exp z-alpha)]
    (if (not (and (isfinite? a) (isfinite? b)))
      (do
        (println "gradient step not reasonable:" dist grad rho)
        (if (isfinite? rho)
          (grad-step dist grad (/ rho 10.0))
          nil))
      (beta a b))))

;; Flip

(defmethod grad-log anglican.runtime.flip-distribution [dist]
  (let [p (:p dist)
        q (- 1. p)
        z (log (/ p q))]
    (fn [x]
      [(* (* p q)
          (if x (/ 1. p) (/ -1. q)))])))

(defmethod grad-step anglican.runtime.flip-distribution [dist grad rho]
  (let [p (:p dist)
        z (first (add [(log (/ p (- 1 p)))] (mul rho grad)))]
    (assoc dist :p (/ 1. (+ 1 (exp (- z)))))))

;; Dirichlet

(defmethod grad-log anglican.runtime.dirichlet-distribution [dist]
  (let [alpha (:alpha dist)
        sum-alpha (reduce + alpha)]
    ;; parameterized by log-alpha
    ;; log-pdf:
    ;; gammaln(sum alpha_i) - (sum gammaln(alpha_i) + (alpha_i-1) ln x_i)
    (fn [x]
      (mul alpha
           ;; "normal" gradient:
           (add (digamma sum-alpha)
                (map #(- (log %1) (digamma %2)) x alpha))))))

(defmethod grad-step anglican.runtime.dirichlet-distribution [dist grad rho]
  (let [z-alpha (add (m/log (:alpha dist)) (mul rho grad))
        alpha (m/exp z-alpha)]
    (if (not (reduce #(and %1 %2) (map isfinite? alpha)))
      (do
        (println "gradient step not reasonable:" dist grad rho)
        (grad-step dist grad (/ rho 10.0)))
      (dirichlet alpha))))

;; Discrete

(defmethod grad-log anglican.runtime.discrete-distribution [dist]
  (let [w (vec (:weights dist))
        p (div w (reduce + w))
        K (count w)
        empty (repeat K 0.0)]
    (fn [x]
      (let [wx (m/set-indices empty [x] 1.)]
        (sub wx p)))))

(defmethod grad-step anglican.runtime.discrete-distribution [dist grad rho]
  (let [z (m/log (:weights dist))
        z (into [] (add z (mul rho grad)))]
    (discrete (m/exp z))))



;;; Check whether a distribution type can be adapted

(def implemented-gradients
  (into #{} (keys (.getMethodTable grad-log))))

(defn adaptable?
  "check whether adaptation or bbvi is implemented for a
  given distribution type"
  [dist-type]
  (contains? implemented-gradients dist-type))
