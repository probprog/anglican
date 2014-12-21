(ns embang.runtime
  (:require [embang.colt.distributions
             :as dist]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.linear :as ml]
            [clojure.core.matrix.operators :as mo]))

;; matrix library uses vectorz for protocol implementations
(m/set-current-implementation :vectorz)

;;; Anglican core functions beyond clojure.core

(defn abs [x] (Math/abs x))
(defn floor [x] (Math/floor x))
(defn ceil [x] (Math/ceil x))
(defn round [x] (Math/round x))
(defn rint [x] (Math/rint x))
(defn signum [x] (Math/signum x))

(defn sin [x] (Math/sin x))
(defn cos [x] (Math/cos x))
(defn tan [x] (Math/tan x))
(defn asin [x] (Math/asin x))
(defn acos [x] (Math/acos x))
(defn atan [x] (Math/atan x))
(defn sinh [x] (Math/sinh x))
(defn cosh [x] (Math/cosh x))
(defn tanh [x] (Math/tanh x))

(defn log [x] (Math/log x))
(defn exp [x] (Math/exp x))
(defn cbrt [x] (Math/cbrt x))
(defn sqrt [x] (Math/sqrt x))
(defn pow [x y] (Math/pow x y))

(defn isnan? [x]
  (Double/isNaN x))

(defn isfinite? [x]
  (not (or (Double/isNaN x) (Double/isInfinite x))))

(defn sum
  "sum of the collection elements"
  [coll]
  (reduce + coll))

(defn mean
  "mean of the collection elements"
  [coll]
  (/ (sum coll) (count coll)))

(defn normalize
  "normalized collection"
  [coll]
  (let [Z (sum coll)]
    (map #(/ % Z) coll)))

;;; Random distributions

(defprotocol distribution
  "random distribution"
  (sample [this]
    "draws a sample from the distribution")
  (observe [this value]
    "return the probability [density] of the value"))

;; distributions, in alphabetical order

(defmacro from-colt
  "wraps incanter distribution"
  ([name args]
     ;; the name and the argument order is the same
     `(from-colt ~name ~args (~name ~@args)))

  ([name args [incanter-name & incanter-args]]
     `(defn ~(with-meta  name {:doc (str name " distribution")})
        ~args
        (let [~'dist (~(symbol (format 
                                "dist/%s-distribution"
                                incanter-name))
                      ~@incanter-args)]
          ~'(reify distribution
              (sample [this] (dist/draw dist))
              (observe [this value] (log (dist/pdf dist value))))))))

(from-colt beta [alpha beta])
(from-colt binomial [n p] (binomial p n))

(defn discrete
  "discrete distribution, accepts unnormalized weights"
  [weights]
  (let [total-weight (reduce + weights)]
    (reify distribution
      (sample [this] 
        (let [x (rand total-weight)]
          (loop [[weight & weights] weights
                 acc 0. value 0]
            (let [acc (+ acc weight)]
              (if (< x acc) value
                  (recur weights acc (inc value)))))))
      (observe [this value] 
        (Math/log (/ (nth weights value) total-weight))))))

(declare gamma) ; Gamma distribution used in Dirichlet distribution

(letfn [(gamma-function [x]
          (cern.jet.stat.Gamma/gamma x))]
  (defn dirichlet
    "Diriclhet distribution"
    ;; borrowed from Anglican runtime
    [alpha]
    (let [Z (delay (/ (reduce * (map gamma-function alpha))
                      (gamma-function (reduce + alpha))))]
      (reify distribution
        (sample [this]
          (let [g (map #(sample (gamma % 1)) alpha)
                t (reduce + g)]
            (map #(/ % t) g)))
        (observe [this value]
          (- (reduce + (map (fn [v a] (* (Math/log v) (- a 1))) 
                            value
                            alpha))
             @Z))))))

(from-colt exponential [rate])

(defn flip
  "flip (bernoulli) distribution"
  [p]
  (reify distribution
    (sample [this] (< (rand) p))
    (observe [this value] (Math/log (if value p (- 1. p))))))

(from-colt gamma [shape rate])
(from-colt normal [mean sd])
(from-colt poisson [lambda])
(from-colt uniform-continuous [min max] (uniform min max))
(from-colt uniform-discrete [min max] (integer min max))

(defn mvn
  "multivariate normal"
  [mean cov]
  (let [k (count mean)     ; number of dimensions
        {Lcov :L} (ml/cholesky (m/matrix cov) {:return [:L]})
        ;; delayed because used only by one of the methods
        unit-normal (delay (normal 0 1))
        Z (delay (let [|Lcov| (reduce * (m/diagonal Lcov))]
                   (* 0.5 (+ (* k (Math/log (* 2 Math/PI)))
                             (Math/log |Lcov|)))))]
    (reify distribution
      (sample [this]
        (mo/+ mean
              (m/mmul Lcov
                      (repeatedly k #(sample @unit-normal)))))
      (observe [this value]
        (let [dx (m/mmul (m/inverse Lcov) (mo/- value mean))]
          (- (* -0.5 (m/dot dx dx)) @Z))))))

(defn wishart
  "Wishart distribution"
  ;; http://en.wikipedia.org/wiki/Wishart_distribution
  [n V] 
  {:pre [(integer? n) (>= n (first (m/shape V)))]}
  (let [d (first (m/shape V))
        {L :L} (ml/cholesky (m/matrix V) {:return [:L]})
        unit-normal (delay (normal 0 1))]
    (reify distribution
      (sample [this]
        (let [X (m/matrix (repeatedly
                            n (fn [] (m/mmul L (repeatedly
                                                 d #(sample @unit-normal))))))]
          (m/mmul (m/transpose X) X))))))
;; `observe' is not implemented because the only use of Wishart distribution
;; is sampling prior for covariance matrix of multivariate normal

;;; Random processes

(defprotocol random-process
  "random process"
  (advance [this value]
        "returns process ready for the next sample"))

;; random processes, in alphabetical order

(defn crp
  "chinese restaurant process"
  ([alpha] (crp [] alpha))
  ([counts alpha] {:pre [(vector? counts)]}
   (prn counts)
   (let [dist (delay (discrete (conj counts alpha)))]
     (reify
       distribution
       (sample [this] (sample @dist))
       (observe [this value] (observe @dist value))

       random-process
       (advance [this value] 
         (crp 
           (update-in counts [value] (fnil inc 0))
           alpha))))))
