(ns embang.runtime
  (:require [incanter.distributions :as dist]))

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

(defprotocol distribution
  (draw [this]
    "draws a sample from the distribution")
  (prob [this value]
    "return the probability [density] of the value"))

;; runtime distribution methods

(defn sample
  "draws a sample from the distribution"
  [dist] 
  (draw dist))

(defn observe
  "computes log-probability [density] of the value"
  [dist value]
  (Math/log (prob dist value)))

;;; Distributions, in alphabetical order

(defmacro from-incanter
  "wraps incanter distribution"
  ([name args]
     ;; the name and the argument order is the same
     `(from-incanter ~name ~args (~name ~@args)))

  ([name args [incanter-name & incanter-args]]
     `(defn ~(with-meta  name {:doc (str name " distribution")})
        ~args
        (let [~'dist (~(symbol (format "dist/%s-distribution"
                                       incanter-name))
                      ~@incanter-args)]
          ~'(reify distribution
              (draw [this] (dist/draw dist))
              (prob [this value] (dist/pdf dist value)))))))

(from-incanter beta [alpha beta])
(from-incanter binomial [n p] (binomial p n))

(defn discrete
  "discrete distribution, accepts unnormalized weights"
  [weights]
  (let [total-weight (reduce + weights)]
    (reify distribution
      (draw [this] 
        (let [x (rand total-weight)]
          (loop [[weight & weights] weights
                 acc 0. value 0]
            (let [acc (+ acc weight)]
              (if (< x acc) value
                  (recur weights acc (inc value)))))))
      (prob [this value] 
        (/ (nth weights value) total-weight)))))

(declare gamma)              ; distribution, used to sample
                             ; from the Dirichlet distribution
(let [p [0.99999999999980993
         676.5203681218851
         -1259.1392167224028
         771.32342877765313
         -176.61502916214059
         12.507343278686905
         -0.13857109526572012
         9.9843695780195716e-6
         1.5056327351493116e-7]]

  (defn gamma-function
    "gamma function --- Lanczos approximation (from Wikipedia)"
    [z]
    ;; reflection formula
    (if (< z 0.5)
      (/ Math/PI 
         (* (sin (* Math/PI z)) (gamma-function (- 1. z))))
      (let [z (- z 1.)]
        (loop [x (p 0)
               i 1]
          (if (not= i (count p))
            (recur (+ x (/ (p i) (+ z i))) (inc i))
            (let [t (+ z (count p) -1.5)]
              (* (Math/sqrt (* 2 Math/PI))
                 (Math/pow t (+ z 0.5))
                 (Math/exp (- t))
                 x))))))))

(defn dirichlet
  "Diriclhet distribution"
  ;; borrowed from Anglican runtime
  [alpha]
  (let [Z (/ (reduce * (map gamma-function alpha))
             (gamma-function (reduce + alpha)))]
    (reify distribution
      (draw [this]
        (let [g (map #(draw (gamma % 1)) alpha)
              t (reduce + g)]
          (map #(/ % t) g)))
      (prob [this value]
        (/ (reduce * (map (fn [v a] (Math/pow v (- a 1))) 
                          value
                          alpha))
           Z)))))

(from-incanter exponential [rate])

(defn flip
  "flip (bernoulli) distribution"
  [p]
  (reify distribution
    (draw [this] (< (rand) p))
    (prob [this value] (if value p (- 1. p)))))

(from-incanter gamma [shape rate])
(from-incanter normal [mean sd])
(from-incanter poisson [lambda])
(from-incanter uniform-continuous [min max] (uniform min max))
(from-incanter uniform-discrete [min max] (integer min max))
