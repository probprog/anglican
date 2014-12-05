(ns embang.runtime
  (:require [embang.colt.distributions
             :as dist])
  (:use [embang.emit :only [def-cps-fn]]))

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
              (draw [this] (dist/draw dist))
              (prob [this value] (dist/pdf dist value)))))))

(from-colt beta [alpha beta])
(from-colt binomial [n p] (binomial p n))

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

(declare gamma) ; Gamma distribution used in Dirichlet distribution

(letfn [(gamma-function [x]
          (cern.jet.stat.Gamma/gamma x))]
  (defn dirichlet
    "Diriclhet distribution"
    ;; borrowed from Anglican runtime
    [alpha]
    (let [Z (/ (reduce * (map dist/gamma-function alpha))
               (dist/gamma-function (reduce + alpha)))]
      (reify distribution
        (draw [this]
          (let [g (map #(draw (gamma % 1)) alpha)
                t (reduce + g)]
            (map #(/ % t) g)))
        (prob [this value]
          (/ (reduce * (map (fn [v a] (Math/pow v (- a 1))) 
                            value
                            alpha))
             Z))))))

(from-colt exponential [rate])

(defn flip
  "flip (bernoulli) distribution"
  [p]
  (reify distribution
    (draw [this] (< (rand) p))
    (prob [this value] (if value p (- 1. p)))))

(from-colt gamma [shape rate])
(from-colt normal [mean sd])
(from-colt poisson [lambda])
(from-colt uniform-continuous [min max] (uniform min max))
(from-colt uniform-discrete [min max] (integer min max))

;; CPS versions of higher-order functions.

(def-cps-fn ^:private $map1 
  "map on a single sequence"
  [fun lst]
  (if (empty? lst) nil
      (cons (fun (first lst))
            ($map1 fun (rest lst)))))

(def-cps-fn ^:private $nils? 
  "true if the list contains nil"
  [lst]
  (and (seq lst)
       (or (nil? (first lst))
           ($nils? (rest lst)))))

(def-cps-fn $map 
  "map in CPS"
  [fun & lsts]
  (let [tuple ($map1 first lsts)]
    (if ($nils? tuple) nil
        (let [lsts ($map1 rest lsts)]
          (cons (apply fun tuple)
                (apply $map fun lsts))))))

(def-cps-fn ^:private $reduce1
  "reduce with explicit init in CPS"
  [fun init lst]
  (if (empty? lst) init
      ($reduce1 fun
                (fun init (first lst))
                (rest lst))))

(def-cps-fn $reduce
  "reduce in CPS"
  [fun & args]
  (let [init (if (seq (rest args))
               (first args) 
               (first (first args)))
        lst (if (seq (rest args))
              (second args)
              (rest (first args)))]
    ($reduce1 fun init lst)))


