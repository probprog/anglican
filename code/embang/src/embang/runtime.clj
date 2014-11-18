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

(defn mean
  [coll]
  (/ (reduce + coll) (count coll)))

(defn cumsum
   "With single argument coll returns lazy cumulative sum sequence y
   with (= (nth y) (cumsum coll)). With two arguments t, coll returns
   lazy cumulative sum sequence y with (= (nth y) (+ t (cumsum coll)))."
  ([coll] (cumsum 0 coll))
  ([t coll] (if (empty? coll) ()
           (let [y (+ t (first coll))]
             (cons y (lazy-seq (cumsum y (rest coll))))))))

(defn sum
  [coll]
  (reduce + coll))

(defn normalize
  [coll]
  (let [Z (reduce + coll)]
    (map #(/ % Z) coll)))

(defprotocol distribution
  (sample [this]
    "return a sample from the distribution")
  (observe [this value]
    "return the log-probability of the value"))


(defn flip [p]
  "flip (bernoulli) distribution object"
  (reify
    distribution
    (sample [this] (if (< (rand) p) 1 0))
    (observe [this value] (log (if (> value 0) p (- 1. p))))))

(defn normal [^double mean ^double sd]
  "normal distribution object"
  (let [dist (dist/->Normal-rec mean sd)]
    (reify 
      distribution
      (sample [this] (dist/draw dist))
      (observe [this value] (log (dist/pdf dist value))))))

(defn poisson [lambda]
  "poisson distribution object"
  (let [dist (dist/->Poisson-rec lambda)]
    (reify
      distribution
      (sample [this] (dist/draw dist))
      (observe [this value] (log (dist/pdf dist value))))))
