(ns anglican.runtime
  "Runtime library"
  (:require [clojure.string :as str]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.linear :as ml]))

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

;;; Distributions

(defprotocol distribution
  "random distribution"
  (sample [this]
    "draws a sample from the distribution")
  (observe [this value]
    "return the probability [density] of the value"))

;; `sample' is both a protocol method and a special form in
;; Anglican. To generate random values without exposing the random
;; choice as a checkpoint, use `sample*'.

(def sample* "draws a sample from the distribution" sample)

;; Log probabilities are used pervasively. A precision-preserving
;; way to add probabilities (e.g. for computing union probability)
;; is log-sum-exp.

(defn log-sum-exp
  "computes (log (+ (exp x) (exp y))) safely"
  [log-x log-y]
  (let [log-max (max log-x log-y)]
    (if (< (/ -1. 0.) log-max (/ 1. 0.))
      (+ log-max
         (Math/log (+ (Math/exp (- log-x log-max))
                      (Math/exp (- log-y log-max)))))
      log-max)))

;; Distribution types, in alphabetical order.

(def ^:private RNG
  "random number generator;
  used by colt distribution objects"
  (anglican.MTMersenneTwister. (java.util.Date.)))

;; Distributions are defined as records so that every
;; distribution has its own type. The distribution arguments
;; are available as the record fields.

(defn ^:private qualify
  "accepts a symbol, returns the qualified symbol;
  intended to be called from a macro"
  [s]
  (symbol (format "%s/%s" *ns* s)))

(defmacro defdist
  "defines distribution"
  [name & args]
  (let [[docstring parameters bindings & methods]
        (if (string? (first args))
          args
          `(~(format "%s distribution" name) ~@args))]
    (let [record-name (symbol (format "%s-distribution" name))
          variables (take-nth 2 bindings)]
      `(do
         (declare ~name)
         (defrecord ~record-name [~@parameters ~@variables]
           Object
           (toString [~'this]
             (str (list '~(qualify name) ~@parameters)))
           distribution
           ~@methods)
         (defn ~name ~docstring ~parameters
           (let ~bindings
             (~(symbol (format "->%s" record-name))
                       ~@parameters ~@variables)))
         (defmethod print-method ~record-name
           [~'o ~'m]
           (print-simple (str ~'o) ~'m))))))

;; Many distributions are available in the Colt library and
;; imported automatically.

(defmacro ^:private from-colt
  "wraps colt distribution"
  ([name args vtype]
   `(from-colt ~name ~args ~vtype (~(str/capitalize name) ~@args)))
  ([name args vtype [colt-name & colt-args]]
   `(defdist ~(symbol name)
      ~(format "%s distribution (imported from colt)" name)
      ~args
      [dist# (~(symbol (format "cern.jet.random.%s." colt-name))
                       ~@colt-args RNG)]
      (~'sample [~'this] (~(symbol (format ".next%s"
                                           (str/capitalize vtype)))
                                   dist#))
      (~'observe [~'this ~'value] (log (~'.pdf dist# ~'value))))))

(defdist bernoulli
  "Bernoulli distribution"
  [p] [dist (cern.jet.random.Uniform. RNG)]
  (sample [this] (if (< (.nextDouble dist) p) 1 0))
  (observe [this value]
    (Math/log (case value
                1 p
                0 (- 1. p)
                0.))))

(from-colt beta [alpha beta] double)
(from-colt binomial [n p] int)

(declare discrete)
(defdist categorical
  "categorical distribution,
  convenience wrapper around discrete distribution;
  accepts a list of categories --- pairs [value weight]"
  [categories] [values (mapv first categories)
                index (into {} (map-indexed (fn [i v] [v i]) values))
                dist (discrete (map second categories))]
  (sample [this] (values (sample dist)))
  (observe [this value] (observe dist (index value -1))))

(defdist discrete
  "discrete distribution, accepts unnormalized weights"
  [weights] [total-weight (double (reduce + weights))
             dist (cern.jet.random.Uniform. 0. total-weight RNG)]
  (sample [this]
    (let [x (.nextDouble dist)]
      (loop [[weight & weights] weights
             acc 0. value 0]
        (let [acc (+ acc weight)]
          (if (< x acc) value
            (recur weights acc (inc value)))))))
  (observe [this value]
    (Math/log
      (try
        (/ (nth weights value) total-weight)
        ;; any value not in the support has zero probability.
        (catch IndexOutOfBoundsException _ 0.)))))

(declare gamma) ; Gamma distribution used in Dirichlet distribution

(defn log-gamma-fn
  "log Gamma function"
  [x]
  (cern.jet.stat.Gamma/logGamma x))

(defdist dirichlet
  "Dirichlet distribution"
  ;; borrowed from Anglican runtime
  [alpha] [Z (delay (- (reduce + (map log-gamma-fn alpha))
                       (log-gamma-fn (reduce + alpha))))]
  (sample [this]
          (let [g (map #(sample (gamma % 1)) alpha)
                t (reduce + g)]
            (map #(/ % t) g)))
  (observe [this value]
           (- (reduce + (map (fn [v a] (* (Math/log v) (- a 1)))
                             value
                             alpha))
              @Z)))

(from-colt exponential [rate] double)

(defdist flip
  "flip (Bernoulli boolean) distribution"
  [p] [dist (cern.jet.random.Uniform. RNG)]
  (sample [this] (< (.nextDouble dist) p))
  (observe [this value]
           (Math/log (case value
                       true p
                       false (- 1. p)
                       0.))))

(defdist gamma
  "Gamma distribution, parameterized by shape and rate"
  [shape rate]
  [dist (cern.jet.random.Gamma. shape rate RNG)
   Z (delay (- (cern.jet.stat.Gamma/logGamma shape)
               (* shape (log rate))))]
  (sample [this] (.nextDouble dist))
  (observe [this value]
           ;;       shape  shape - 1
           ;;     rate    x
           ;; log --------------------
           ;;      rate x
           ;;     e       Gamma(shape)
           (- (* (- shape 1.) (log value))
              (* rate value)
              @Z)))

(from-colt normal [mean sd] double)
(from-colt poisson [lambda] int)
(from-colt uniform-continuous [min max] double
  ;; The explicit type cast below is a fix to clojure
  ;; constructor matching (clojure.lang.Reflector.isCongruent).
  ;; If the constructor is overloaded with the same number of
  ;; arguments, clojure refuses to extend numeric types.
  (Uniform (double min) (double max)))

(defdist uniform-discrete
  "uniform discrete distribution"
  [min max] [dist (uniform-continuous min max)
             p (/ 1. (- max min))]
  (sample [this] (int (sample dist)))
  (observe [this value]
           (Math/log
             (if (and (integer? value)
                      (<= min value) (< value max))
               p 0.))))

(defprotocol multivariate-distribution
  "additional methods for multivariate distributions"
  (transform-sample [this samples]
    "accepts a vector of random values and generates
    a sample from the multivariate distribution"))


(defdist mvn
  "multivariate normal"
  [mean cov] [k (m/ecount mean)     ; number of dimensions
              Lcov (:L (ml/cholesky (m/matrix cov)))
              unit-normal (normal 0 1)
              Z (delay (let [|Lcov| (reduce * (m/diagonal Lcov))]
                         (* 0.5 (+ (* k (Math/log (* 2 Math/PI)))
                                  (Math/log |Lcov|)))))
              iLcov (delay (m/inverse Lcov))
              transform-sample (fn [samples]
                                 (m/add mean (m/mmul Lcov samples)))]
  (sample [this] (transform-sample
                   (repeatedly k #(sample unit-normal))))
  (observe [this value]
           (let [dx (m/mmul @iLcov (m/sub value mean))]
             (- (* -0.5 (m/dot dx dx)) @Z)))
  multivariate-distribution
  (transform-sample [this samples] (transform-sample samples)))

(defn log-mv-gamma-fn
  "multivariate Gamma function"
  [p a]
  (+ (* 0.25 p (- p 1) (Math/log Math/PI))
     (reduce + (map (fn [j]
                      (log-gamma-fn (- a (* 0.5 j))))
                    (range p)))))

(defn create-matrix
  "creates a matrix, elements of which are initialised
  using the filler procedure"
  [rows columns filler]
  (m/reshape
    (for [r (range rows) c (range columns)]
      (filler r c))
    [rows columns]))

(defdist continuous-chi-squared
  "Chi-squared distribution with continuous parameter nu"
  [nu]
  [
   ;; Chi-Squared(nu) ~ Gamma(shape = nu / 2, scale = 2.0).
   ;; In Colt library Gamma is parametrised in its second argument
   ;; by rate, where scale = 1 / rate.
   gamma-dist (gamma (* nu 0.5) 0.5)]
  (sample [this]
          (sample gamma-dist))
  (observe [this value]
           (+
            (* (- (* nu 0.5) 1) (log value))
            (* -0.5 value)
            (* -0.5 nu (log 2))
            (* -1.0 (log-gamma-fn (* 0.5 nu))))))

(defdist wishart
  "Wishart distribution"
  [n V]
  [p (first (m/shape V))
   L (:L (ml/cholesky (m/matrix V)))
   unit-normal (normal 0 1)
   ;; Sample from Chi-squared distribution
   ;; with the help of Gamma distribution.
   ;; http://en.wikipedia.org/wiki/Chi-squared_distribution#Relation_to_other_distributions
   continuous-chi-squared-wrapper
   (memoize
    (fn
      [chi-squared-nu]
      (continuous-chi-squared chi-squared-nu)))
   ;; For Bartlett decomposition
   ;; http://en.wikipedia.org/wiki/Wishart_distribution#Bartlett_decomposition
   wishart-filler
   (fn
     [row column]
     (if (= row column)
       ;; (inc row) below since indexing start from 0.
       (sqrt
        (sample (continuous-chi-squared-wrapper(+ (- n (inc row)) 1))))
       (if (> row column)
         (sample unit-normal)
         0.0)))
   Z (delay (+ (* 0.5 n p (Math/log 2))
               (* 0.5 n (Math/log (m/det V)))
               (log-mv-gamma-fn p (* 0.5 n))))]
  (sample [this]
          ;; Bartlett decomposition
          ;; http://en.wikipedia.org/wiki/Wishart_distribution#Bartlett_decomposition
          ;; and https://stat.duke.edu/~km68/materials/214.9%20%28Wishart%29.pdf
          (let
            [A (create-matrix p p wishart-filler)
             LA (m/mmul L A)]
            (m/mmul LA (m/transpose LA))))
  (observe [this value]
           (- (* 0.5 (- n p 1) (Math/log (m/det value)))
              (* 0.5 (m/trace (m/mmul (m/inverse (m/matrix V)) value)))
              @Z)))

;;; Random processes

(defprotocol random-process
  "random process"
  (produce [this]
    "produces a static random source")

  (absorb [this sample]
    "absorbs the sample and returns a new process"))

(defmacro defproc
  "defines random process"
  [name & args]
  (let [[docstring parameters bindings & methods]
        (if (string? (first args))
          args
          `(~(format "%s random process" name) ~@args))]
    (let [record-name (symbol (format "%s-process" name))
          variables (take-nth 2 bindings)
          values (take-nth 2 (rest bindings))]
      `(do
         (declare ~name)
         (defrecord ~record-name [~@parameters ~@variables]
           Object
           (toString [~'this]
             (str (list '~(qualify name) ~@parameters)))
           random-process
           ~@methods)
         (defn ~name ~docstring
           ;; Include parameters-only overload only if variables
           ;; are not empty.
           ~@(when (seq variables)
               `((~parameters (~name ~@parameters ~@values))))
           ([~@parameters ~@variables]
            (~(symbol (format "->%s" record-name))
                      ~@parameters ~@variables)))
         (defmethod print-method ~record-name
           [~'o ~'m]
           (print-simple (str ~'o) ~'m))))))

;; Random processes can accept and return functions,
;; and translations in and out of CPS form must be performed.
;; To avoid circular dependencies (emit<trap<runtime<emit),
;; we define two wrappers here.

(defn ^:private uncps
  "reconstructs value-returning function from CPS form"
  [f]
  (fn [& args]
    (trampoline (apply f (fn [v _] v) nil args))))

(defn ^:private cps
  "wrap value-returning function into CPS form"
  [f]
  (fn [cont $state & args]
    (fn [] (cont (apply f args) $state))))

;; Random process types, in alphabetical order.

(defdist discrete-crp
  "discrete distribution extended
  by a random sample, for use with CRP"
  [counts alpha] [dist (discrete (conj counts alpha))]
  (sample [this] (sample dist))
  (observe [this value]
    ;; Observing any new sample has the same probability.
    (observe dist (min (count counts) value))))

(defproc CRP
  "Chinese Restaurant process"
  [alpha] [counts []]
  (produce [this] (discrete-crp counts alpha))
  (absorb [this sample]
    (CRP alpha
         (-> counts
             ;; Fill the counts with alpha (corresponding to
             ;; the zero count) until the new sample.
             (into (repeat (+ (- sample (count counts)) 1) alpha))
             (update-in [sample] inc)))))

(defdist categorical-dp
  "categorical distribution extended
  by a random sample, for use with DP"
  [counts H alpha] [dist (categorical
                           (vec (conj counts [::new alpha])))]
  (sample [this]
    (let [s (sample dist)]
      ;; When a `new' value is drawn, sample the actual
      ;; value from the base measure.
      (if (= s ::new) (sample H) s)))
  (observe [this value]
    (log-sum-exp
      ;; The value is one of absorbed values.
      (observe dist value)
      ;; The value is drawn from the base distribution.
      (+ (observe dist ::new) (observe H value)))))

(defproc DP
  "Dirichlet process"
  [alpha H] [counts {}]
  (produce [this] (categorical-dp counts H alpha))
  (absorb [this sample]
          (DP alpha H (update-in counts [sample] (fnil inc 0)))))

(defn cov
  "computes covariance matrix of xs and ys under k"
  [k xs ys]
  (for [x xs] (for [y ys] (k x y))))

(defproc GP
  "Gaussian process"
  ;; GP is intended to be called from inside m! programs,
  ;; where CPS-transformed functions are passed.
  [m$ k$] [m (uncps m$)
           k (uncps k$)
           points []]
  (produce [this]
    ;; The formulae are taken from
    ;;   http://mlg.eng.cam.ac.uk/pub/pdf/Ras04.pdf
    ;; Carl Edward Rasmussen. Gaussian processes in machine learning.
    ;; In Revised Lectures, volume 3176 of Lecture Notes in Computer
    ;; Science (LNCS), pages 63-71. Springer-Verlag, Heidelberg, 2004.
    (cps
      (if (seq points)
        (let [xs (mapv first points)
              isgm (m/inverse (m/matrix (cov k xs xs)))
              zs (let [ys (mapv second points)
                       ms (mapv m xs)]
                   (m/mmul isgm (m/sub ys ms)))]
          (fn [x]
            (let [mx (m x)
                  sgm* (cov k xs [x])
                  tsgm* (m/transpose sgm*)]
              (normal (+ mx (first (m/mmul tsgm* zs)))
                      (sqrt (- (k x x)
                               (ffirst
                                 (m/mmul tsgm* isgm sgm*))))))))
        (fn [x] (normal (m x) (sqrt (k x x)))))))
  (absorb [this sample]
    (GP m$ k$ m k (conj points sample))))
