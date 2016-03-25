(ns anglican.runtime
  "Runtime library"
  (:import [anglican MersenneTwister])
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

(defn finite?
  "is the numeric value x finite?
  returns false for Infinity, -Infinity, and NaN"
  [x] (> (/ 1. 0.) x (/ -1. 0.)))


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

(def Colt-RNG
  "random number generator;
  used by Colt distribution objects"
  (MersenneTwister. (java.util.Date.)))

(def RNG
  "random number generator;
  used by Apache Commons Math distribution objects"
  (org.apache.commons.math3.random.Well19937c.))

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
  (let [[docstring parameters & args]
        (if (string? (first args))
          args
          `(~(format "%s distribution" name) ~@args))
        [bindings & methods]
        (if (vector? (first args))
          args
          `[[] ~@args])
        record-name (symbol (format "%s-distribution" name))
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
         (print-simple (str ~'o) ~'m)))))

;; Many distributions are available in the Colt library and
;; imported automatically.

(defmacro ^:private from-colt
  "wraps Colt distribution"
  ([name args vtype]
   `(from-colt ~name ~args ~vtype (~(str/capitalize name) ~@args)))
  ([name args vtype [colt-name & colt-args]]
   `(defdist ~(symbol name)
      ~(format "%s distribution (imported from colt)" name)
      ~args
      [dist# (~(symbol (format "cern.jet.random.%s." colt-name))
                       ~@colt-args Colt-RNG)]
      (~'sample [~'this] (~(symbol (format ".next%s"
                                           (str/capitalize vtype)))
                                   dist#))
      (~'observe [~'this ~'value] (log (~'.pdf dist# ~'value))))))

(defmacro ^:private from-apache
  "wraps Apache Commons Math distribution"
  [name args type [apache-name & apache-args]]
  (let [dist (gensym "dist")]
    `(defdist ~(symbol name)
       ~(format "%s distribution (imported from apache)" name)
       ~args
       [~dist (~(symbol (format "org.apache.commons.math3.distribution.%sDistribution." apache-name))
                        RNG ~@apache-args)]
       (~'sample [~'this] (.sample ~dist))
       (~'observe [~'this ~'value] 
         ~(case type
            :integer `(~'.logProbability ~dist ~'value)
            :real `(~'.logDensity ~dist ~'value))))))

(defdist bernoulli
  "Bernoulli distribution"
  [p] [dist (cern.jet.random.Uniform. Colt-RNG)]
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
             dist (cern.jet.random.Uniform. 0. total-weight Colt-RNG)]
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
  [p] [dist (cern.jet.random.Uniform. Colt-RNG)]
  (sample [this] (< (.nextDouble dist) p))
  (observe [this value]
           (Math/log (case value
                       true p
                       false (- 1. p)
                       0.))))

(defdist gamma
  "Gamma distribution, parameterized by shape and rate"
  [shape rate]
  [dist (cern.jet.random.Gamma. shape rate Colt-RNG)
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

(defdist chi-squared
  "Chi-squared distribution. Equivalent to a gamma distribution 
  with shape nu/2 and rate 1/2."
  [nu]
  [
   ;; Chi-Squared(nu) ~ Gamma(shape = nu / 2, scale = 2.0).
   ;; In Colt library Gamma is parametrised in its second argument
   ;; by rate, where scale = 1 / rate.
   gamma-dist (gamma (* nu 0.5) 0.5)]
  (sample [this]
          (sample gamma-dist))
  (observe [this value]
           (observe gamma-dist value)))

(from-apache normal [mean sd] :real
  (Normal (double mean) (double sd)))
(from-apache poisson [lambda] :integer
  (Poisson (double lambda) 1E-12 10000000))

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
                         (+ (* 0.5 k (Math/log (* 2 Math/PI)))
                            (Math/log |Lcov|))))
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

(defdist multivariate-t
  "Multivariate T-distribution."
  ; Implemented according to http://en.wikipedia.org/wiki/Multivariate_t-distribution
  [nu mu sigma]
  [dim (m/ecount mu)
   mvn-dist (mvn (m/zero-vector dim) sigma)
   chi-sq-dist (chi-squared nu)]
  (sample 
   [this]
   (let [y (sample mvn-dist)
         u (sample chi-sq-dist)]
     (m/add mu (m/mul y (Math/sqrt (/ nu u))))))
  (observe 
   [this y]
   (let [dy (m/sub mu y)
         dy-sinv-dy (m/mget
                     (m/mmul
                      (m/transpose dy)
                      (m/inverse sigma)
                     dy))]
     (- (log-gamma-fn (* 0.5 (+ nu dim)))
        (+ (log-gamma-fn (* 0.5 nu))
           (* 0.5 dim (Math/log nu))
           (* 0.5 dim (Math/log Math/PI))
           (* 0.5 (Math/log (m/det sigma)))
           (* 0.5 (+ nu dim)
              (Math/log
               (+ 1.0
                  (* (/ 1.0 nu)
                     dy-sinv-dy)))))))))

(defn log-mv-gamma-fn
  "multivariate Gamma function"
  [p a]
  (+ (* 0.25 p (- p 1) (Math/log Math/PI))
     (reduce + (map (fn [j]
                      (log-gamma-fn (- a (* 0.5 j))))
                    (range p)))))

(defn gen-matrix
  "creates a matrix, elements of which are initialised
  using the filler procedure f"
  [f rows columns]
  (m/reshape
    (for [r (range rows)
          c (range columns)]
      (f r c))
    [rows columns]))


(defdist wishart
  "Wishart distribution"
  [n V]
  [p (first (m/shape V))
   L (:L (ml/cholesky (m/matrix V)))
   unit-normal (normal 0 1)
   ;; Sample from Chi-squared distribution
   ;; with the help of Gamma distribution.
   ;; http://en.wikipedia.org/wiki/Chi-squared_distribution#Relation_to_other_distributions
   chi-squared-dists
   ;; Be aware that indexing here starts from 0,
   ;; while in Wikipedia in the Bartlett decomposition subsection
   ;; it starts from 1 so they have:
   ;; c^2_i ~ ChiSq^2_(n - i + 1) where i goes from 1 to p inclusive for them.
   (mapv (comp chi-squared #(- n %)) (range 0 p))
   ;; For Bartlett decomposition
   ;; http://en.wikipedia.org/wiki/Wishart_distribution#Bartlett_decomposition
   Z (delay (+ (* 0.5 n p (Math/log 2))
               (* 0.5 n (Math/log (m/det V)))
               (log-mv-gamma-fn p (* 0.5 n))))
   transform-sample
   (fn [A]
     (let
       [LA (m/mmul L A)]
       (m/mmul LA (m/transpose LA))))]
  (sample [this]
          ;; Bartlett decomposition
          ;; http://en.wikipedia.org/wiki/Wishart_distribution#Bartlett_decomposition
          ;; and https://stat.duke.edu/~km68/materials/214.9%20%28Wishart%29.pdf
          (let
            [A (gen-matrix
                (fn [row column]
                  (cond
                   (= row column) (sqrt (sample (get chi-squared-dists row)))
                   (> row column) (sample unit-normal)
                   :else 0.0))
                p p)]
            (transform-sample A)))
  (observe [this value]
           (- (* 0.5 (- n p 1) (Math/log (m/det value)))
              (* 0.5 (m/trace (m/mmul (m/inverse (m/matrix V)) value)))
              @Z))
  multivariate-distribution
  (transform-sample [this A] (transform-sample A)))

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
  (let [[docstring parameters & args]
        (if (string? (first args))
          args
          `(~(format "%s random process" name) ~@args))
        [bindings & methods]
        (if (vector? (first args))
          args
          `[[] ~@args])
        record-name (symbol (format "%s-process" name))
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
         (print-simple (str ~'o) ~'m)))))

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

(defdist categorical-crp
  "categorical distribution extended
  by a random sample, for use with CRP"
  [counts alpha] [dist (categorical
                         (vec (conj counts [::new alpha])))]
  (sample [this] 
    (let [s (sample dist)]
      (if (= s ::new) 
        ;; When a `new' value is drawn,  sample 
        ;; the smallest natural number with zero count.
        (loop [s 0]
          (if (contains? counts s)
            (recur (inc s))
            s))
        s)))

  (observe [this value]
    (if (contains? counts value)
      ;; The value is one of absorbed values.
      (observe dist value)
      ;; The value is a new value.
      (observe dist ::new))))

(defproc CRP
  "Chinese Restaurant process"
  [alpha] [counts {}]
  (produce [this] (categorical-crp counts alpha))
  (absorb [this sample] 
    (CRP alpha (update-in counts [sample] (fnil inc 0)))))

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

(defproc dirichlet-discrete
  "Dirichlet-discrete process."
  [counts]
  (produce [this]
    (discrete counts)) 
  (absorb [this x]
    (dirichlet-discrete
      (assoc counts 
        x (inc (get counts x))))))

(defn- mvn-niw-posterior
  "Returns the posterior parameters for a mvn-niw process."
  [mu kappa nu psi n sum-x sum-x-sq]
  (let [dim (m/ecount mu)
        mean-x (if (> n 0) (m/div sum-x n) sum-x)
        n-cov-x (if (> n 0) 
                  (m/sub sum-x-sq 
                           (m/outer-product sum-x mean-x))
                  sum-x-sq)
        delta-x (m/sub mean-x mu)
        kappa-post (+ kappa n)
        nu-post (+ nu n)
        mu-post (m/div
                 (m/add sum-x (m/mul mu kappa))
                 kappa-post)
        psi-post (m/add
                   psi 
                   n-cov-x
                   (m/mul
                    (m/outer-product delta-x delta-x)
                    (/ (* kappa n) kappa-post)))]
    [mu-post kappa-post nu-post psi-post]))

(defn- mvn-niw-predictive
  "Returns the parameters for the predictive distribution 
  of a mvn-niw-process, which is a multivariate-t."
  [mu kappa nu psi]
  (let [t-nu (- (inc nu) (m/ecount mu))
        t-mu mu
        t-sigma (m/mul psi (/ (inc kappa) (* kappa t-nu)))]
    [t-nu t-mu t-sigma]))

(defproc mvn-niw
  "Multivariate normal with unknown mean and covariance
  matrix. Parameterized using a normal inverse-Wishart."
  [mu kappa nu psi]
  [n 0
   sum-x (m/zero-vector (first (m/shape psi)))
   sum-x-sq (apply m/zero-matrix (m/shape psi))]
  (produce 
   [this]
   (apply multivariate-t
          (apply mvn-niw-predictive
                 (mvn-niw-posterior
                   mu kappa nu psi n sum-x sum-x-sq))))
  (absorb [this x]
   (let [x (m/matrix x)]
     (mvn-niw mu kappa nu psi
              (inc n) 
              (m/add sum-x x)
              (m/add sum-x-sq (m/outer-product x x))))))

;;; Add semantic tags to different distribution objects

(defn tag [id dist]
  (vary-meta dist assoc :anglican-tag id))

(defn get-tag [dist]
  (:anglican-tag (meta dist)))
