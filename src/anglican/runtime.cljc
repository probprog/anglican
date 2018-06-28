(ns anglican.runtime
  "Runtime library"
  (:require [clojure.string :as str]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.linear :as ml]

            #?(:cljs [anglican.util :refer [format]])
            #?(:cljs [goog.string.format])
            [net.cgrand.macrovich :as macros])
  #?(:cljs (:require-macros [net.cgrand.macrovich :as macros]
                           [anglican.runtime :refer [defdist defproc from-webppl]])))


;; matrix library uses vectorz for protocol implementations
#?(:clj (m/set-current-implementation :vectorz))


;;; Anglican core functions beyond clojure.core

(defn abs [x] (#?(:clj Math/abs :cljs js/Math.abs) x))
(defn floor [x] (#?(:clj Math/floor :cljs js/Math.floor) x))
(defn ceil [x] (#?(:clj Math/ceil :cljs js/Math.ceil) x))
(defn round [x] (#?(:clj Math/round :cljs js/Math.round) x))
(defn rint [x] (#?(:clj Math/rint :cljs js/Math.rint) x))
(defn signum [x] (#?(:clj Math/signum :cljs js/Math.sign) x))

(defn sin [x] (#?(:clj Math/sin :cljs js/Math.sin) x))
(defn cos [x] (#?(:clj Math/cos :cljs js/Math.cos) x))
(defn tan [x] (#?(:clj Math/tan :cljs js/Math.tan) x))
(defn asin [x] (#?(:clj Math/asin :cljs js/Math.asin) x))
(defn acos [x] (#?(:clj Math/acos :cljs js/Math.acos) x))
(defn atan [x] (#?(:clj Math/atan :cljs js/Math.atan) x))
(defn sinh [x] (#?(:clj Math/sinh :cljs js/Math.sinh) x))
(defn cosh [x] (#?(:clj Math/cosh :cljs js/Math.cosh) x))
(defn tanh [x] (#?(:clj Math/tanh :cljs js/Math.tanh) x))

(defn log [x] (#?(:clj Math/log :cljs js/Math.log) x))
(defn exp [x] (#?(:clj Math/exp :cljs js/Math.exp) x))
(defn cbrt [x] (#?(:clj Math/cbrt :cljs js/Math.cbrt) x))
(defn sqrt [x] (#?(:clj Math/sqrt :cljs js/Math.sqrt) x))
(defn pow [x y] (#?(:clj Math/pow :cljs js/Math.pow) x y))

(def PI #?(:clj Math/PI :cljs (.-PI js/Math)))

(defn finite?
  "is the numeric value x finite?
  returns false for Infinity, -Infinity, and NaN"
  [x] (> (/ 1. 0.) x (/ -1. 0.)))

;;; Special functions used in distributions

(defn erf
  "Error funciton. https://en.wikipedia.org/wiki/Error_function

  Maximum error in cljs version is 1.5*10^7"
  [x]
  #?(:clj (org.apache.commons.math3.special.Erf/erf x)
    :cljs
    (let [t (/ 1
               (+ 1 (* 0.3275911 (abs x))))
          a1 0.254829592
          a2 -0.284496736
          a3 1.421413741
          a4 -1.453152027
          a5 1.061405429
          poly-horner (* t (+ a1 (* t (+ a2 (* t (+ a3 (* t (+ a4 (* t a5)))))))))]
      (* (signum x) (- 1 (* poly-horner (exp (- (* x x)))))))))

;; TODO put into a test
(comment
  (erf -1))
;; => -0.8427006897475899
;; Wolfram Alpha -0.84270079294971486934122063508260925929606699796630

(defn log-gamma-fn
  "log Gamma function"
  [x]
  #?(:clj (org.apache.commons.math3.special.Gamma/logGamma x)
    :cljs :TODO))

(defn digamma
  "digamma function psi(x)"
  [x]
  (org.apache.commons.math3.special.Gamma/digamma x))

;;; Distributions

(defprotocol distribution
  "random distribution"
  (sample* [this]
    "draws a sample from the distribution")
  (observe* [this value]
    "return the probability [density] of the value"))

;; Log probabilities are used pervasively. A precision-preserving
;; way to add probabilities (e.g. for computing union probability)
;; is log-sum-exp.

(defn log-sum-exp
  "computes (log (+ (exp x) (exp y))) safely"
  [log-x log-y]
  (let [log-max (max log-x log-y)]
    (if (< (/ -1. 0.) log-max (/ 1. 0.))
      (+ log-max
         (log (+ (exp (- log-x log-max))
                 (exp (- log-y log-max)))))
      log-max)))

;; Distribution types, in alphabetical order.

#?(:clj
   (def RNG
     "random number generator;
  used by Apache Commons Math distribution objects"
   (org.apache.commons.math3.random.SynchronizedRandomGenerator.
    (org.apache.commons.math3.random.Well19937c.))))

;; Distributions are defined as records so that every
;; distribution has its own type. The distribution arguments
;; are available as the record fields.

(defn ^:private qualify
  "accepts a symbol, returns the qualified symbol;
  intended to be called from a macro"
  [s]
  (symbol (format "%s/%s" *ns* s)))


(macros/deftime
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
         (macros/case
             :clj
             (defrecord ~record-name [~@parameters ~@variables]
               Object
               (toString [~'this]
                 (str (list '~(qualify name) ~@parameters)))
               distribution
               ~@methods)
             :cljs
             (defrecord ~record-name [~@parameters ~@variables]
               distribution
               ~@methods))
         (defn ~name ~docstring ~parameters
           (let ~bindings
             (~(symbol (format "->%s" record-name))
              ~@parameters ~@variables)))
         (macros/case
           :clj
           (defmethod print-method ~record-name
             [~'o ~'m]
             (print-simple (str ~'o) ~'m))
           :cljs
           (extend-protocol cljs.core/IPrintWithWriter
             ~record-name
             (cljs.core/-pr-writer [o# writer# opts#]
               (cljs.core/-write writer# (str o#)))))))))

;; Many distributions are available in the Apache Commons Math library and
;; imported automatically.

(macros/deftime
  (defmacro ^:private from-apache
    "wraps Apache Commons Math distribution"
    [name args type [apache-name & apache-args]]
    (let [dist (gensym "dist")]
      `(defdist ~(symbol name)
         ~(format "%s distribution (imported from apache)" name)
         ~args
         [~dist (~(symbol (format "org.apache.commons.math3.distribution.%sDistribution." apache-name))
                 RNG ~@apache-args)]
         (~'sample* [~'this] (.sample ~dist))
         (~'observe* [~'this ~'value]
          ~(case type
             :discrete `(~'.logProbability ~dist ~'value)
             :continuous `(~'.logDensity ~dist ~'value))))))

  (defmacro ^:private from-webppl
    [name args [webppl-name & webppl-args]]
    (let [dist (gensym "dist")]
      `(defdist ~(symbol name)
         ~(format "%s distribution (imported from webppl)" name)
         ~args
         [~dist (new ~(symbol (str "js/dists." webppl-name))
                     (cljs.core/clj->js ~@webppl-args))]
         (~'sample* [~'this] (.sample ~dist))
         (~'observe* [~'this ~'value] (.score ~dist ~'value))))
    ))

(declare uniform-continuous)
(defdist bernoulli
  "Bernoulli distribution"
  [p] [dist (uniform-continuous 0.0 1.0)]
  (sample* [this] (if (< (sample* dist) p) 1 0))
  (observe* [this value]
    (log (case value
           1 p
           0 (- 1. p)
           0.))))

(macros/case
  :clj
  (from-apache beta [alpha beta] :continuous
               (Beta (double alpha) (double beta)))
  :cljs
  (from-webppl beta [alpha beta] (Beta {:a alpha :b beta})))

(macros/case
  :clj
  (from-apache binomial [n p] :discrete
               (Binomial (int n) (double p)))
  :cljs
  (from-webppl binomial [n p] (Binomial {:n n :p p})))

(declare discrete)
(defdist categorical
  "categorical distribution,
  convenience wrapper around discrete distribution;
  accepts a list of categories --- pairs [value weight]"
  [categories] [values (mapv first categories)
                index (into {} (map-indexed (fn [i v] [v i]) values))
                dist (discrete (map second categories))]
  (sample* [this] (values (sample* dist)))
  (observe* [this value] (observe* dist (index value -1))))

(declare uniform-continuous)
(defdist discrete
  "discrete distribution, accepts unnormalized weights"
  [weights] [total-weight (double (reduce + weights))
             dist (uniform-continuous 0. total-weight)]
  (sample* [this]
    (let [x (sample* dist)]
      (loop [[weight & weights] weights
             acc 0. value 0]
        (let [acc (+ acc weight)]
          (if (< x acc) value
            (recur weights acc (inc value)))))))
  (observe* [this value]
    (log
     (/ (nth weights value 0.0) total-weight))))

;; TODO port to cljs
#?(:clj
  (declare gamma))
#?(:clj
  (defdist dirichlet
    "Dirichlet distribution"
    ;; borrowed from Anglican runtime
    [alpha] [Z (delay (- (reduce + (map log-gamma-fn alpha))
                         (log-gamma-fn (reduce + alpha))))]
    (sample* [this]
             (let [g (map #(sample* (gamma % 1)) alpha)
                   t (reduce + g)]
               (map #(/ % t) g)))
    (observe* [this value]
              (- (reduce + (map (fn [v a] (* (Math/log v) (- a 1)))
                                value
                                alpha))
                 @Z))))

(macros/case
  :clj
  (from-apache exponential [rate] :continuous
               (Exponential (/ 1. (double rate))))
  :cljs
  (from-webppl exponential [rate] (Exponential {:a rate})))

(defdist flip
  "flip (Bernoulli boolean) distribution"
  [p] [dist (uniform-continuous 0.0 1.0)]
  (sample* [this] (< (sample* dist) p))
  (observe* [this value]
           (log (case value
                  true p
                  false (- 1. p)
                  0.))))

(macros/case
  :clj
  (from-apache gamma [shape rate] :continuous
               (Gamma (double shape) (/ 1.0 (double rate))))
  :cljs
  (from-webppl gamma [shape rate] (Gamma {:scale rate :shape shape})))

(defdist chi-squared
  "Chi-squared distribution. Equivalent to a gamma distribution
  with shape nu/2 and rate 1/2."
  [nu]
  [;; Chi-Squared(nu) ~ Gamma(shape = nu / 2, rate = 0.5).
   gamma-dist (gamma (* nu 0.5) 0.5)]
  (sample* [this] (sample* gamma-dist))
  (observe* [this value] (observe* gamma-dist value)))

(defprotocol multivariate-distribution
  "additional methods for multivariate distributions"
  (transform-sample [this samples]
    "accepts a vector of random values and generates
    a sample from the multivariate distribution"))

;; TODO port cholesky decomposition to al-jabr
#?(:clj
  (declare normal))
#?(:clj
  (defdist mvn
    "multivariate normal"
    [mean cov] [k (m/ecount mean)     ; number of dimensions
                Lcov (:L (ml/cholesky (m/matrix cov)))
                unit-normal (normal 0 1)
                Z (delay (let [|Lcov| (reduce * (m/diagonal Lcov))]
                           (+ (* 0.5 k (log (* 2 PI)))
                              (log |Lcov|))))
                iLcov (delay (m/inverse Lcov))
                transform-sample (fn [samples]
                                   (m/add mean (m/mmul Lcov samples)))]
    (sample* [this] (transform-sample
                     (repeatedly k #(sample* unit-normal))))
    (observe* [this value]
              (let [dx (m/mmul @iLcov (m/sub value mean))]
                (- (* -0.5 (m/dot dx dx)) @Z)))
    multivariate-distribution
    (transform-sample [this samples] (transform-sample samples))))

;; TODO port to cljs
#?(:clj
  (defdist multivariate-t
    "Multivariate T-distribution."
    ;; Implemented according to http://en.wikipedia.org/wiki/Multivariate_t-distribution
    [nu mu sigma]
    [dim (m/ecount mu)
     mvn-dist (mvn (m/zero-vector dim) sigma)
     chi-sq-dist (chi-squared nu)]
    (sample*
     [this]
     (let [y (sample* mvn-dist)
           u (sample* chi-sq-dist)]
       (m/add mu (m/mul y (sqrt (/ nu u))))))
    (observe*
     [this y]
     (let [dy (m/sub mu y)
           dy-sinv-dy (m/mget
                       (m/mmul
                        (m/transpose dy)
                        (m/inverse sigma)
                        dy))]
       (- (log-gamma-fn (* 0.5 (+ nu dim)))
          (+ (log-gamma-fn (* 0.5 nu))
             (* 0.5 dim (log nu))
             (* 0.5 dim (log PI))
             (* 0.5 (log (m/det sigma)))
             (* 0.5 (+ nu dim)
                (log
                 (+ 1.0
                    (* (/ 1.0 nu)
                       dy-sinv-dy))))))))))

(macros/case
  :clj
  (from-apache normal [mean sd] :continuous
               (Normal (double mean) (double sd)))
  :cljs
  (from-webppl normal [mean sd]
               (Gaussian {:mu mean :sigma sd})))

(macros/case
  :clj
  (from-apache laplace [loc scale] :continuous
               (Laplace (double loc) (double scale)))
  :cljs
  (from-webppl laplace [loc scale] (Laplace {:location loc :scale scale})))

(macros/case
  :clj
  (from-apache poisson [lambda] :discrete
               (Poisson (double lambda) 1E-12 10000000))
  :cljs
  (from-webppl poisson [lambda] (Poisson {:mu lambda})))

(macros/deftime
  :clj
  (from-apache student-t [nu] :continuous
    (T (double nu))))

;; TODO port to cljs
(macros/deftime
  :clj
  (defdist student-t-loc-scale [nu loc scale] [dist (student-t nu)]
    (sample* [this] (+ (* (sample* dist) scale) loc))
    (observe* [this value] (- (observe* dist (/ (- value loc) scale)) (Math/log scale)))))

(macros/case
  :clj
  (from-apache uniform-continuous [min max] :continuous
               (UniformReal (double min) (double max)))
  :cljs
  (defdist uniform-continuous [min max] []
    (sample* [this]
             (let [len (- max min)]
               (+ min (* len (rand)))))
    (observe* [this value]
              (if (<= min value  max)
                (log (/ 1 (- max min)))
                js/Number.NEGATIVE_INFINITY))))

(macros/case
  :clj
  (from-apache uniform-discrete [min max] :discrete
               (UniformInteger (int min) (dec (int max))))
  :cljs
  (defdist uniform-discrete [min' max'] [min (int min') max (int max')]
     (sample* [this]
              (+ (int min)
                 (rand-int (- max min))))
     (observe* [this value]
               (if (and (integer? value) (<= min value (dec max)))
                 (log (/ 1 (- max min)))
                 js/Number.NEGATIVE_INFINITY))))

;; TODO port to cljs
#?(:clj
  (defn log-mv-gamma-fn
    "multivariate Gamma function"
    [p a]
    (+ (* 0.25 p (- p 1) (log PI))
       (reduce + (map (fn [j]
                        (log-gamma-fn (- a (* 0.5 j))))
                      (range p))))))

(defn gen-matrix
  "creates a matrix, elements of which are initialised
  using the filler procedure f"
  [f rows columns]
  (m/reshape
    (for [r (range rows)
          c (range columns)]
      (f r c))
    [rows columns]))

;; TODO port to cljs
#?(:clj
  (defdist wishart
    "Wishart distribution"
    [n V]
    [p (first (m/shape V))
     #?(:cljs _) #?(:cljs (println "Warning: Cholesky not ported yet."))
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
    (sample* [this]
             ;; Bartlett decomposition
             ;; http://en.wikipedia.org/wiki/Wishart_distribution#Bartlett_decomposition
             ;; and https://stat.duke.edu/~km68/materials/214.9%20%28Wishart%29.pdf
             (let
                 [A (gen-matrix
                     (fn [row column]
                       (cond
                         (= row column) (sqrt (sample* (get chi-squared-dists row)))
                         (> row column) (sample* unit-normal)
                         :else 0.0))
                     p p)]
               (transform-sample A)))
    (observe* [this value]
              (- (* 0.5 (- n p 1) (Math/log (m/det value)))
                 (* 0.5 (m/trace (m/mmul (m/inverse (m/matrix V)) value)))
                 @Z))
    multivariate-distribution
    (transform-sample [this A] (transform-sample A))))

;;; Random processes

(defprotocol random-process
  "random process"
  (produce [this]
    "produces a static random source")

  (absorb [this sample]
    "absorbs the sample and returns a new process"))

(macros/deftime
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
         (macros/case
             :clj
           (defrecord ~record-name [~@parameters ~@variables]
             ~'Object
             (toString [~'this]
               (str (list '~(qualify name) ~@parameters)))
             random-process
             ~@methods)
           :cljs
           (defrecord ~record-name [~@parameters ~@variables]
             random-process
             ~@methods))
         (defn ~name ~docstring
           ;; Include parameters-only overload only if variables
           ;; are not empty.
           ~@(when (seq variables)
              `((~parameters (~name ~@parameters ~@values))))
           ([~@parameters ~@variables]
            (~(symbol (format "->%s" record-name))
             ~@parameters ~@variables)))
         (macros/case
             :clj
           (defmethod print-method ~record-name
             [~'o ~'m]
             (print-simple (str ~'o) ~'m))
           :cljs
           (extend-protocol cljs.core/IPrintWithWriter
             ~record-name
             (cljs.core/-pr-writer [o# writer# opts#]
               (cljs.core/-write writer# (str o#)))))))))

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
  (sample* [this]
    (let [s (sample* dist)]
      (if (= s ::new)
        ;; When a `new' value is drawn,  sample
        ;; the smallest natural number with zero count.
        (loop [s 0]
          (if (contains? counts s)
            (recur (inc s))
            s))
        s)))

  (observe* [this value]
    (if (contains? counts value)
      ;; The value is one of absorbed values.
      (observe* dist value)
      ;; The value is a new value.
      (observe* dist ::new))))

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
  (sample* [this]
    (let [s (sample* dist)]
      ;; When a `new' value is drawn, sample the actual
      ;; value from the base measure.
      (if (= s ::new) (sample* H) s)))
  (observe* [this value]
    (log-sum-exp
      ;; The value is one of absorbed values.
      (observe* dist value)
      ;; The value is drawn from the base distribution.
      (+ (observe* dist ::new) (observe* H value)))))

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

#?(:clj
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
                       (m/add sum-x-sq (m/outer-product x x)))))))

;;; Add semantic tags to different distribution objects

(defn tag [id dist]
  (vary-meta dist assoc :anglican-tag id))

(defn get-tag [dist]
  (:anglican-tag (meta dist)))

;;; Functions for computing sums, mean, variance, etc..

(defn power 
  "computes x^n for integer powers n by multiplication. vectorizes
  and preserves input types, at the expense of being slower than pow."
  [x n]
  (apply m/mul (repeat n x)))

(defn sum
  "sums array slices along specified dimension"
  ([a dimension]
   (reduce
     m/add
     (m/slices a dimension)))
  ([a]
   (sum a 0)))

(defn mean
  "mean of array slices along specified dimension"
  ([a dimension]
   (m/div (sum a dimension)
          (get (m/shape a) dimension)))
  ([a]
   (mean a 0)))

(defn variance
  "variance of array slices along specified dimension"
  ([a dimension]
   (let [e-a (mean a dimension)
         e-a2 (mean (power a 2) dimension)]
     (m/sub e-a2 (power e-a 2))))
  ([a]
   (variance a 0)))

(defn covariance
  "covariance of array slices along specified dimension"
  ([x dimension]
   (let [x-mean (mean x dimension)]
     (mean (map (fn [x-slice]
                  (let [dx-slice (m/sub x-slice x-mean)]
                    (m/outer-product dx-slice dx-slice)))
                (m/slices x dimension)))))
  ([x]
   (covariance x 0)))

(defn std
  "standard deviation (sqrt of variance) of array slices
  along specified dimension"
  ([a dimension]
   (m/sqrt (variance a dimension)))
  ([a]
   (std a 0)))

(defn skew
  "standardized skew of array slices along specified dimension"
  ([a dimension]
   (let [mu (mean a dimension)
         s (std a dimension)]
     (power (m/div (m/sub a mu) s) 3)))
  ([a]
   (skew a 0)))

(defn kurtosis
  "standardized skew of array slices along specified dimension"
  ([a dimension]
   (let [mu (mean a dimension)
         s (std a dimension)]
     (power (m/div (m/sub a mu) s) 4)))
  ([a]
   (kurtosis a 0)))

(defn l2-norm
  "calculates L2 norm (sum of squared differences) between a and b"
  [a b]
  (reduce +
          (map #(power (- %1 %2) 2)
               (m/to-vector a)
               (m/to-vector b))))

