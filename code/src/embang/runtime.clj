(ns embang.runtime
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

;;; Random distributions

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

;; distributions, in alphabetical order

(def ^:private RNG
  "random number generator;
  used by colt distribution objects"
  (embang.MTMersenneTwister. (java.util.Date.)))

(defmacro from-colt
  "wraps colt distribution"
  ([name args vtype]
   `(from-colt ~name ~args ~vtype (~(str/capitalize name) ~@args)))
  ([name args vtype [colt-name & colt-args]]
   `(defn ~(with-meta  name {:doc (str name " distribution")})
      ~args
      (let [~'dist (~(symbol (format "cern.jet.random.%s." colt-name))
                             ~@colt-args RNG)]
        (~'reify ~'distribution
          (~'sample [~'this] (~(symbol (format ".next%s"
                                               (str/capitalize vtype)))
                                       ~'dist))
          ~'(observe [this value] (log (.pdf dist value))))))))

(from-colt beta [alpha beta] double)
(from-colt binomial [n p] int)

(declare discrete)
(defn categorical
  "categorical distribution,
  convenience wrapper around discrete distribution;
  accepts a list of categories --- pairs [value weight]"
  [categories]
  (let [values (mapv first categories)
        weights (mapv second categories)
        indices (into {} (map-indexed (fn [i v] [v i]) values))
        dist (discrete weights)]
    (reify distribution
      (sample [this] (values (sample dist)))
      (observe [this value] (observe dist (indices value -1))))))

(defn discrete
  "discrete distribution, accepts unnormalized weights"
  [weights]
        
  (let [weights (mapv double weights)
        total-weight (reduce + weights)
        dist (cern.jet.random.Uniform. 0. total-weight RNG)]
    (reify distribution
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
            (catch IndexOutOfBoundsException _ 0.)))))))

(declare gamma) ; Gamma distribution used in Dirichlet distribution

(defn log-gamma-fn 
  "log gamma function"
  [x]
  (cern.jet.stat.Gamma/logGamma x))

(defn dirichlet
  "Diriclhet distribution"
  ;; borrowed from Anglican runtime
  [alpha]
  (let [Z (delay (- (reduce + (map log-gamma-fn alpha))
                    (log-gamma-fn (reduce + alpha))))]
    (reify distribution
      (sample [this]
        (let [g (map #(sample (gamma % 1)) alpha)
              t (reduce + g)]
          (map #(/ % t) g)))
      (observe [this value]
        (- (reduce + (map (fn [v a] (* (Math/log v) (- a 1))) 
                          value
                          alpha))
           @Z)))))

(from-colt exponential [rate] double)

(defn flip
  "flip (Bernoulli) distribution"
  [p]
  (let [dist (cern.jet.random.Uniform. RNG)]
    (reify distribution
      (sample [this] (< (.nextDouble dist) p))
      (observe [this value]
        (Math/log (case value
                    true p
                    false (- 1. p)
                    0.))))))

(from-colt gamma [shape rate] double)
(from-colt normal [mean sd] double)
(from-colt poisson [lambda] int)
(from-colt uniform-continuous [min max] double
           ;; The explicit type cast below is a fix to clojure
           ;; constructor matching (clojure.lang.Reflector.isCongruent).
           ;; If the constructor is overloaded with the same number of
           ;; arguments, clojure refuses to extend numeric types.
           (Uniform (double min) (double max)))

(defn uniform-discrete
  "uniform discrete distribution"
  [min max]
  {:pre [(integer? min) (integer? max)]}
  (let [dist (uniform-continuous min max)
        p (/ 1. (- max min))]
    (reify distribution
      (sample [this] (int (sample dist)))
      (observe [this value] 
        (Math/log 
          (if (and (integer? value)
                   (<= min value) (< value max))
            p 0.))))))

(defprotocol multivariate-distribution
  "additional methods for multivariate distributions"
  (transform-sample [this samples]
    "accepts a vector of random values and generates
    a sample from the multivariate distribution"))

(defn mvn
  "multivariate normal"
  [mean cov]
  (let [k (count mean)     ; number of dimensions
        {Lcov :L} (ml/cholesky (m/matrix cov) {:return [:L]})
        ;; delayed because used only by one of the methods
        unit-normal (normal 0 1)
        Z (delay (let [|Lcov| (reduce * (m/diagonal Lcov))]
                   (* 0.5 (+ (* k (Math/log (* 2 Math/PI)))
                             (Math/log |Lcov|)))))
        iLcov (delay (m/inverse Lcov))
        transform-sample (fn [samples]
                           (m/add mean (m/mmul Lcov samples)))]
    (reify distribution
      (sample [this] (transform-sample
                       (repeatedly k #(sample unit-normal))))
      (observe [this value]
        (let [dx (m/mmul @iLcov (m/sub value mean))]
          (- (* -0.5 (m/dot dx dx)) @Z)))

      multivariate-distribution
      (transform-sample [this samples] (transform-sample samples)))))

(defn log-mv-gamma-fn
  "multivariate gamma function"
  [p a]
  (+ (* 0.25 p (- p 1) (Math/log Math/PI))
     (reduce + (map (fn [j]
                      (log-gamma-fn (- a (* 0.5 j))))
                    (range p)))))

(defn wishart
  "Wishart distribution"
  ;; http://en.wikipedia.org/wiki/Wishart_distribution
  [n V] 
  {:pre [(let [[p q] (m/shape V)] (= p q))
         (integer? n)
         (>= n (first (m/shape V)))]}
  (let [p (first (m/shape V))
        {L :L} (ml/cholesky (m/matrix V) {:return [:L]})
        unit-normal (normal 0 1)
        Z (delay (+ (* 0.5 n p (Math/log 2))
                    (* 0.5 n (Math/log (m/det V)))
                    (log-mv-gamma-fn p (* 0.5 n))))
        transform-sample
        (fn [samples]
          (let [X (m/mmul L (m/reshape samples [p n]))]
            (m/mmul X (m/transpose X))))]
    (reify distribution
      (sample [this] (transform-sample
                       (repeatedly (* n p) #(sample unit-normal))))
      (observe [this value]
        (- (* 0.5 (- n p 1) (Math/log (m/det value)))
           (* 0.5 (m/trace (m/mul (m/inverse (m/matrix V)) value)))
           @Z))
      
      multivariate-distribution
      (transform-sample [this samples] (transform-sample samples)))))

;;; Random processes

(defprotocol random-process
  "random process"
  (produce [this]
    "produces a static random source")
    
  (absorb [this sample]
    "absorbs the sample and returns a new process"))

;; Random processes can accept and return functions,
;; and translations in and out of CPS form must be performed.
;; To avoid circular dependencies (emit<trap<runtime<emit),
;; we define two wrappers here.

(defn ^:private uncps
  "reconstructs value-returning function from CPS form"
  [f]
  (fn [& args]
    (apply f (fn [v _] v) nil args)))

(defn ^:private cps
  "wrap value-returning function into CPS form"
  [f]
  (fn [cont $state & args]
    (cont (apply f args) $state)))

;; random processes, in alphabetical order

(defn CRP
  "Chinese Restaurant process"
  ([alpha] (CRP alpha []))
  ([alpha counts]
   {:pre [(vector? counts)]}
   (reify
     random-process
     (produce [this]
       (let [dist (discrete (conj counts alpha))]
         (reify distribution
           (sample [this] (sample dist))
           (observe [this sample]
             ;; Observing any new sample has the same probability.
             (observe dist (min (count counts) sample))))))
     (absorb [this sample] 
       (CRP alpha
            (-> counts
                ;; Fill the counts with zeroes until the new sample.
                (into (repeat (+ (- sample (count counts)) 1) 0))
                (update-in [sample] inc)))))))

(defn DP
  "Dirichlet process"
  ([alpha H] (DP alpha H {}))
  ([alpha H counts]
   {:pre [(map? counts)]}
   (reify
     random-process
     (produce [this]
       ;; Sample from the categorical distribution of realized
       ;; samples extended with a `new' value.
       (let [dist (categorical (vec (conj counts [::new alpha])))]
         (reify distribution
           (sample [this] 
             (let [s (sample dist)]
               ;; When a `new' value is drawn, sample the actual
               ;; value from the base measure.
               (if (= s ::new) (sample H) s)))
           (observe [this sample]
             (log-sum-exp
               ;; The sample is one of absorbed samples.
               (observe dist sample)
               ;; The sample is drawn from the base distribution.
               (+ (observe dist ::new) (observe H sample)))))))
     (absorb [this sample]
       (DP alpha H (update-in counts [sample] (fnil inc 0)))))))

(defn cov
  "computes covariance matrix of xs and ys under k"
  [k xs ys]
  (for [x xs] (for [y ys] (k x y))))

(defn GP
  "Gaussian process"
  ([m$ k$]
     ;; two-parameter call is intended for
     ;; use from inside m! programs, where
     ;; CPS-transformed functions are passed
     (GP (uncps m$) (uncps k$) []))
  ([m k points]
     (reify random-process
       (produce [this]
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
         (GP m k (conj points sample))))))
