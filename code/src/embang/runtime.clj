(ns embang.runtime
  (:require [embang.colt.distributions
             :as dist]
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
        unit-normal (normal 0 1)
        Z (delay (let [|Lcov| (reduce * (m/diagonal Lcov))]
                   (* 0.5 (+ (* k (Math/log (* 2 Math/PI)))
                             (Math/log |Lcov|)))))
        iLcov (delay (m/inverse Lcov))]
    (reify distribution
      (sample [this]
        (m/add mean
               (m/mmul Lcov
                       (repeatedly k #(sample unit-normal)))))
      (observe [this value]
        (let [dx (m/mmul @iLcov (m/sub value mean))]
          (- (* -0.5 (m/dot dx dx)) @Z))))))

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
  {:pre [(let [[n m] (m/shape V)] (= n m))
         (integer? n)
         (>= n (first (m/shape V)))]}
  (let [p (first (m/shape V))
        {L :L} (ml/cholesky (m/matrix V) {:return [:L]})
        unit-normal (normal 0 1)
        Z (delay (+ (* 0.5 n p (Math/log 2))
                    (* 0.5 n (Math/log (m/det V)))
                    (log-mv-gamma-fn p (* 0.5 n))))]
                    
    (reify distribution
      (sample [this]
        (let [X (repeatedly
                 n (fn [] (m/mmul L (repeatedly
                                     p #(sample unit-normal)))))]
          (m/mmul (m/transpose X) X)))
      (observe [this value]
        (- (* 0.5 (- n p 1) (Math/log (m/det value)))
           (* 0.5 (m/trace (m/mul (m/inverse (m/matrix V)) value)))
           @Z)))))

;;; Random processes

(defprotocol random-process
  "random process"
  (produce [this]
    "produces a static random source")
    
  (absorb [this sample]
    "absorbs the sample and returns a new process"))

;; Random processes can accept and return functions,
;; and translations in and out of CPS form must be
;; performed. To avoid mutual dependencies, we
;; define here two wrappers.

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
  ([alpha counts] {:pre [(vector? counts)]}
     (reify
       random-process
       (produce [this] (discrete (conj counts alpha)))
       (absorb [this sample] 
         (CRP alpha
              (update-in counts [sample] (fnil inc 0)))))))

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
