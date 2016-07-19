(ns anglican.dist-test
  (:require [clojure.test :refer :all]
            [clojure.core.matrix :as mat]
            [anglican.runtime :refer :all]
            [anglican.stat :as stat]))

(defn- normal-cdf 
  [mu sigma x]
  (* 0.5 (+ 1 (erf (/ (- x mu)
                      (* (sqrt 2) sigma))))))

(defn- chi-squared-cdf 
  [nu x]
  (let [d (org.apache.commons.math3.distribution.ChiSquaredDistribution. (double nu))]
    (.cumulativeProbability d (double x))))

(defn- sqr 
  [x] 
  (* x x))

(defn- ess 
  "calculates the effective sample size, defined as the ratio of the
  square of the sum and the sum of the squares"
  [log-weights]
  (let [max-log-weight (reduce max log-weights)
        weights (map #(Math/exp (- % max-log-weight))
                     log-weights)]
    (if (seq weights)
      (let [sum-w (reduce + weights)
            sum-sq-w (reduce + (map #(* % %) weights))]
        (/ (* sum-w sum-w) sum-sq-w))
      0.0)))

(defn- upper-diag 
  "returns a vector of upper diagonal elements in a matrix"
  [m]
  (let [[k l] (mat/shape m)]
    (assert (= k l)
            "matrix m must be a square matrix")
    (mat/matrix (mapcat (partial mat/diagonal m) (range k)))))

(defprotocol DistributionMoments
  (mean [self])
  (variance [self])
  (covariance [self]))

(extend-protocol DistributionMoments
  anglican.runtime.bernoulli-distribution
  (mean [d]
    (:p d))
  (variance [d]
    (* (:p d) (- 1.0 (:p d))))
  anglican.runtime.beta-distribution
  (mean [d] 
    (let [a (:alpha d)
          b (:beta d)]
      (/ a (+ a b))))
  (variance [d] 
    (let [a (:alpha d)
          b (:beta d)]
      (/ (* a b)
         (* (sqr (+ a b))
            (+ a b 1)))))
  anglican.runtime.binomial-distribution
  (mean [d]
    (* (:n d) (:p d)))
  (variance [d]
    (* (:n d) (:p d) (- 1.0 (:p d))))
  anglican.runtime.chi-squared-distribution
  (mean [d]
    (:nu d))
  (variance [d]
    (* 2 (:nu d)))
  anglican.runtime.discrete-distribution
  (mean [d]
    (let [ws (:weights d)
          ks (range (count ws))
          sum-w (:total-weight d)]
      (/ (reduce + (map * ws ks))
         sum-w)))
  (variance [d]
    (let [ws (:weights d)
          k2s (map sqr (range (count ws)))
          sum-w (:total-weight d)]
      (- (/ (reduce + (map * ws k2s))
            sum-w)
         (sqr (mean d)))))
  anglican.runtime.exponential-distribution
  (mean [d]
    (/ 1.0 (:rate d)))
  (variance [d]
    (/ 1.0 (sqr (:rate d))))
  anglican.runtime.gamma-distribution
  (mean [d]
    (let [a (:shape d)
          b (:rate d)]
      (/ a b)))
  (variance [d]
    (let [a (:shape d)
          b (:rate d)]
      (/ a (sqr b))))
  anglican.runtime.multivariate-t-distribution
  (mean [d]
    (:mu d))
  (variance [d]
    (mat/diagonal (covariance d)))
  (covariance [d]
    (mat/mul (/ (:nu d)
                (- (:nu d) 2))
             (:sigma d)))
  anglican.runtime.mvn-distribution
  (mean [d]
    (:mean d))
  (variance [d]
    (mat/diagonal (covariance d)))
  (covariance [d]
    (:cov d))
  anglican.runtime.normal-distribution
  (mean [d] 
    (:mean d))
  (variance [d] 
    (sqr (:sd d)))
  anglican.runtime.poisson-distribution
  (mean [d]
    (:lambda d))
  (variance [d]
    (:lambda d))
  anglican.runtime.uniform-continuous-distribution
  (mean [d]
    (* 0.5 (+ (:min d) (:max d))))
  (variance [d]
    (/ (sqr (- (:max d) (:min d)))
       12.0))
  anglican.runtime.uniform-discrete-distribution
  (mean [d]
    (* 0.5 (+ (:min d) (dec (:max d)))))
  (variance [d]
    (/ (sqr (- (:max d) (:min d)))
       12.0))
  anglican.runtime.wishart-distribution
  (mean [d]
    (mat/mul (:V d) (:n d)))
  (variance [d]
    (let [V (:V d)
          v (mat/diagonal V)
          n (:n d)]
      (mat/mul (mat/add (mat/mul V V) 
                        (mat/outer-product v v))
               n))))

(defn sample-moments 
  [d num-samples]
  (let [samples (repeatedly num-samples #(sample* d))
        m (stat/mean samples)
        c (if (mat/matrix? m)
            (if (mat/symmetric? m)
              (stat/covariance (map upper-diag samples))
              (stat/covariance (map mat/to-vector samples)))
            (stat/covariance samples))]
    [m c]))

(defn importance-moments
  [p-dist q-dist num-samples]
  (let [samples (repeatedly num-samples #(sample* q-dist))
        log-weights (map (fn [x] 
                           (- (observe* p-dist x)
                              (observe* q-dist x)))
                         samples)
        weighted (map vector samples log-weights)
        m (stat/empirical-mean weighted)
        c (if (mat/matrix? m)
            (if (mat/symmetric? m)
              (stat/empirical-covariance 
               (map #(update-in % [0] upper-diag) 
                    weighted))
              (stat/empirical-covariance 
               (map #(update-in % [0] mat/to-vector) 
                    weighted)))
            (stat/empirical-covariance weighted))]
    [m c (ess log-weights)]))

(defn sample-mean-quantile
  [d num-samples]
  (let [m (mean d)
        [sm sc] (sample-moments d num-samples)]
    (cond 
      (mat/scalar? m)
      ;; return the distance from the 0.5 quantile under CLT assumption
      (let [v (variance d)
            q (normal-cdf m (sqrt (/ v num-samples)) sm)]
        [(/ (abs (- q 0.5)) 0.5) m sm sc])
      ;; return the quantile on the mahalabonis distance
      (mat/vec? m)
      (let [s (covariance d)
            dm (mat/sub sm m)
            r2 (mat/mmul dm 
                         (mat/mul (mat/inverse s) 
                                  num-samples) 
                         dm)
            q (chi-squared-cdf (mat/ecount m) r2)]
        [q m sm sc])
      ;; return quantile on mahalabonis distance for flattened mean
      (mat/matrix? m)
      (let [dm (if (mat/symmetric? m)
                 (mat/sub (upper-diag m)
                          (upper-diag sm))
                 (mat/sub (mat/to-vector m)
                          (mat/to-vector sm)))
            r2 (mat/mmul dm 
                          (mat/mul (mat/inverse sc) 
                                   num-samples) 
                          dm)
            q (chi-squared-cdf (mat/ecount m) (mat/mget r2))]
        [q m sm sc]))))

(defn importance-mean-quantile
  [p q num-samples]
  (let [m (mean p)
        [sm sc eff-samples] (importance-moments p q num-samples)]
    (cond
      (mat/scalar? m)
      ;; return the distance from the 0.5 quantile under CLT assumption
      (let [f (normal-cdf m (sqrt (/ sc eff-samples)) sm)]
        [(/ (abs (- f 0.5)) 0.5) m sm sc eff-samples])
      (mat/vec? m)
      ;; return the quantile on the mahalabonis distance
      (let [dm (mat/sub sm m)
            r2 (mat/mmul dm 
                         (mat/mul (mat/inverse sc) 
                                  eff-samples) 
                         dm)
            quantile (chi-squared-cdf (mat/ecount m) r2)]
        [quantile m sm sc eff-samples])
      (mat/matrix? m)
      ;; quantile on mahalabonis distance for flattened mean
      (let [dm (if (mat/symmetric? m)
                 (mat/sub (upper-diag m)
                          (upper-diag sm))
                 (mat/sub (mat/to-vector m)
                          (mat/to-vector sm)))
            r2 (mat/mmul dm 
                          (mat/mul (mat/inverse sc) 
                                   eff-samples) 
                          dm)
            q (chi-squared-cdf (mat/ecount dm) (mat/mget r2))]
        [q m sm sc eff-samples]))))

(def test-dists
  {'bernoulli 
   [(bernoulli 0.7)
    (bernoulli 0.4)]
   'beta
   [(beta 3.2 2.2)
    (beta 1.0 1.0)]
   'binomial
   [(binomial 5 0.7)
    (binomial 5 0.4)]
   'chi-squared
   [(chi-squared 5)
    (chi-squared 3)]
   'discrete
   [(discrete [1.0 3.0 2.0])
    (discrete [1.0 2.0 1.5])]
   'exponential
   [(exponential 3.1)
    (exponential 1.4)]
   'gamma
   [(gamma 2.1 1.4)
    (gamma 1.0 1.0)]
   'multivariate-t
   [(multivariate-t 5.0 
                    [0.5 0.2] 
                    [[0.1 0.01]
                     [0.01 0.1]])
    (multivariate-t 4.0
                    [0.0 0.0] 
                    [[1.0 0.1] 
                     [0.1 1.0]])]
   'mvn
   [(mvn [0.5 -0.2] [[0.5 0.2] [0.3 0.4]])
    (mvn [0.0 0.0] [[1.0 0.5] [0.5 1.0]])]
   'normal
   [(normal 0.3 0.1)
    (normal 0.1 1.0)]
   'poisson
   [(poisson 2.1)
    (poisson 3.2)]
   'uniform-continuous
   [(uniform-continuous 0.3 1.2)
    (uniform-continuous 0.2 2.0)]
   'uniform-discrete 
   [(uniform-discrete 2 5)
    (uniform-discrete 0 6)]
   'wishart
   [(wishart 100 [[0.1 0.01] [0.01 0.1]])
    (wishart 10 [[1.0 0.1] [0.1 1.0]])]})

(deftest test-sample-mean
  (doseq [[s [p-dist _]] test-dists]
    (testing (symbol (str s "-distribution"))
      (let [[q m sm sc] (sample-mean-quantile p-dist 1000)]
        (is (< q 0.99)
            (str "sample mean does not fall within 99% quantile "
                 "predicted from central limit theorem. "
                 "mean: " m ", sample mean: " sm 
                 "sample covariance: " sc))))))

(deftest test-importance-mean
  (doseq [[s [p-dist q-dist]] test-dists]
    (testing (symbol (str s "-distribution"))
      (let [[q m sm sc eff-samples] (importance-mean-quantile p-dist q-dist 1000)]
        (is (< q 0.99)
            (str "importance sampling mean does not fall within 99% "
                 "quantile predicted from central limit theorem. "
                 "mean: " m ", sample mean: " sm ", sample covariance: " sc 
                 ", effective sample size: " eff-samples)))))) 
