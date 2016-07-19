(ns anglican.runtime-test
  (:require 
    [clojure.test :refer [deftest testing is]]
    [clojure.core.matrix :as mat]
    [anglican.stat :as stat])
  (:use anglican.runtime))

(deftest test-categorical
  (testing "categorical"
    (let [dist (categorical '((x 1) (y 2)))]
      (is (= (observe* dist 'x) (Math/log (/ 1. 3.)))
          "observing value in support")
      (is (= (observe* dist 'z) (Math/log 0.))
          "observing value not in support"))))

(deftest test-uniform-discrete
  (testing "uniform-discrete"
    (let [dist (uniform-discrete 0 3)]
      (is (= (observe* dist 1) (Math/log (/ 1. 3.)))
          "values in domain are uniformly distributed")
      (is (= (observe* dist 3) (Math/log 0.))
          "upper bound is not in the domain")
      (is (= (observe* dist -1) (Math/log 0.))
          "values not in the range have zero probability"))))

(defn approx
  [x y eps]
  (and (> x (- y eps))
       (< x (+ y eps))))

(deftest test-mvn
  (testing "mvn"
    (let [dist (mvn [0 0 0] [[2 0 0] [0 1 0] [0 0 3]])]
      (is (approx (observe* dist [3 4 5]) -18.0694 0.1))
      (is (approx (observe* dist [0 0 0]) -3.6527 0.1))
      (is (approx (observe* dist [10 20 30]) -378.6527 0.1)))))

(deftest test-CRP
  (testing  "CRP"
    (let [proc (CRP 0.1 {1 1 2 2 3 3})]
      (is (= (observe* (produce proc) 3) 
             (Math/log (/ 3 6.1)))
          "observing existing value")
      (is (= (observe* (produce proc) 0) 
             (Math/log (/ 0.1 6.1)))
          "observing new value")
      (is (= (observe* (produce (absorb proc 3)) 3) 
             (Math/log (/ 4 7.1)))
          "observing absorbed existing value")
      (is (= (observe* (produce (absorb proc 0)) 1) 
             (Math/log (/ 1 7.1)))
          "observing absorbed new value greater than count")
      (is (= (observe* (produce (absorb proc 1)) 0) 
             (Math/log (/ 0.1 7.1)))
          "observing unabsorbed value less than count"))))

(deftest test-cov
  (testing "cov"
    (is (= (cov + [1 2] [1 3]) [[2 4] [3 5]])
        "square matrix")
    (is (= (cov * [1 2] [3]) [[3] [6]])
        "vector to scalar")
    (is (= (cov str "c" ["a" "b"]) [["ca" "cb"]])
        "scalar to vector")))

(defn- within 
  [a b e]
  (-> (mat/sub a b)
      mat/abs
      (mat/le e)
      mat/to-vector
      stat/sum
      (mat/equals (mat/ecount a))))

(defn- multi-sample 
  [n proc]
  (loop [samples []
         proc proc]
    (if (>= (count samples) n)
      samples
      (let [x (sample* (produce proc))]
        (recur (conj samples x)
               (absorb proc x))))))

(deftest test-multivariate-t
  (testing "sampling"
    (let [nu 5.0
          mu (mat/matrix [3.0 1.0])
          sigma (mat/matrix [[1.0 0.1] [0.1 1.0]])
          t-dist (multivariate-t nu mu sigma)
          samples (repeatedly 10000 #(sample* t-dist))]
      (is (within (stat/mean samples) mu (mat/mul 0.2 mu))
          "sample mean does not fall within 20% of expected value")
      (is (within (mat/mul 
                   (stat/covariance samples) 
                   (/ (- nu 2) nu))
                  sigma
                  (mat/mul 0.5 sigma))
          "sample covariance does not fall within 50% of expected value"))))
         
(defn sample-mvn-niw-generative 
  [num-params num-samples mu kappa nu psi]
  (let [prec-prior (wishart nu (mat/inverse psi))
        sigmas (repeatedly 
                  num-params 
                  #(mat/inverse (sample* prec-prior)))
        means (map (fn [sigma] 
                     (sample* (mvn mu (mat/div sigma kappa))))
                   sigmas)
        likes (map mvn means sigmas)]
    (reduce (fn [[m c] like]
              (let [xs (repeatedly num-samples 
                                   #(sample* like))]
                [(mat/add m (mat/div (stat/mean xs) num-params))
                 (mat/add c (mat/div (stat/covariance xs) num-params))]))
            [(mat/zero-vector 2)
             (mat/zero-matrix 2 2)]
            likes)))

(defn sample-mvn-niw-collapsed
  [num-params num-samples mu kappa nu psi]
  (reduce (fn [[m c] proc]
            (let [xs (multi-sample 
                      num-samples 
                      proc)]
              [(mat/add m (mat/div (stat/mean xs) num-params))
               (mat/add c (mat/div (stat/covariance xs) num-params))]))
          [(mat/zero-vector 2)
           (mat/zero-matrix 2 2)]
          (repeat num-params (mvn-niw mu kappa nu psi))))

(deftest test-mvn-niw
  (let [num-params 100
        num-samples 100
        mu (mat/matrix [2.0 3.0])
        kappa 12.0
        nu 7.0
        psi (mat/mul
             (mat/matrix 
              [[1.0 0.1] 
               [0.1 1.0]]) 
             nu)
        [mug cg] (sample-mvn-niw-generative 
                    num-params num-samples
                    mu kappa nu psi)
        [muc cc] (sample-mvn-niw-collapsed
                    num-params num-samples
                    mu kappa nu psi)]
    (is (within mug muc 
                  (mat/mul 0.125 (mat/add mug muc)))
        "empirical mean from mvn-niw samples is not within 25% of 
        empirical mean from uncollapsed generative process")
    (is (within cg cc
                (mat/mul 0.125 (mat/outer-product mug muc)))
        "empirical covariance from mvn-niw samples is not within 25% of 
        empirical covariance from uncollapsed generative process")))
