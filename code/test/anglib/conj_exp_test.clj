(ns anglib.conj-exp-test
  (require [anglican.runtime :as run
            :refer [sample produce absorb
                    mvn wishart]]
           [anglican.stat :as stat]
           [clojure.core.matrix :as mat])
  (use anglib.conj-exp
       clojure.test))

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
      (let [x (sample (produce proc))]
        (recur (conj samples x)
               (absorb proc x))))))

(deftest test-multivariate-t
  (testing "sampling"
    (let [nu 5.0
          mu (mat/matrix [3.0 0.0])
          sigma (mat/matrix [[1.0 0.1] [0.1 1.0]])
          t-dist (multivariate-t nu mu sigma)
          samples (repeatedly 10000 #(sample t-dist))]
      ;; this should produce something close to [mu sigma]
      [(stat/mean samples) (mat/mul (stat/covariance samples) (/ (- nu 2) nu))])))
         
(defn sample-mvn-niw-generative 
  [num-params num-samples mu kappa nu psi]
  (let [prec-prior (wishart nu (mat/inverse psi))
        sigmas (repeatedly 
                  num-params 
                  #(mat/inverse
                    (mat/mul kappa 
                             (sample prec-prior))))
        means (map (fn [sigma] (sample (mvn mu sigma)))
                   sigmas)
        likes (map mvn means sigmas)]
    (reduce (fn [[m c] like]
              (let [xs (repeatedly num-samples 
                                   #(sample like))]
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
  (let [num-params 1000
        num-samples 100
        mu (mat/matrix [2.0 3.0])
        kappa 10.0
        nu 10.0
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
                  (mat/mul 0.05 (mat/add mug muc)))
        "empirical mean from mvn-niw samples is not within 10% of 
        empirical mean from uncollapsed generative process")
    (is (within cg cc
                (mat/mul 0.05 (mat/outer-product mug muc)))
        "empirical covariance from mvn-niw samples is not within 10% of 
        empirical covariance from uncollapsed generative process")))



