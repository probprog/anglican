(ns anglib.conj-exp
  (require [anglican.runtime :as run 
            :refer [defdist defproc distribution 
                    sample observe 
                    produce absorb
                    discrete gamma mvn wishart 
                    log-gamma-fn]]
           [anglican.emit :as emit]
           [clojure.core.matrix :as mat]))

(defdist chi-sq
  "Chi-squared distribution. Equivalent to a gamma distribution with
  rate nu/2 and scale 1/2."
  [nu] [dist (gamma (* 0.5 nu) 0.5)]
  (sample [this] (sample dist))
  (observe [this value] (observe dist value)))

(defdist multivariate-t
  "Multivariate T-distribution."
  ; Implemented according to http://en.wikipedia.org/wiki/Multivariate_t-distribution
  [nu mu sigma]
  [dim (mat/ecount mu)
   mvn-dist (mvn (mat/zero-vector dim) sigma)
   chi-sq-dist (chi-sq nu)]
  (sample 
   [this]
   (let [y (sample mvn-dist)
         u (sample chi-sq-dist)]
     (mat/add mu (mat/mul y (Math/sqrt (/ nu u))))))
  (observe 
   [this y]
   (let [dy (mat/sub mu y)
         dy-sinv-dy (mat/mget
                     (mat/mmul
                      (mat/transpose dy)
                      (mat/inverse sigma)
                     dy))]
     (- (log-gamma-fn (* 0.5 (+ nu dim)))
        (+ (log-gamma-fn (* 0.5 nu))
           (* 0.5 dim (Math/log nu))
           (* 0.5 dim (Math/log Math/PI))
           (* 0.5 (Math/log (mat/det sigma)))
           (* 0.5 (+ nu dim)
              (Math/log
               (+ 1.0
                  (* (/ 1.0 nu)
                     dy-sinv-dy)))))))))

;; test code
#_(let [nu 5.0
      mu (mat/matrix [3.0 .0])
      sigma (mat/matrix [[1.0 0.1] [0.1 1.0]])
      t-dist (multivariate-t nu mu sigma)
      samples (repeatedly 10000 #(sample t-dist))]
  ;; this should produce something close to [mu sigma]
  [(mean samples) (mat/mul (covariance samples) (/ (- nu 2) nu))])

(defn- mvn-niw-posterior
  "Returns the posterior parameters for a mvn-niw process."
  [mu kappa nu psi n sum-x sum-x-sq]
  (let [dim (mat/ecount mu)
        mean-x (if (> n 0) (mat/div sum-x n) sum-x)
        n-cov-x (if (> n 0) 
                  (mat/sub sum-x-sq 
                           (mat/outer-product sum-x mean-x))
                  sum-x-sq)
        delta-x (mat/sub mean-x mu)
        kappa-post (+ kappa n)
        nu-post (+ nu n)
        mu-post (mat/div
                 (mat/add sum-x (mat/mul mu kappa))
                 kappa-post)
        psi-post (mat/add
                   psi 
                   n-cov-x
                   (mat/mul
                    (mat/outer-product delta-x delta-x)
                    (/ (* kappa n) kappa-post)))]
    [mu-post kappa-post nu-post psi-post]))

(defn- mvn-niw-predictive
  "Returns the parameters for the predictive distribution 
  of a mvn-niw-process, which is a multivariate-t."
  [mu kappa nu psi]
  (let [t-nu (- (inc nu) (mat/ecount mu))
        t-mu mu
        t-sigma (mat/mul psi (/ (inc kappa) (* kappa t-nu)))]
    [t-nu t-mu t-sigma]))

(defproc mvn-niw
  "Multivariate normal with unknown mean and covariance
  matrix. Parameterized using a normal inverse-Wishart."
  [mu kappa nu psi]
  [n 0
   sum-x (mat/zero-vector (first (mat/shape psi)))
   sum-x-sq (apply mat/zero-matrix (mat/shape psi))]
  (produce 
   [this]
   (apply multivariate-t
          (apply mvn-niw-predictive
                 (mvn-niw-posterior
                   mu kappa nu psi n sum-x sum-x-sq))))
  (absorb [this x]
   (let [x (mat/matrix x)]
     (mvn-niw mu kappa nu psi
              (inc n) 
              (mat/add sum-x x)
              (mat/add sum-x-sq (mat/outer-product x x))))))

;; test code
#_(let [num-params 1000
      num-samples 100
      mu (mat/matrix [2.0 3.0])
      kappa 10.0
      nu 10.0
      psi (mat/mul
           (mat/matrix 
            [[1.0 0.1] 
             [0.1 1.0]]) 
           nu)
      prec-prior (wishart nu (mat/inverse psi))
      sigmas (repeatedly 
               num-params 
               #(mat/inverse
                 (mat/mul kappa 
                          (sample prec-prior))))
      means (map (fn [sigma] (sample (mvn mu sigma)))
                 sigmas)
      likes (map mvn means sigmas)]
  (prn (mean sigmas))
  (reduce (fn [[m c] like]
            (let [xs (repeatedly num-samples 
                                 #(sample like))]
             [(mat/add m (mat/div (mean xs) num-params))
              (mat/add c (mat/div (covariance xs) num-params))]))
          [(mat/zero-vector 2)
           (mat/zero-matrix 2 2)]
          likes))

;; delete me - for testing
(defn- multi-sample 
  [n proc]
  (loop [samples []
         proc proc]
    (if (>= (count samples) n)
      samples
      (let [x (sample (produce proc))]
        (recur (conj samples x)
               (absorb proc x))))))
;; test code
#_(let [num-params 1000
      num-samples 100
      mu (mat/matrix [2.0 3.0])
      kappa 10.0
      nu 10.0
      psi (mat/mul
           (mat/matrix 
            [[1.0 0.1] 
             [0.1 1.0]]) 
           nu)]
  (reduce (fn [[m c] proc]
            (let [xs (multi-sample 
                        num-samples 
                        proc)]
             [(mat/add m (mat/div (mean xs) num-params))
              (mat/add c (mat/div (covariance xs) num-params))]))
          [(mat/zero-vector 2)
           (mat/zero-matrix 2 2)]
          (repeat num-params (mvn-niw mu kappa nu psi))))

(defproc dirichlet-discrete
  "Dirichlet-discrete process."
  [counts]
  (produce [this]
    (discrete counts)) 
  (absorb [this x]
    (dirichlet-discrete
      (assoc counts 
        x (inc (get counts x))))))

