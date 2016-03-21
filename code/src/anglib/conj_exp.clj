(ns anglib.conj-exp
  (require [anglican.runtime :as run 
            :refer [defdist defproc distribution 
                    sample observe 
                    produce absorb
                    chi-squared discrete gamma mvn wishart 
                    log-gamma-fn]]
           [anglican.emit :as emit]
           [anglican.stat :as stat]
           [clojure.core.matrix :as mat]))

(defdist multivariate-t
  "Multivariate T-distribution."
  ; Implemented according to http://en.wikipedia.org/wiki/Multivariate_t-distribution
  [nu mu sigma]
  [dim (mat/ecount mu)
   mvn-dist (mvn (mat/zero-vector dim) sigma)
   chi-sq-dist (chi-squared nu)]
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

(defproc dirichlet-discrete
  "Dirichlet-discrete process."
  [counts]
  (produce [this]
    (discrete counts)) 
  (absorb [this x]
    (dirichlet-discrete
      (assoc counts 
        x (inc (get counts x))))))

