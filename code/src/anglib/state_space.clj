(ns anglib.state-space
  (:require [clojure.core.matrix :as m])
  (:use [embang emit runtime]))

(defproc LDS
  "Linear Dynamical System (i.e. Kalman Filter). "
  [init-mean init-cov obs-matrix obs-cov trans-matrix trans-cov]
  [mu nil V nil]
  (produce [this]
     (if V
       ;; distribution on next observation
       (let [P (m/add trans-cov
                      (m/mmul trans-matrix
                              (m/mmul V
                                      (m/transpose trans-matrix))))]
         (mvn (m/mmul obs-matrix
                      (m/mmul trans-matrix mu))
              (m/add obs-cov
                     (m/mmul obs-matrix
                             (m/mmul P
                                     (m/transpose obs-matrix))))))
       ;; distribution on initial observation
       (mvn (m/mmul obs-matrix init-mean)
            (m/add obs-cov
                   (m/mmul obs-matrix
                           (m/mmul init-cov
                                   (m/transpose obs-matrix)))))))
  (absorb [this y]
     (if V
       ;; update mu and V (see Bishop 13.88-13.92)
       (let [P (m/add trans-cov
                      (m/mmul trans-matrix
                              (m/mmul V
                                      (m/transpose trans-matrix))))
             K (m/mmul P
                       (m/mmul (m/transpose trans-matrix)
                               (m/inverse
                                (m/add obs-cov
                                       (m/mmul obs-matrix
                                               (m/mmul P
                                                       (m/transpose obs-matrix)))))))
             Amu (m/mmul trans-matrix mu)
             mu (m/add Amu
                       (m/mmul K
                               (m/sub y
                                      (m/mmul obs-matrix
                                              Amu))))
             I (apply m/identity-matrix (m/shape K))
             V (m/mmul (m/sub I
                              (m/mmul K
                                      obs-matrix))
                       P)]
          (LDS init-mean init-cov
               obs-matrix obs-cov
               trans-matrix trans-cov
               mu V))
       ;; initialize mu and V (see Bishop 13.94-13.97)
       (let [K (m/mmul init-cov
                       (m/mmul (m/transpose obs-matrix)
                               (m/inverse
                                 (m/mmul obs-matrix
                                         (m/mmul init-cov
                                                 (m/transpose obs-matrix))))))
             mu (m/add init-mean
                       (m/mmul K
                               (m/sub y
                                      (m/mmul obs-matrix
                                              init-mean))))
             I (apply m/identity-matrix (m/shape K))
             V (m/mmul (m/sub I
                              (m/mmul K
                                      obs-matrix))
                       init-cov)]
         (LDS init-mean init-cov
              obs-matrix obs-cov
              trans-matrix trans-cov
              mu V)))))

(comment
  (let [proc (LDS [0. 0.]
                  [[1. 0.]
                   [0. 1.]]
                  [[0.9  0.1]
                   [-0.1 0.9]]
                  [[0.1   0.01]
                   [0.01  0.1]]
                  [[0.8  0.2]
                   [-0.2 0.8]]
                  [[1.0   0.1]
                   [0.1  1.0]])]
    (sample (produce (reduce absorb
                             proc
                             [[0. 0.]
                              [0.5 -0.5]])))))

