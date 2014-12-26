(ns angsrc.gp
  (:use [embang emit runtime]))

;;; Gaussian process example

(def data [[0.0 0.5]
           [1.0 0.4]
           [2.0 0.2]
           [3.0 -0.05]
           [4.0 -0.2]
           [5.0 0.1]])

(defanglican gp
  [assume belief (normal 0 1)]
  [assume positive-belief (gamma 2 2)]

  ; priors on process parameters
  [assume a (sample belief)]
  [assume b (sample belief)]
  [assume c (sample belief)]
  [assume d (sample positive-belief)]
  [assume e (sample positive-belief)]
  
  [assume m (lambda (x)
              (+ c (* x (+ b (* x a)))))]
  [assume k (lambda (x y)
              (let ((dx (- x y)))
                (* d (exp (- (/ (* dx dx) 2. e))))))]

  [assume gp
   (reduce (lambda (gp point)
             (let ((d (produce gp)))
               [observe (d (first point)) (second point)]
               (absorb gp point)))
           (GP m k) data)]

  ;; predict hyper-parameters
  [predict (list a b c d e)]

  ;; interpolate
  [assume d (produce gp)]
  [predict (map (lambda (point)
                  (sample (d (- 0.5 (first point)))))
                (next data))])
