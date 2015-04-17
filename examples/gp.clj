(ns gp
  (:use [anglican emit runtime]))

;;; Gaussian process example

;; Observed points:
;;           x    y
(def data [[0.0  0.5 ]
           [1.0  0.4 ]
           [2.0  0.2 ]
           [3.0 -0.05]
           [4.0 -0.2 ]
           [5.0  0.1 ]])

(defanglican gp
  [assume belief (normal 0 1)]
  [assume positive-belief (gamma 1 1)]

  ;;; priors on process parameters

  ;; mean function parameters
  [assume a (sample belief)]
  [assume b (sample belief)]
  [assume c (sample belief)]

  ;; kernel function parameters
  [assume d (sample positive-belief)]
  [assume e (sample positive-belief)]
  
  ;; the mean function: ax²+bx+c
  [assume m (lambda (x)
              (+ c (* x (+ b (* x a)))))]

  ;; the kernel function: d·exp(-(x-y)²/2e)
  [assume k (lambda (x y)
              (let ((dx (- x y)))
                (* d (exp (- (/ (* dx dx) 2. e))))))]

  ;; observe and absorb each observation point
  [assume gp
   (reduce (lambda (gp point)
             (let ((d (produce gp)))
               [observe (d (first point)) (second point)]
               (absorb gp point)))
           (GP m k) data)]

  ;; predict parameters of the mean function only
  [predict a]
  [predict b]
  [predict c])

(defanglican noisy
  [assume belief (normal 0 1)]
  [assume positive-belief (gamma 2 2)]

  ; priors on process parameters
  [assume a (sample belief)]
  [assume b (sample belief)]
  [assume c (sample belief)]
  [assume d (sample positive-belief)]
  [assume f (sample positive-belief)]
  [assume g (sample positive-belief)]
  
  ;; the mean function: ax²+bx+c
  [assume m (lambda (x)
              (+ c (* x (+ b (* x a)))))]
  ;; the kernel function: d·exp(-(x-y)²/2f)+δg
  [assume k (lambda (x y)
              (let ((dx (- x y)))
                (+ (* d (exp (- (/ (* dx dx) 2. f))))
                   (if (= dx 0.) g 0.))))]

  [assume gp
   (reduce (lambda (gp point)
             (let ((d (produce gp)))
               [observe (d (first point)) (second point)]
               (absorb gp point)))
           (GP m k) data)]

  [predict a]
  [predict b]
  [predict c])
