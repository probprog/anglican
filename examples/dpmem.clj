(ns dpmem
  (:use [embang emit runtime]))

;; Implementation of DPmem from
;;   http://www.robots.ox.ac.uk/~fwood/anglican/examples/dp_mixture_model/index.html
;; Used in dp-church.

;; sample-stick-index is a procedure that samples an index from
;; a potentially infinite dimensional discrete distribution 
;; lazily constructed by a stick breaking rule
(defun sample-stick-index (breaking-rule index)
  (if (sample (flip (breaking-rule index)))
    index
    (sample-stick-index breaking-rule (+ index 1))))

;; sethuraman-stick-picking-procedure returns a procedure that picks
;; a stick each time its called from the set of sticks lazily constructed
;; via the closed-over one-parameter stick breaking rule
(defun make-sethuraman-stick-picking-procedure (concentration)
  (define V (mem (lambda (x) (sample (beta 1.0 concentration)))))
  (lambda () (sample-stick-index V 1)))

;; DPmem is a procedure that takes two arguments -- the concentration
;; to a Dirichlet process and a base sampling procedure
;; DPmem returns a procedure 
(defun DPmem (concentration base)
  (define get-value-from-cache-or-sample
    (mem (lambda (args stick-index) 
                 (apply base args))))
  (define get-stick-picking-procedure-from-cache
    (mem (lambda (args) 
                 (make-sethuraman-stick-picking-procedure
                   concentration))))

  (lambda varargs
          ;; when the returned function is called, the first thing it does is get
          ;; the cached stick breaking procedure for the passed in arguments
          ;; and _calls_ it to get an index
          (begin (define index ((get-stick-picking-procedure-from-cache varargs)))
                 ;; if, for the given set of arguments and just sampled index
                 ;; a return value has already been computed, get it from the cache
                 ;; and return it, otherwise sample a new value
                 (get-value-from-cache-or-sample varargs index))))
