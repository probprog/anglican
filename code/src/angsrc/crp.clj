(ns angsrc.crp
  (:use [embang emit runtime]))

;; Stateful Chinese restaurant process, 
;; draws an index.

(defun crp (alpha)
  (let ((name (gensym "crp")))
    (lambda ()
      (let ((p (or (retrieve name) (CRP alpha)))
            (s (sample (produce p))))
        (store name (absorb p s))
        s))))

;; Dirichlet process, 
;; draws a value obtained from the base measure.

(defun dp (alpha H)
  (let ((C (crp alpha))
        (G (mem (lambda (s) (H)))))
    (lambda ()
      (G (C)))))

;; DPmem, whatever it means.

(defun dp-mem (alpha H)
  ((mem (lambda args
          (dp alpha (lambda () (apply H args)))))))
