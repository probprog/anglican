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

;; DPmem, memoizes calls to h softly.
;; h may get an arbitrary number of arguments.

(defun dp-mem (alpha h)
  (let ((C (crp alpha))
        (f (mem (lambda (s args) (apply h args)))))
    (lambda args
      (f (C) args))))
