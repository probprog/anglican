(ns angsrc.crp
  (:use [embang emit runtime]))

(defun crp (alpha)
  (let ((name (gensym)))
    (lambda ()
      (let ((p (or (retrieve name) (CRP alpha)))
            (s (sample (produce p))))
        (store name (absorb p s))
        s))))
