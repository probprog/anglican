(ns angsrc.nomap
  (:use [embang emit runtime]))

(defanglican nomap
 [assume foo (lambda ()
               (+ (sample (normal 1 0.1))
                  (if (sample (flip 0.5)) (foo) 0.)))]
 [predict (foo)])
