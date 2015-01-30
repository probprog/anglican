(ns logi
  (:use [embang emit runtime]))

;;; Logistic regression

(defanglican logi
  "logistic regression"
  [assume m (sample (normal 0 1))]
  [assume sigma (sqrt (sample (gamma 1 1)))]
  [assume m (sample (normal 0 sigma))]
  [assume b (sample (normal 0 sigma))]
  [assume y (lambda (x)  (+ (* m x) b))]
  [assume z (lambda (x) (/ 1 (+ 1 (exp (* -1 (y x))))))]
  [observe (flip (z -10)) false]
  [observe (flip (z -5)) false]
  [observe (flip (z 2)) true]
  [observe (flip (z 6)) true]
  [observe (flip (z 10)) true]
  [predict (sample (flip (z 8)))])
