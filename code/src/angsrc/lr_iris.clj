(ns angsrc.lr-iris
  (:use [embang runtime emit]
        [angsrc iris-data]))

(defanglican lr-iris
  [assume dot-product
   (lambda (u v) 
     (if (= (count u) 0) 0 
       (+ (* (first u) (first v))
          (dot-product (rest u) (rest v)))))]
  [assume sigma (sqrt (sample (gamma 1 1)))]
  [assume b (repeatedly
              5 (lambda () (sample (normal 0. sigma))))]
  [assume z (lambda (x) 
              (/ 1. (+ 1. (exp (* -1. (dot-product b x))))))]
  (reduce (lambda (_ record)
            (observe (flip (z (cons 1 (butlast record))))
                     (= (last record) iris-setosa)))
          () iris-data)
  [predict b]
  ; should be Iris-setosa    (from training data)
  [assume x1 '( 1 5.1 3.5 1.4 0.2)]
  [predict (z x1)]
  ; should be Iris-virginica (from training data)
  [assume x2 '(1 7.7 2.6 6.9 2.3)]
  [predict (z x2)])
