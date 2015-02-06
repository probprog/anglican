(ns lr-iris
  (:require [clojure.core.matrix :refer [dot]])
  (:use [embang runtime emit]
        [angsrc iris-data]))

(def iris-data-setosa (filter #(= (last %) :setosa) iris-data))
(def iris-data-not-setosa (filter #(= (last %) :setosa) iris-data))
(defun random-nth (coll)
  (nth coll
       (sample (uniform-discrete 0 (count coll)))))

(with-primitive-procedures [dot]
  (defanglican lr-iris
    [assume features (lambda (record) (cons 1 (butlast record)))]
    ;; choose random samples from the dataset, one from each class
    [assume test-setosa (features (random-nth iris-data-setosa))]
    [assume test-not-setosa (features (random-nth iris-data-not-setosa))]
    ;; removing test data from the training set
    [assume iris-data (filter (lambda (record)
                                (not (or (= record test-setosa)
                                         (= record test-not-setosa))))
                              iris-data)]

    [assume sigma (sqrt (sample (gamma 1 1)))]
    [assume b (repeatedly
                5 (lambda () (sample (normal 0. sigma))))]
    [assume z (lambda (x) 
                (/ 1. (+ 1. (exp (* -1. (dot b x))))))]

    (reduce (lambda (_ record)
              (observe (flip (z (features record)))
                       (= (last record) iris-setosa)))
            () iris-data)

    [assume is-setosa (lambda (x) (sample (flip (z x))))]
    [predict (is-setosa test-setosa)]
    [predict (is-setosa test-not-setosa)]))
