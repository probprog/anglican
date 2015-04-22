(ns lr-iris
  (:require [clojure.core.matrix :refer [dot]])
  (:use [anglican runtime emit]
        [anglib iris-data]))

(def iris-data-setosa (filter #(= (last %) :setosa) iris-data))
(def iris-data-not-setosa (filter #(not= (last %) :setosa) iris-data))

(assert (= (last (first iris-data-setosa)) :setosa))
(assert (not= (last (first iris-data-not-setosa)) :setosa))

(def test-setosa (rand-nth iris-data-setosa))
(def test-not-setosa (rand-nth iris-data-not-setosa))

(assert (= (last test-setosa) :setosa))
(assert (not= (last test-not-setosa) :setosa))

(with-primitive-procedures [dot]
  (defanglican lr-iris
	[assume features (lambda (record) (cons 1 (butlast record)))]
	;; choose random samples from the dataset, one from each class
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
    [predict (is-setosa (features test-setosa))]
    [predict (is-setosa (features test-not-setosa))]))
