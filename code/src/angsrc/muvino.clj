(ns angsrc.muvino
  (:use [embang emit runtime]))

(defanglican muvino
  [assume x1 (sample (normal 1 1))]
  [assume x2 (sample (normal 1 1))]
  [assume x3 (sample (normal 1 1))]
  [assume x4 (sample (normal 1 1))]
  [assume x5 (sample (normal 1 1))]
  [assume x6 (sample (normal 1 1))]
  [assume x7 (sample (normal 1 1))]
  [assume x8 (sample (normal 1 1))]
  [assume x9 (sample (normal 1 1))]
  [assume x10 (sample (normal 1 1))]
  [assume x11 (sample (normal 1 1))]
  [assume x12 (sample (normal 1 1))]
  [assume x13 (sample (normal 1 1))]
  [assume x14 (sample (normal 1 1))]
  [assume x15 (sample (normal 1 1))]
  [assume x16 (sample (normal 1 1))]
  [assume x17 (sample (normal 1 1))]
  [assume z1 (+ (* 0.4 x1) (* 0.5 x2) (* 0.0001 x3))]
  [assume z2 (+ (* 0.6 x1) (* 0.2 x2) (* 0.0002 x3)
                (* 0.08 x5))]
  [predict z1]
  [predict z2])
