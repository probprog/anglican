(ns anglican.math
  "Implements standard mathematical functions which we need,
   but may not want to include in the runtime.")

(defn digamma
  "digamma function psi(x): derivative of gammaln(x),
  apparently bizarrely missing from all Clojure libraries.
  Not yet implemented for negative values of x.
  source: http://en.wikipedia.org/wiki/Digamma_function"
  [x]
  (assert (>= x 0.0))
  (if (<= x 0.0)
    (Math/log 0.0)
    (let [approximating-series [#(Math/log %)
                                #(/ -1. (* 2 %))
                                #(/ -1. (* 12 (Math/pow % 2)))
                                #(/ 1. (* 120 (Math/pow % 4)))
                                #(/ -1. (* 252 (Math/pow % 6)))
                                #(/ 1. (* 240 (Math/pow % 8)))
                                #(/ -5. (* 660 (Math/pow % 10)))
                                #(/ 691. (* 32760 (Math/pow % 12)))
                                #(/ -1. (* 12 (Math/pow % 14)))]
          partial-sum (if (< x 1) (/ -1. x) 0.0)
          x (if (< x 1) (+ x 1.0) x)]
      (+ partial-sum (reduce + (for [func approximating-series] (func x)))))))


(defn isfinite?
  "is the numeric value x finite?
  returns false for Infinity, -Infinity, and NaN"
  [x] (> (/ 1. 0.) x (/ -1. 0.)))
