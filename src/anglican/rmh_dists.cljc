(ns anglican.rmh-dists
  "Alternative proposal distributions used in Random-walk Metropolis-Hastings (rmh)"
  (:use [anglican.runtime :only [observe* sample* defdist abs log exp erf normal sqrt round]]))

(defdist folded-normal
  "folded normal distribution
  (http://en.wikipedia.org/wiki/Folded_normal_distribution)"
  [mean std] [normal (normal mean std)]
  (sample* [this] (abs (sample* normal)))
  (observe* [this value]
           (if (< value 0)
             (log 0)
             (log (+ (exp (observe* normal value))
                     (exp (observe* normal (- value))))))))

(defdist folded-normal-positive
  "folded normal distribution
  (http://en.wikipedia.org/wiki/Folded_normal_distribution)
  with positive support: (0, infinity)"
  [mean std] [folded-normal (folded-normal mean std)]
  (sample* [this]
          (let [x (sample* folded-normal)]
            (if (= x 0.0) Double/MIN_VALUE x))) ; a bit dodgy...
  (observe* [this value]
           (if (<= value 0)
             (log 0)
             (if (= value Double/MIN_VALUE)
               (observe* folded-normal 0)
               (observe* folded-normal value)))))

;; Helper function for folded-normal-discrete
(defn normal-cdf
  [x mean std]
  "Cumulative probability density of a sample x drawn from a univariate normal
  with mean mean and standard deviation std"
  (+ 0.5
     (* 0.5 (erf (/ (- x mean)
                    (* (sqrt 2) std))))))

(defdist folded-normal-discrete
  "discrete version of the folded normal distribution
  (http://en.wikipedia.org/wiki/Folded_normal_distribution)"
  [mean std] [folded-normal (folded-normal mean std)]
  (sample* [this] (round (sample* folded-normal)))
  (observe* [this value]
           (let [x (double value)]
             (if (or (< x 0) ; negative
                     (not (= x (double (round x))))) ; not an integer
               (log 0)
               (if (= x 0.0)
                 ;; Integral of the bin centered at zero
                 (log (- (normal-cdf 0.5 mean std)
                         (normal-cdf (- 0.5) mean std)))
                 ;; Integral of the positive bin and the negative bin
                 (log (+ (- (normal-cdf (+ x 0.5) mean std)
                            (normal-cdf (- x 0.5) mean std))
                         (- (normal-cdf (+ (- x) 0.5) mean std)
                            (normal-cdf (- (- x) 0.5) mean std)))))))))
