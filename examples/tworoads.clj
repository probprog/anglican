(ns tworoads
  (:use [anglican runtime emit]))

(def wc 0.5)
(def w1 0.4)
(def w2 0.6)
(def p1 0.6)
(def p2 0.1)
(def cost 1)

(defquery tworoads "two roads example"
  (let [o1 (sample (flip p1))
        o2 (sample (flip p2))
        q (sample (uniform-continuous 0. 1.))
        s (sample (flip q))]
    (observe (flip 1.0) (or o1 o2))

    (let [distance (if (sample (flip q))
                     (if o1 w1 (+ w2 wc))
                     (if o2 w2 (+ w1 wc)))]
      (observe (flip (exp (- (* cost distance)))) true)
      (predict distance)
      (predict q))))
