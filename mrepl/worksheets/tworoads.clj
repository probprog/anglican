;; gorilla-repl.fileformat = 1

;; **
;;; Boiler-plate code --- importing necessary things.
;; **

;; @@
(ns tworoads
  (:require [gorilla-plot.core :as plot]) ; visualizing results
  (:use [mrepl core]             ; performing inference and handling results
        [embang emit runtime]))  ; defining Anglican programs
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; # Two Roads
;;; 
;;; An inference example. We are given two roads with probability being open p1 and p2, costs of each road c1 and c2, and the cost of bumping into a blocked road cb. We want to learn the optimum policy --- with which probability to select each of the roads to pay less on average.
;;; 
;;; The Anglican code below defines the problem. We predict the policy (q) but not the distance, the distance prediction does not reflect the actual average distance. As Brooks says, "I have either a pound or nothing, guess right".
;; **

;; @@
(def c1 3)
(def c2 1)
(def cb 2)
(def p1 1/3)
(def p2 2/3)
(def gas-cost 100)

(defquery tworoads "two roads example"
  (let [o1 (sample (flip p1))
        o2 (sample (flip p2))
        q (sample (uniform-continuous 0. 1.))
        s (sample (flip q))]
    (observe (flip 1.0) (or o1 o2))

    (let [distance (if (sample (flip q))
                     (if o1 c1 (+ c2 cb))
                     (if o2 c2 (+ c1 cb)))]
      (observe (flip (exp (- (* gas-cost distance)))) true)
      (predict :q q))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tworoads/tworoads</span>","value":"#'tworoads/tworoads"}
;; <=

;; **
;;; Now we want to run (lazily some inference). We'll use LMH (Lightweight Metropolis-Hastings) for that.
;; **

;; @@
(def states (doquery :lmh tworoads nil))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tworoads/states</span>","value":"#'tworoads/states"}
;; <=

;; **
;;; It is the time to visualize and retrieve the results. First, decide how many samples we want to look at.
;; **

;; @@
(def N 10000)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tworoads/N</span>","value":"#'tworoads/N"}
;; <=

;; **
;;; Then, take these samples, extract the policy, and plot the policy distribution. Since in LMH all samples have the same weight, we'll ignore the weights and only keep the predicts.
;; **

;; @@
(def predicts (map get-predicts (take N states)))
(def policy (map :q predicts))
(plot/histogram policy)

;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"c32d26c9-5752-4761-a36a-094601bff497","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"c32d26c9-5752-4761-a36a-094601bff497","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"c32d26c9-5752-4761-a36a-094601bff497"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"c32d26c9-5752-4761-a36a-094601bff497","values":[{"x":2.7038343250751495E-4,"y":0},{"x":0.06637863665819169,"y":1012.0},{"x":0.13248688988387586,"y":1059.0},{"x":0.19859514310956003,"y":1219.0},{"x":0.2647033963352442,"y":1140.0},{"x":0.33081164956092834,"y":765.0},{"x":0.3969199027866125,"y":954.0},{"x":0.46302815601229663,"y":809.0},{"x":0.5291364092379808,"y":610.0},{"x":0.5952446624636649,"y":606.0},{"x":0.6613529156893491,"y":616.0},{"x":0.7274611689150332,"y":408.0},{"x":0.7935694221407174,"y":320.0},{"x":0.8596776753664015,"y":240.0},{"x":0.9257859285920856,"y":159.0},{"x":0.9918941818177698,"y":77.0},{"x":1.058002435043454,"y":6.0},{"x":1.1241106882691383,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"c32d26c9-5752-4761-a36a-094601bff497\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"c32d26c9-5752-4761-a36a-094601bff497\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"c32d26c9-5752-4761-a36a-094601bff497\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"c32d26c9-5752-4761-a36a-094601bff497\", :values ({:x 2.7038343250751495E-4, :y 0} {:x 0.06637863665819169, :y 1012.0} {:x 0.13248688988387586, :y 1059.0} {:x 0.19859514310956003, :y 1219.0} {:x 0.2647033963352442, :y 1140.0} {:x 0.33081164956092834, :y 765.0} {:x 0.3969199027866125, :y 954.0} {:x 0.46302815601229663, :y 809.0} {:x 0.5291364092379808, :y 610.0} {:x 0.5952446624636649, :y 606.0} {:x 0.6613529156893491, :y 616.0} {:x 0.7274611689150332, :y 408.0} {:x 0.7935694221407174, :y 320.0} {:x 0.8596776753664015, :y 240.0} {:x 0.9257859285920856, :y 159.0} {:x 0.9918941818177698, :y 77.0} {:x 1.058002435043454, :y 6.0} {:x 1.1241106882691383, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; As we would expect, the policy distribution is linear, reaching its mode at either q=0 or q=1:
;;; 
;;; @@q = \arg \max\limits_q \left[q\color{blue}{(p_1c_1+(1-p_1)(c_b+c_2))} + (1-q)\color{brown}{(p_2c_2+(1-p_2)(c_b+c_1))}\right]@@
;;; 
;;; If @@\color{blue} {(p_1c_1+(1-p_1)(c_b+c_2))}@@ is less than @@\color{brown} {(p_2c_2+(1-p_2)(c_b+c_1)}@@,
;;; q must be 1, that is, the first road should always be checked first. Otherwise, q must  be 0.
;; **
