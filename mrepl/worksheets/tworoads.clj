;; gorilla-repl.fileformat = 1

;; **
;;; Boiler-plate code -- importing necessary things from Anglican.
;;; Should be wrapped nicely in the MREPL namespace in the future.
;; **

;; @@
(ns tworoads
  (:require [gorilla-plot.core :as plot]
   			[embang lmh
                    [state :refer [get-predicts]]
                    [inference :refer [infer warmup]]])
  (:use [embang emit runtime]))
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
;;; Now we want to run (lazily some inference). We'll use LMH (Lightweight Metropolis-Hastings) for that. Since in LMH all samples have the same weight, we'll ignore the weights and only keep the predicts. Before running the program, we 'warm it up' --- force all deterministic computations in the beginning.
;; **

;; @@
(def tworoads (warmup tworoads nil))

(def predicts (map get-predicts (infer :lmh tworoads nil)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tworoads/predicts</span>","value":"#'tworoads/predicts"}
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
;;; Then, take these samples, extract the distance and the policy, and plot them.
;; **

;; @@
(def predicts (map (partial into {}) (take N predicts)))
(def policy (map :q predicts))
(plot/histogram policy)

;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"38eb9f04-e6ce-4de9-911b-8bb61e9a9194","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"38eb9f04-e6ce-4de9-911b-8bb61e9a9194","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"38eb9f04-e6ce-4de9-911b-8bb61e9a9194"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"38eb9f04-e6ce-4de9-911b-8bb61e9a9194","values":[{"x":4.5181834138929844E-4,"y":0},{"x":0.0667214694277694,"y":1329.0},{"x":0.1329911205141495,"y":1060.0},{"x":0.19926077160052957,"y":1081.0},{"x":0.2655304226869097,"y":973.0},{"x":0.3318000737732898,"y":1057.0},{"x":0.3980697248596699,"y":596.0},{"x":0.46433937594605,"y":663.0},{"x":0.5306090270324301,"y":718.0},{"x":0.5968786781188101,"y":748.0},{"x":0.6631483292051902,"y":637.0},{"x":0.7294179802915702,"y":334.0},{"x":0.7956876313779503,"y":357.0},{"x":0.8619572824643303,"y":242.0},{"x":0.9282269335507104,"y":146.0},{"x":0.9944965846370905,"y":55.0},{"x":1.0607662357234706,"y":4.0},{"x":1.1270358868098507,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"38eb9f04-e6ce-4de9-911b-8bb61e9a9194\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"38eb9f04-e6ce-4de9-911b-8bb61e9a9194\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"38eb9f04-e6ce-4de9-911b-8bb61e9a9194\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"38eb9f04-e6ce-4de9-911b-8bb61e9a9194\", :values ({:x 4.5181834138929844E-4, :y 0} {:x 0.0667214694277694, :y 1329.0} {:x 0.1329911205141495, :y 1060.0} {:x 0.19926077160052957, :y 1081.0} {:x 0.2655304226869097, :y 973.0} {:x 0.3318000737732898, :y 1057.0} {:x 0.3980697248596699, :y 596.0} {:x 0.46433937594605, :y 663.0} {:x 0.5306090270324301, :y 718.0} {:x 0.5968786781188101, :y 748.0} {:x 0.6631483292051902, :y 637.0} {:x 0.7294179802915702, :y 334.0} {:x 0.7956876313779503, :y 357.0} {:x 0.8619572824643303, :y 242.0} {:x 0.9282269335507104, :y 146.0} {:x 0.9944965846370905, :y 55.0} {:x 1.0607662357234706, :y 4.0} {:x 1.1270358868098507, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; As we would expect, the policy distribution is linear, reaching its mode at either q=0 or q=1:
;;; 
;;; @@q = \arg \max\limits_q \left[q(p_1c_1+(1-p_1)(c_b+c_2))+(1-q)(p_2c_2+(1-p_2)(c_b+c_1))\right]@@
;;; 
;;; If @@(p_1c_1+(1-p_1)(c_b+c_2)) < (p_2c_2+(1-p_2)(c_b+c_1)@@
;;; 
;;; q must be 1, that is, the first road should always be checked first. Otherwise, q must  be 0.
;; **
