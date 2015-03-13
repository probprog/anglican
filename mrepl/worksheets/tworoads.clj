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
;;; An inference example. We are given two roads with probability being open p1 and p2, costs of each road c1 and c2, and the cost of bumping into a blocked road cc. We want to learn the optimum policy --- with which probability to select each of the roads to pay less on average.
;;; 
;;; The Anglican code below defines the problem. We predict the policy (q) but not the distance, the distance prediction does not reflect the actual average distance. As Brooks says, "I have either a pound or nothing, guess right".
;; **

;; @@
(def cc 2)
(def c1 3)
(def c2 1)
(def p1 1/3)
(def p2 2/3)
(def gas-cost 10)

(defquery tworoads "two roads example"
  (let [o1 (sample (flip p1))
        o2 (sample (flip p2))
        q (sample (uniform-continuous 0. 1.))
        s (sample (flip q))]
    (observe (flip 1.0) (or o1 o2))

    (let [distance (if (sample (flip q))
                     (if o1 c1 (+ c2 cc))
                     (if o2 c2 (+ c1 cc)))]
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
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"6afd9beb-2515-4abe-8c20-11ee40197aee","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"6afd9beb-2515-4abe-8c20-11ee40197aee","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"6afd9beb-2515-4abe-8c20-11ee40197aee"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"6afd9beb-2515-4abe-8c20-11ee40197aee","values":[{"x":3.138675820082426E-4,"y":0},{"x":0.06686154049821198,"y":1082.0},{"x":0.13340921341441572,"y":841.0},{"x":0.19995688633061948,"y":965.0},{"x":0.2665045592468232,"y":834.0},{"x":0.33305223216302693,"y":981.0},{"x":0.39959990507923066,"y":763.0},{"x":0.4661475779954344,"y":826.0},{"x":0.5326952509116382,"y":694.0},{"x":0.599242923827842,"y":693.0},{"x":0.6657905967440457,"y":542.0},{"x":0.7323382696602495,"y":407.0},{"x":0.7988859425764533,"y":470.0},{"x":0.8654336154926571,"y":445.0},{"x":0.9319812884088609,"y":259.0},{"x":0.9985289613250646,"y":198.0},{"x":1.0650766342412683,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"6afd9beb-2515-4abe-8c20-11ee40197aee\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"6afd9beb-2515-4abe-8c20-11ee40197aee\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"6afd9beb-2515-4abe-8c20-11ee40197aee\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"6afd9beb-2515-4abe-8c20-11ee40197aee\", :values ({:x 3.138675820082426E-4, :y 0} {:x 0.06686154049821198, :y 1082.0} {:x 0.13340921341441572, :y 841.0} {:x 0.19995688633061948, :y 965.0} {:x 0.2665045592468232, :y 834.0} {:x 0.33305223216302693, :y 981.0} {:x 0.39959990507923066, :y 763.0} {:x 0.4661475779954344, :y 826.0} {:x 0.5326952509116382, :y 694.0} {:x 0.599242923827842, :y 693.0} {:x 0.6657905967440457, :y 542.0} {:x 0.7323382696602495, :y 407.0} {:x 0.7988859425764533, :y 470.0} {:x 0.8654336154926571, :y 445.0} {:x 0.9319812884088609, :y 259.0} {:x 0.9985289613250646, :y 198.0} {:x 1.0650766342412683, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; As we would expect, the policy distribution is linear, reaching its mode at either q=0 or q=1.
;; **
