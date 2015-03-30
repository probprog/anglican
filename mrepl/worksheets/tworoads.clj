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
;;; ## Pseudo-distributions
;;; 
;;; Policy learning requires a pseudo distribution for manipulating the log weight.
;; **

;; @@
(defdist factor
  "factor distribution,
  sample returns true,
  the log-weight is the argument"
  [log-weight] []
  (sample [this] true)
  (observe [this value] log-weight))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#&lt;MultiFn clojure.lang.MultiFn@235cd5cc&gt;</span>","value":"#<MultiFn clojure.lang.MultiFn@235cd5cc>"}
;; <=

;; **
;;; Also, we want to compute probabilities of values given distributions. 
;; **

;; @@
(defm observe* [dist value] (apply observe dist value nil))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tworoads/observe*</span>","value":"#'tworoads/observe*"}
;; <=

;; **
;;; # A Cooling Schedule for the Two Road Example
;;; 
;;; Let us see if we can make the gas cost go up automatically without changing the algorithm.
;;; The example is the same as in 'tworoads.clj'. We will explore the idea of defining a slowly increasing gas cost source.
;;; 
;; **

;; **
;;; # Two Roads
;;; 
;;; An inference example. We are given two roads with probability being open p1 and p2, costs of each road c1 and c2, and the cost of bumping into a blocked road cb. We want to learn the optimum policy --- with which probability to select each of the roads to pay less on average.
;;; 
;;; The Anglican code below defines the problem. We predict the policy (q) but not the distance, the distance prediction does not reflect the actual average distance. As Brooks says, "I have either a pound or nothing, guess right".
;; **

;; @@
(def c1 1)
(def c2 2)
(def cb 1)
(def p1 0.25)
(def p2 0.75)

(with-primitive-procedures [factor get-log-weight]
(defquery tworoads "two roads example" gas-cost
  (let [gas-cost (or gas-cost 1.)]
    (loop []
        (let [o1 (sample (flip p1))
              o1-log-p (observe* (flip p1) o1)
              o2 (sample (flip p2))
              o2-log-p (observe* (flip p2) o2)]
          (if (not (or o1 o2)) (recur)
            (let [q (sample (uniform-continuous 0. 1.))
                  s (< q 0.5)]
              (let [distance (if s
                               (if o1 c1 (+ c2 cb))
                               (if o2 c2 (+ c1 cb)))
                    log-p-dist (+ o1-log-p o2-log-p)
                    log-weight (- log-p-dist (* distance gas-cost (exp log-p-dist)))]
                (observe (factor log-weight) true)
                (predict :joint [o1 o2 q distance])
                (predict :q q)))))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tworoads/tworoads</span>","value":"#'tworoads/tworoads"}
;; <=

;; **
;;; Now we want to run (lazily) some inference. We'll use LMH (Lightweight Metropolis-Hastings) for that.
;; **

;; @@
(def states (doquery :lmh tworoads 2))
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
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"afacb438-a117-4d5d-98b5-f349916150d6","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"afacb438-a117-4d5d-98b5-f349916150d6","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"afacb438-a117-4d5d-98b5-f349916150d6"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"afacb438-a117-4d5d-98b5-f349916150d6","values":[{"x":0.0012999784667044878,"y":0},{"x":0.06786157774428528,"y":512.0},{"x":0.13442317702186607,"y":593.0},{"x":0.20098477629944686,"y":491.0},{"x":0.26754637557702765,"y":648.0},{"x":0.3341079748546084,"y":519.0},{"x":0.4006695741321892,"y":551.0},{"x":0.46723117340976994,"y":543.0},{"x":0.5337927726873507,"y":751.0},{"x":0.6003543719649315,"y":756.0},{"x":0.6669159712425122,"y":728.0},{"x":0.733477570520093,"y":772.0},{"x":0.8000391697976738,"y":751.0},{"x":0.8666007690752545,"y":800.0},{"x":0.9331623683528353,"y":800.0},{"x":0.999723967630416,"y":776.0},{"x":1.066285566907997,"y":9.0},{"x":1.1328471661855777,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"afacb438-a117-4d5d-98b5-f349916150d6\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"afacb438-a117-4d5d-98b5-f349916150d6\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"afacb438-a117-4d5d-98b5-f349916150d6\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"afacb438-a117-4d5d-98b5-f349916150d6\", :values ({:x 0.0012999784667044878, :y 0} {:x 0.06786157774428528, :y 512.0} {:x 0.13442317702186607, :y 593.0} {:x 0.20098477629944686, :y 491.0} {:x 0.26754637557702765, :y 648.0} {:x 0.3341079748546084, :y 519.0} {:x 0.4006695741321892, :y 551.0} {:x 0.46723117340976994, :y 543.0} {:x 0.5337927726873507, :y 751.0} {:x 0.6003543719649315, :y 756.0} {:x 0.6669159712425122, :y 728.0} {:x 0.733477570520093, :y 772.0} {:x 0.8000391697976738, :y 751.0} {:x 0.8666007690752545, :y 800.0} {:x 0.9331623683528353, :y 800.0} {:x 0.999723967630416, :y 776.0} {:x 1.066285566907997, :y 9.0} {:x 1.1328471661855777, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; If @@\color{blue} {(p_1c_1+(1-p_1)(c_b+c_2))}@@ is less than @@\color{brown} {(p_2c_2+(1-p_2)(c_b+c_1)}@@,
;;; q must be 0, that is, the first road should always be checked first. Otherwise, q must  be 1.
;; **

;; @@
(let  [expected-cost-1 (+ (* p1 c1) (* (- 1 p1) (+ cb c2)))
       expected-cost-2 (+ (* p2 c2) (* (- 1 p2) (+ cb c1)))]
  (println "EC1 =" expected-cost-1 "EC2 =" expected-cost-2 "\nselect"
    (cond (< expected-cost-1 expected-cost-2) "first road (q=0)"
          (> expected-cost-1 expected-cost-2) "second road (q=1)"
          :else "any road")))
;; @@
;; ->
;;; EC1 = 2.5 EC2 = 2.0 
;;; select second road (q=1)
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=
