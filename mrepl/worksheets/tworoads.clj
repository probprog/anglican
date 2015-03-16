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

(defquery tworoads "two roads example" gas-cost
  (let [gas-cost (or gas-cost 1.)
        o1 (sample (flip p1))
        o2 (sample (flip p2))
        q (sample (beta (/ 1. gas-cost) (/ 1. gas-cost)))]
    (observe (flip 1.0) (or o1 o2))

    (let [distance (if (sample (flip q))
                     (if o1 c1 (+ c2 cb))
                     (if o2 c2 (+ c1 cb)))
          log-weight (* distance gas-cost)]
      (observe (flip (exp (- log-weight))) true)
      (predict :q q))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tworoads/tworoads</span>","value":"#'tworoads/tworoads"}
;; <=

;; **
;;; Now we want to run (lazily) some inference. We'll use LMH (Lightweight Metropolis-Hastings) for that.
;; **

;; @@
(def states (doquery :almh tworoads 1.))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tworoads/states</span>","value":"#'tworoads/states"}
;; <=

;; **
;;; It is the time to visualize and retrieve the results. First, decide how many samples we want to look at.
;; **

;; @@
(def N 2000)
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
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"0d22a009-04bb-448a-bc79-cbdc8e109784","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"0d22a009-04bb-448a-bc79-cbdc8e109784","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"0d22a009-04bb-448a-bc79-cbdc8e109784"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"0d22a009-04bb-448a-bc79-cbdc8e109784","values":[{"x":5.953020881861448E-4,"y":0},{"x":0.08324227994307877,"y":307.0},{"x":0.1658892577979714,"y":274.0},{"x":0.24853623565286403,"y":255.0},{"x":0.33118321350775665,"y":222.0},{"x":0.41383019136264926,"y":129.0},{"x":0.49647716921754187,"y":132.0},{"x":0.5791241470724345,"y":211.0},{"x":0.6617711249273271,"y":128.0},{"x":0.7444181027822198,"y":123.0},{"x":0.8270650806371124,"y":91.0},{"x":0.909712058492005,"y":70.0},{"x":0.9923590363468976,"y":58.0},{"x":1.0750060142017903,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"0d22a009-04bb-448a-bc79-cbdc8e109784\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"0d22a009-04bb-448a-bc79-cbdc8e109784\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"0d22a009-04bb-448a-bc79-cbdc8e109784\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"0d22a009-04bb-448a-bc79-cbdc8e109784\", :values ({:x 5.953020881861448E-4, :y 0} {:x 0.08324227994307877, :y 307.0} {:x 0.1658892577979714, :y 274.0} {:x 0.24853623565286403, :y 255.0} {:x 0.33118321350775665, :y 222.0} {:x 0.41383019136264926, :y 129.0} {:x 0.49647716921754187, :y 132.0} {:x 0.5791241470724345, :y 211.0} {:x 0.6617711249273271, :y 128.0} {:x 0.7444181027822198, :y 123.0} {:x 0.8270650806371124, :y 91.0} {:x 0.909712058492005, :y 70.0} {:x 0.9923590363468976, :y 58.0} {:x 1.0750060142017903, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; Estimating the mode numerically can be problematic, but we can compute the 'mean' policy easily.
;; **

;; @@
(/ (reduce + policy) (count policy))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>0.36768894525943324</span>","value":"0.36768894525943324"}
;; <=

;; **
;;; As we would expect, the policy distribution is linear, reaching its mode at either q=0 or q=1:
;;; 
;;; @@q = \arg \max\limits_q \left[q\color{blue}{(p_1c_1+(1-p_1)(c_b+c_2))} + (1-q)\color{brown}{(p_2c_2+(1-p_2)(c_b+c_1))}\right]@@
;;; 
;;; If @@\color{blue} {(p_1c_1+(1-p_1)(c_b+c_2))}@@ is less than @@\color{brown} {(p_2c_2+(1-p_2)(c_b+c_1)}@@,
;;; q must be 1, that is, the first road should always be checked first. Otherwise, q must  be 0.k
;; **

;; **
;;; ## Simulated Annealing using non-uniform priors
;;; 
;;; We would like to modify the inference so that we can numerically, rather than visually, discover the optimal policy. The SAME algorithm could be an option if we had an easy way to implement it for probabilistic programming.
;;; 
;;; However, there is another way: instead of selecting the policy from the uniform prior, we can gradually 'tighten' the prior so that in the beginning the prior is uniform, but as we proceed, we give mixed policies an increasingly low chance. We do this in M steps.
;; **

;; @@
(def M 15)

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tworoads/M</span>","value":"#'tworoads/M"}
;; <=

;; **
;;; Growing gas-cost linearly is most probably too slow, we will grow it exponentially.
;; **

;; @@
(let  [c 1.5]
  (def gas-costs (map #(pow c %) (range M)))
  (map round gas-costs))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-long'>11</span>","value":"11"},{"type":"html","content":"<span class='clj-long'>17</span>","value":"17"},{"type":"html","content":"<span class='clj-long'>26</span>","value":"26"},{"type":"html","content":"<span class='clj-long'>38</span>","value":"38"},{"type":"html","content":"<span class='clj-long'>58</span>","value":"58"},{"type":"html","content":"<span class='clj-long'>86</span>","value":"86"},{"type":"html","content":"<span class='clj-long'>130</span>","value":"130"},{"type":"html","content":"<span class='clj-long'>195</span>","value":"195"},{"type":"html","content":"<span class='clj-long'>292</span>","value":"292"}],"value":"(1 2 2 3 5 8 11 17 26 38 58 86 130 195 292)"}
;; <=

;; **
;;; At every step, we run the inference with a higher value of gas cost. As the gas cost goes up, both the prior and the log weight cause the distribution to become more 'peaked'. For each step, we compute the 'mean' policy.
;; **

;; @@
(def mean-policy (for [gas-cost gas-costs]
              	   (let [states (doquery :almh tworoads gas-cost :number-of-particles 100)
                   		 predicts (map get-predicts (take N states))
                         policy (map :q predicts)]
                     (/ (reduce + policy) (count policy)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tworoads/mean-policy</span>","value":"#'tworoads/mean-policy"}
;; <=

;; **
;;; We can now plot the mean policies to see where they converge to:
;; **

;; @@
(plot/list-plot mean-policy :joined true)
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"41430543-e8e4-477a-8ae0-aeae2d88cfa0","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"41430543-e8e4-477a-8ae0-aeae2d88cfa0","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"41430543-e8e4-477a-8ae0-aeae2d88cfa0"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"41430543-e8e4-477a-8ae0-aeae2d88cfa0","values":[{"x":0,"y":0.3868710343770217},{"x":1,"y":0.3301962189839565},{"x":2,"y":0.2178491338997762},{"x":3,"y":0.19126668160654162},{"x":4,"y":0.1357756064466921},{"x":5,"y":0.1151070171535678},{"x":6,"y":0.26477733035366974},{"x":7,"y":0.06132792407620439},{"x":8,"y":0.03223000031142047},{"x":9,"y":0.20368688645303176},{"x":10,"y":0.015813414441288056},{"x":11,"y":0.20907572147465187},{"x":12,"y":1.5150892600777535E-61},{"x":13,"y":9.200738827367392E-50},{"x":14,"y":0.007152233998398705}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"41430543-e8e4-477a-8ae0-aeae2d88cfa0\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"41430543-e8e4-477a-8ae0-aeae2d88cfa0\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"41430543-e8e4-477a-8ae0-aeae2d88cfa0\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"41430543-e8e4-477a-8ae0-aeae2d88cfa0\", :values ({:x 0, :y 0.3868710343770217} {:x 1, :y 0.3301962189839565} {:x 2, :y 0.2178491338997762} {:x 3, :y 0.19126668160654162} {:x 4, :y 0.1357756064466921} {:x 5, :y 0.1151070171535678} {:x 6, :y 0.26477733035366974} {:x 7, :y 0.06132792407620439} {:x 8, :y 0.03223000031142047} {:x 9, :y 0.20368688645303176} {:x 10, :y 0.015813414441288056} {:x 11, :y 0.20907572147465187} {:x 12, :y 1.5150892600777535E-61} {:x 13, :y 9.200738827367392E-50} {:x 14, :y 0.007152233998398705})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; We observe two things: 
;;; * the 'mean' policy indeed converges to the best policy
;;; * however, as the gas cost goes up, the result becomes noiser.
;;; 
;;; The former is good, but the latter has to be fixed. We can fix it by increasing the number of iterations with the gas cost:
;; **

;; @@
(def max-cost (last gas-costs))
(def mean-policy (for [gas-cost gas-costs]
              	   (let [states (doquery :almh tworoads gas-cost :number-of-particles 100)
                   		 predicts (map get-predicts
                                       (take (int (* (+ 1. (* 9. (/ gas-cost max-cost))) N))
                                             states))
                         policy (map :q predicts)]
                     (/ (reduce + policy) (count policy)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tworoads/mean-policy</span>","value":"#'tworoads/mean-policy"}
;; <=

;; **
;;; We expect to see the same trend, but with less noise for higher values of gas cost.
;; **

;; @@
(plot/list-plot mean-policy :joined true)
;; @@

;; **
;;; Much better now. Of course, the 'cooling schedule', both the change in the gas cost, and the  increase of the number of iterations, can be smarter. 
;;; 
;;; Also, we do not have to start the inference from scratch for each increase of gas-cost. Instead, we can continue the same inference in a way similar to Simulated Annealing. This can be achieved by a small modification to the inference algorithm: we can make the number of iterations available to the program code, and will adjust the gas cost according to the number of iterations using a slowly increasing schedule.
;; **
