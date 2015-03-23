;; gorilla-repl.fileformat = 1

;; **
;;; # Logistic regression on Iris dataset
;;; 
;; **

;; @@
(ns lr-iris
  (:require [clojure.core.matrix :refer [dot]])
  (:require [gorilla-plot.core :as plot])
  (:use [mrepl core]
        [embang runtime emit]
        [anglib iris-data]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; We'll use iris-data defined in iris-data module to infer the weights of logistic regression function whether a specimen is of kind setosa.
;; **

;; @@
(defn features [record] (cons 1 (butlast record)))

(defn z [b x] (/ 1. (+ 1. (exp (* -1. (dot b x))))))  

(with-primitive-procedures [features z]
 (defquery lr-iris
   "logistic regression on iris"
   (let [sigma (/ 1. (sqrt (sample (gamma 1. 1.))))
         b (repeatedly 5  (fn [] (sample (normal 0. sigma))))]
   
      (reduce (fn [_ record]
                (observe (flip (z b (features record)))
                        (= (last record) :setosa)))
              () iris-data)
   
     (predict :b b))))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;lr-iris/lr-iris</span>","value":"#'lr-iris/lr-iris"}
;; <=

;; **
;;; Lazily define the inference. The inference isn't performed until the samples are retrieved.
;; **

;; @@
(def samples (doquery :palmh lr-iris nil))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;lr-iris/samples</span>","value":"#'lr-iris/samples"}
;; <=

;; **
;;; Now retrieve `N` samples, taking every `T`'th sample from the sample sequence. 
;; **

;; @@
(def N 1000)
(def T 10)
(def bs (mapv (comp vec :b) (map get-predicts (take-nth T (take (* T N) samples)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;lr-iris/bs</span>","value":"#'lr-iris/bs"}
;; <=

;; **
;;; We want to see how well the inference converges. Let us output the total number of correct answers given by the logistic regression classifier with confidence `conf`.
;; **

;; @@
(def conf 0.95)
(def scores
  (map (fn [b]
       (reduce (fn [score record]
                 (if (= (= (last record) :setosa) (> (z b (features record)) conf))
                   (inc score)
                   score))
               0 iris-data))
     bs))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;lr-iris/scores</span>","value":"#'lr-iris/scores"}
;; <=

;; @@
(plot/histogram scores :joined true)
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"ab9f93f3-089f-4328-a978-2dfd6ee0cb8d","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"ab9f93f3-089f-4328-a978-2dfd6ee0cb8d","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"ab9f93f3-089f-4328-a978-2dfd6ee0cb8d"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"ab9f93f3-089f-4328-a978-2dfd6ee0cb8d","values":[{"x":100.0,"y":0},{"x":104.45454545454545,"y":29.0},{"x":108.9090909090909,"y":0.0},{"x":113.36363636363636,"y":1.0},{"x":117.81818181818181,"y":6.0},{"x":122.27272727272727,"y":10.0},{"x":126.72727272727272,"y":6.0},{"x":131.1818181818182,"y":10.0},{"x":135.63636363636365,"y":15.0},{"x":140.09090909090912,"y":20.0},{"x":144.5454545454546,"y":41.0},{"x":149.00000000000006,"y":862.0},{"x":153.45454545454552,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"ab9f93f3-089f-4328-a978-2dfd6ee0cb8d\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"ab9f93f3-089f-4328-a978-2dfd6ee0cb8d\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"ab9f93f3-089f-4328-a978-2dfd6ee0cb8d\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"ab9f93f3-089f-4328-a978-2dfd6ee0cb8d\", :values ({:x 100.0, :y 0} {:x 104.45454545454545, :y 29.0} {:x 108.9090909090909, :y 0.0} {:x 113.36363636363636, :y 1.0} {:x 117.81818181818181, :y 6.0} {:x 122.27272727272727, :y 10.0} {:x 126.72727272727272, :y 6.0} {:x 131.1818181818182, :y 10.0} {:x 135.63636363636365, :y 15.0} {:x 140.09090909090912, :y 20.0} {:x 144.5454545454546, :y 41.0} {:x 149.00000000000006, :y 862.0} {:x 153.45454545454552, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=
