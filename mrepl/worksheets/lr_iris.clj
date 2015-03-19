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

;; @@
(def samples (doquery :palmh lr-iris nil))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;lr-iris/samples</span>","value":"#'lr-iris/samples"}
;; <=

;; @@
(def scale 10)
(def bs (mapv (comp vec :b) (map get-predicts (take-nth scale  (take (* scale 1000) samples)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;lr-iris/bs</span>","value":"#'lr-iris/bs"}
;; <=

;; @@
(def scores
  (map (fn [b]
       (reduce (fn [score record]
                 (if (= (= (last record) :setosa) (> (z b (features record)) 0.95))
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
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"5e067db0-f3e7-4082-8235-4c044d88f127","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"5e067db0-f3e7-4082-8235-4c044d88f127","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"5e067db0-f3e7-4082-8235-4c044d88f127"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"5e067db0-f3e7-4082-8235-4c044d88f127","values":[{"x":100.0,"y":0},{"x":104.45454545454545,"y":21.0},{"x":108.9090909090909,"y":0.0},{"x":113.36363636363636,"y":4.0},{"x":117.81818181818181,"y":3.0},{"x":122.27272727272727,"y":2.0},{"x":126.72727272727272,"y":2.0},{"x":131.1818181818182,"y":10.0},{"x":135.63636363636365,"y":17.0},{"x":140.09090909090912,"y":12.0},{"x":144.5454545454546,"y":38.0},{"x":149.00000000000006,"y":891.0},{"x":153.45454545454552,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"5e067db0-f3e7-4082-8235-4c044d88f127\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"5e067db0-f3e7-4082-8235-4c044d88f127\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"5e067db0-f3e7-4082-8235-4c044d88f127\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"5e067db0-f3e7-4082-8235-4c044d88f127\", :values ({:x 100.0, :y 0} {:x 104.45454545454545, :y 21.0} {:x 108.9090909090909, :y 0.0} {:x 113.36363636363636, :y 4.0} {:x 117.81818181818181, :y 3.0} {:x 122.27272727272727, :y 2.0} {:x 126.72727272727272, :y 2.0} {:x 131.1818181818182, :y 10.0} {:x 135.63636363636365, :y 17.0} {:x 140.09090909090912, :y 12.0} {:x 144.5454545454546, :y 38.0} {:x 149.00000000000006, :y 891.0} {:x 153.45454545454552, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; @@
(clojure.pprint/pprint (macroexpand '(query 
   "logistic regression on iris"
   (let [sigma (/ 1. (sqrt (sample (gamma 1. 1.))))
         b (repeatedly 5  (fn [] (sample (normal 0. sigma))))]
   
      (reduce (fn [_ record]
                (observe (flip (z b (features record)))
                        (= (last record) :setosa)))
              () iris-data)
   
     (predict :b b)))))
;; @@
;; ->
;;; (let*
;;;  [map
;;;   $map
;;;   reduce
;;;   $reduce
;;;   filter
;;;   $filter
;;;   some
;;;   $some
;;;   repeatedly
;;;   $repeatedly
;;;   comp
;;;   $comp
;;;   partial
;;;   $partial]
;;;  (fn
;;;   query17368
;;;   [$value $state]
;;;   (fn
;;;    []
;;;    ((fn
;;;      do17369
;;;      [_ $state]
;;;      (embang.trap/-&gt;sample
;;;       &#x27;S17389
;;;       (gamma 1.0 1.0)
;;;       (fn
;;;        arg17388
;;;        [A17387 $state]
;;;        (fn
;;;         []
;;;         ((fn
;;;           arg17386
;;;           [A17385 $state]
;;;           (fn
;;;            []
;;;            ((fn
;;;              var17384
;;;              [sigma $state]
;;;              (repeatedly
;;;               (fn
;;;                var17380
;;;                [b $state]
;;;                (reduce
;;;                 (fn
;;;                  do17370
;;;                  [_ $state]
;;;                  (fn
;;;                   []
;;;                   (#&lt;trap$result_cont embang.trap$result_cont@5ce5b777&gt;
;;;                    nil
;;;                    (embang.state/add-predict $state :b b))))
;;;                 $state
;;;                 (fn
;;;                  fn17372
;;;                  [C17371 $state _ record]
;;;                  (features
;;;                   (fn
;;;                    arg17379
;;;                    [A17378 $state]
;;;                    (z
;;;                     (fn
;;;                      arg17377
;;;                      [A17376 $state]
;;;                      (fn
;;;                       []
;;;                       ((fn
;;;                         arg17374
;;;                         [A17373 $state]
;;;                         (embang.trap/-&gt;observe
;;;                          &#x27;O17375
;;;                          A17373
;;;                          (= (last record) :setosa)
;;;                          C17371
;;;                          $state))
;;;                        (flip A17376)
;;;                        $state)))
;;;                     $state
;;;                     b
;;;                     A17378))
;;;                   $state
;;;                   record))
;;;                 ()
;;;                 iris-data))
;;;               $state
;;;               5
;;;               (fn
;;;                fn17382
;;;                [C17381 $state]
;;;                (embang.trap/-&gt;sample
;;;                 &#x27;S17383
;;;                 (normal 0.0 sigma)
;;;                 C17381
;;;                 $state))))
;;;             (/ 1.0 A17385)
;;;             $state)))
;;;          (sqrt A17387)
;;;          $state)))
;;;       $state))
;;;     &quot;logistic regression on iris&quot;
;;;     $state))))
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;; @@
