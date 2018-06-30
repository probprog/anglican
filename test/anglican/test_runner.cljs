(ns anglican.test-runner
  (:require  [cljs.test :as t :include-macros true]
             [anglican.dist-test :as dist-test]
             [anglican.emit-test :as emit-test]
             [anglican.runtime-test :as runtime-test]
             [anglican.stat-test :as stat-test]
             [anglican.trap-test :as trap-test]
             [anglican.algorithm-test :as algorithm-test]))


(enable-console-print!)

(defmethod t/report [::t/default :end-run-tests] [m]
  (when (exists? js/phantom)
    (js/phantom.exit (+ (:fail m) (:error m)))))

(t/run-tests 'anglican.trap-test
             'anglican.stat-test
             'anglican.runtime-test
             'anglican.emit-test
             'anglican.dist-test
             'anglican.algorithm-test)


