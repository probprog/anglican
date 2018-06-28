(ns anglican.test-runner
  (:require  [cljs.test :as t :include-macros true]
             [anglican.trap-test :as trap-test]
             [anglican.stat-test :as stat-test]
             [anglican.runtime-test :as runtime-test]))


(enable-console-print!)

(defmethod t/report [::t/default :end-run-tests] [m]
  (when (exists? js/phantom)
    (js/phantom.exit (+ (:fail m) (:error m)))))

(t/run-tests 'anglican.trap-test
             'anglican.stat-test
             'anglican.runtime-test)


