(ns anglican.ipmcmc-test
  (:require  [clojure.core.matrix :as mat]
             [clojure.test :refer [deftest testing is]])
  (:use anglican.ipmcmc))

(defn gibbs-sweep-average-estimate
  "Calls gibbs-update-csmc-indices a number of times and calculates
   the average number of times a point is sampled and average estimate
   for sum-zetas.

   Accepts: log-Zs  Marginal likelihood estimates of nodes
            P       Number of conditional nodes
            n-samples  Number of samples to average over

   Returns: p-sample  Average number of times each node was sampled
            mean-sum-zetas  Average estimate for sum zetas"
  [log-Z P n-samples]
  (loop [n-times-sampled (into [] (repeat (count log-Z) 0))
         total-sum-zetas (into [] (repeat (count log-Z) 0))
         i 0]
    (if (= i n-samples)
      [(mapv double (mat/div n-times-sampled (reduce + n-times-sampled)))
       (mat/div total-sum-zetas (* P n-samples))]
      (let [[cs sum-zetas] (gibbs-update-csmc-indices log-Z P)]
        (recur (reduce (fn [n i]
                            (assoc n i (inc (nth n i))))
                       n-times-sampled
                       cs)
               (mapv + total-sum-zetas sum-zetas)
               (inc i))))))

;; Used params and associated ground truth
(def log-Zs [-3 -4 -1 -8 -10 -5 -4 -3.4])
(def P 4)
(def true-p-sample [0.22461	0.11994	0.24693	0.00132	0.00044	0.06128	0.14521	0.20026])
(def true-mean-sum-zetas true-p-sample)

(deftest ipmcmc-gibbs-update
  (testing "ipmcmc-conditional-indices-gibbs-update"
    (let [n-samples 10000
          test-tolerance 0.005
          [p-sample mean-sum-zetas] (gibbs-sweep-average-estimate log-Zs P n-samples)]
      (is (every? #(< (Math/abs %) test-tolerance)
                  (concat (mat/sub p-sample true-p-sample)
                          (mat/sub mean-sum-zetas true-mean-sum-zetas)))
          "testing ipmcmc-conditional-indices-gibbs-update"))))