(ns anglican.algorithm-test
  (:require [clojure.test :refer :all]
            [clojure.core.matrix :as m]
            [anglican.stat :as stat])
  (:use [anglican
          core runtime emit
          [state :only [get-predicts get-log-weight]]
          [inference :only [collect-by]]]))

(defrecord benchmark
  [;; query to be run in benchmark
   query
   ;; arguments for the query
   args
   ;; a function (fn [samples]) -> result
   result
   ;; ground truth value for result
   truth
   ;; a function (fn [result truth]) -> distance
   metric
   ;; benchmark passes if (< distance threshold)
   threshold])

(defn- square [x]
  "returns product (mul x x)"
  (m/mul x x))

(defn- kl-categorical
  "calculates the kl divergence from two sets of categorical parameters"
  [p-categories q-categories]
  (let [p-norm (reduce + (map second p-categories))
        q-norm (reduce + (map second q-categories))
        q (into {} (for [[c w] q-categories]
                     [c (/ w q-norm)]))]
    (reduce +
            (for [[c w] p-categories]
              (if (> w 0.0)
                (* (/ w p-norm)
                   (log (/ (double (/ w p-norm))
                           (double (get q c 0.0)))))
                0.0)))))

(defn- kl-normal
  "calculates the kl divergenece from two sets of normal parameters"
  [[p-mean p-sigma] [q-mean q-sigma]]
  (+ (- (log q-sigma)
        (log p-sigma))
     (/ (+ (square p-sigma)
           (square (- p-mean
                      q-mean)))
        (* 2 (square q-sigma)))
     (/ -1 2)))

;; benchmark definitions

(def gaussian
  (->benchmark
   ;; query
   (query
    [observations sigma mu0 sigma0]
    (let [mu (sample (normal mu0 sigma0))
          likelihood (normal mu sigma)]
      (reduce (fn [_ obs]
                (observe likelihood obs))
              nil
              observations)
      (predict :mu mu)))
   ;; args
   [[9.0 8.0] (sqrt 2.0) 1.0 (sqrt 5.0)]
   ;; result
   #((juxt stat/empirical-mean
           stat/empirical-std)
     (collect-by :mu %))
   ;; truth
   [7.25 (sqrt (/ 1.0 1.2))]
   ;; metric
   kl-normal
   ;; threshold
   0.10))

(defn- index->ind
   "converts a collection of indices to a matrix of indicator vectors"
   [values vmin vmax]
   (let [zero-vec (into [] (repeat (- (inc vmax) vmin) 0))]
     (m/matrix (map #(assoc zero-vec (- % vmin) 1) values))))

(def hmm
  (->benchmark
   ;; query
   (query
    [observations init-dist trans-dists obs-dists]
    (predict
     :states
     (reduce
      (fn [states obs]
        (let [state (sample (get trans-dists
                                 (peek states)))]
          (observe (get obs-dists state) obs)
          (conj states state)))
      [(sample init-dist)]
      observations)))
   ;; args
   [[0.9 0.8 0.7 0.0 -0.025 -5.0 -2.0 -0.1 0.0 0.13 0.45 6 0.2 0.3 -1 -1]
    (discrete [1.0 1.0 1.0])
    {0 (discrete [0.1 0.5 0.4])
     1 (discrete [0.2 0.2 0.6])
     2 (discrete [0.15 0.15 0.7])}
    {0 (normal -1 1)
     1 (normal 1 1)
     2 (normal 0 1)}]
   ;; result
   (fn [samples]
     (stat/empirical-mean
      (collect-by (comp #(index->ind % 0 2) :states)
                  samples)))
   ;; truth
   [[ 0.3775 0.3092 0.3133]
    [ 0.0416 0.4045 0.5539]
    [ 0.0541 0.2552 0.6907]
    [ 0.0455 0.2301 0.7244]
    [ 0.1062 0.1217 0.7721]
    [ 0.0714 0.1732 0.7554]
    [ 0.9300 0.0001 0.0699]
    [ 0.4577 0.0452 0.4971]
    [ 0.0926 0.2169 0.6905]
    [ 0.1014 0.1359 0.7626]
    [ 0.0985 0.1575 0.744 ]
    [ 0.1781 0.2198 0.6022]
    [ 0.0000 0.9848 0.0152]
    [ 0.1130 0.1674 0.7195]
    [ 0.0557 0.1848 0.7595]
    [ 0.2017 0.0472 0.7511]
    [ 0.2545 0.0611 0.6844]]
   ;; metric
   (comp sqrt #(/ % (* 3 16)) stat/l2-norm)
   ;; threshold
   0.1))

(defn- fib [n]
  "returns the n-th number in the Fibonacci sequence"
  (loop [a 0 b 1 m 0]
    (if (= m n)
      a
      (recur b (+ a b) (inc m)))))

(def -inf (/ -1.0 0.0))

(def branching
  (->benchmark
   ;; query
   (with-primitive-procedures [fib]
     (query
      (let [count-prior (poisson 4)
            r (sample count-prior)
            l (if (< 4 r)
                6
                (+ (fib (* 3 r))
                   (sample count-prior)))]
        (observe (poisson l) 6)
        (predict :r r)
        (predict :l l))))
   ;; args
   nil
   ;; result
   #(stat/empirical-distribution (collect-by :r %))
   ;; truth
   (into (sorted-map)
      (stat/empirical-distribution
       (let [count-prior (poisson 4)]
         (for [r (range 40)
               s (range 40)]
           (let [l (if (< 4 r)
                     6
                     (+ (fib (* 3 r))
                        (sample count-prior)))]
             [r (+ (observe (poisson l) 6)
                   (observe count-prior r)
                   (observe count-prior s))])))))
   ;; metric
   kl-categorical
   ;; threshold
   0.05))

(def crp-gmm
  (->benchmark
   ;; query
   (query
    [observations alpha mu beta a b]
    (let [precision-prior (gamma a b)]
      (loop [observations observations
             state-proc (CRP alpha)
             obs-dists {}
             states []]
        (if (empty? observations)
          (do
            (predict :states states)
            (predict :num-clusters (count obs-dists)))
          (let [state (sample (produce state-proc))
                obs-dist (get obs-dists
                              state
                              (let [l (sample precision-prior)
                                    s (sqrt (/ (* beta l)))
                                    m (sample (normal mu s))]
                                (normal m (sqrt (/ l)))))]
            (observe obs-dist (first observations))
            (recur (rest observations)
                   (absorb state-proc state)
                   (assoc obs-dists state obs-dist)
                   (conj states state)))))))
   ;; args
   [[10 11 12 -100 -150 -200 0.001 0.01 0.005 0.0]
    1.72 0.0 100.0 1.0 10.0]
   ;; result
   #(stat/empirical-distribution (collect-by :num-clusters %))
   ;; truth
   (zipmap
    (range 1 11)
    (mapv exp
          [-11.4681 -1.0437 -0.9126 -1.6553 -3.0348
           -4.9985 -7.5829 -10.9459 -15.6461 -21.6521]))
   ;; metric
   (fn [r t]
     (kl-categorical r t))
   ;; threshold
   1.0))

(defn auto-burn
  "discards half of samples when all samples have equal weight"
  [samples]
  (if (> (count (distinct (map get-log-weight samples))) 2)
    samples
    (drop (/ (count samples) 2) samples)))

(defn dobenchmark
  [benchmark number-of-samples algorithm & opts]
  (->> (apply doquery algorithm (:query benchmark) (:args benchmark) opts)
       (take number-of-samples)
       auto-burn
       ((:result benchmark))
       (#((:metric benchmark) % (:truth benchmark)))))

(def benchmarks
  "benchmarks to be included in tests"
  {:branching branching
   :crp-gmm crp-gmm
   :gaussian gaussian
   :hmm hmm})

(def algorithm-opts
  "options for each algorithm to be included tests"
  {:importance nil
   :lmh nil
   :almh nil
   :rmh [:alpha 0.5 :sigma 1]
   :ais [:number-of-steps 10]
   :smc [:number-of-particles 100]
   :pimh [:number-of-particles 100]
   :pgibbs [:number-of-particles 100]
   :pgas [:number-of-particles 10]
   :pcascade [:number-of-particles 100 :number-of-threads 200]})

(def scale-num-samples
  "options that allow increase/reduction of number of samples"
  {:ais 0.2})

(deftest test-benchmarks
  (doseq [[id benchmark] benchmarks
          [algorithm opts] algorithm-opts]
    (testing [id algorithm]
      (let [error (apply dobenchmark 
                         benchmark 
                         (* 10000 (get scale-num-samples 
                                       algorithm 1.0))
                         algorithm 
                         opts)]
        (prn id algorithm error)
        (is (< error (:threshold benchmark)))))))
