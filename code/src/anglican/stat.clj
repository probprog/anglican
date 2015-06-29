(ns anglican.stat
  "Statistics related functions"
  (:require [clojure.core.matrix :as m])
  (:use [anglican
           runtime
           [state :only [get-predicts get-log-weight]]]))

(defn- square [x]
  "returns square of argument x, which may be an array"
  (m/mul x x))

(defprotocol KL
  "calculation of KL divergences"
  (kl-divergence [p q]
    "returns the KL divergence between distributions p and q"))

(extend-protocol KL
  anglican.runtime.bernoulli-distribution
  (kl-divergence
   [p q]
   (+ (if (> (:p p) 0.0)
        (* (:p p)
           (log (/ (:p p) (:p q))))
        0.0)
      (if (< (:p p) 1.0)
        (* (- 1.0 (:p p))
           (log (/ (- 1.0 (:p p))
                   (- 1.0 (:p q)))))
        0.0)))
  anglican.runtime.categorical-distribution
  (kl-divergence
   [p q]
   (let [p-norm (get-in p [:dist :total-weight])
         q-norm (get-in q [:dist :total-weight])
         q-map (into {} (:categories q))]
     (reduce +
             (map (fn [[c p-weight]]
                    (if (> p-weight 0.0)
                      (* (/ p-weight p-norm)
                         (log (/ (double (/ p-weight p-norm))
                                 (double (/ (get q-map c 0.0)
                                            q-norm)))))
                      0.0))
                  (:categories p)))))
  anglican.runtime.discrete-distribution
  (kl-divergence
   [p q]
   (reduce +
           (map (fn [p-prob q-prob]
                  (if (> p-prob 0.0)
                    (* p-prob (log (/ p-prob q-prob)))
                    0.0))
                (map #(/ % (:total-weight p)) (:weights p))
                (map #(/ % (:total-weight q)) (:weights q)))))
  anglican.runtime.flip-distribution
  (kl-divergence
   [p q]
   (+ (if (> (:p p) 0.0)
        (* (:p p)
           (log (/ (:p p) (:p q))))
        0.0)
      (if (< (:p p) 1.0)
        (* (- 1.0 (:p p))
           (log (/ (- 1.0 (:p p))
                   (- 1.0 (:p q)))))
        0.0)))
  anglican.runtime.normal-distribution
  (kl-divergence
   [p q]
   (+ (- (log (:sd q))
         (log (:sd p)))
      (/ (+ (square (:sd p))
            (square (- (:mean p)
                       (:mean q))))
         (* 2 (square (:sd q))))
      (/ -1 2))))

(defn weighted-frequencies
  "applies f to each sample and returns a map {v W} containing
  the total weight W associated with each unique return value v"
  [f samples]
  (let [log-Z (- (reduce
                   log-sum-exp
                   (map get-log-weight samples))
                 (log (count samples)))]
    (reduce (fn [freqs s]
              (let [v (f s)]
                (assoc freqs
                  v (+ (get freqs v 0.0)
                       (exp (- (get-log-weight s)
                               log-Z))))))
            {}
            samples)))

(defn weighted-expectation
  "applies f to each sample and computes a weighted expectation value.
  f may return either a scalar or an array"
  [f samples]
  (let [vs (map f samples)
        lws (map get-log-weight samples)
        vlws (map vector vs lws)
        max-lw (reduce max lws)]
    (loop [vlws vlws
           sum-wv 0.0
           sum-w 0.0]
      (if-let [[v lw] (first vlws)]
        (let [w (exp (- lw max-lw))]
          (recur (rest vlws)
                 (m/add sum-wv (m/mul w v))
                 (m/add sum-w w)))
        (m/div sum-wv sum-w)))))

