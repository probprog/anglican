(ns embang.inference
  (:require [clojure.string :as str]
            [clojure.data.json :as json]
            embang.trap)
  (:use embang.state
        [embang.runtime :only [sample observe]]))

;;; Inference multimethod

(defmulti infer (fn [algorithm & _] algorithm))

;;; Checkpoints

(defmulti checkpoint (fn [alg cpt] [alg (type cpt)]))

;; default method implementations

(defmethod checkpoint [::algorithm embang.trap.observe] [algorithm obs]
  #((:cont obs) nil (add-log-weight (:state obs)
                                    (observe (:dist obs) (:value obs)))))

(defmethod checkpoint [::algorithm embang.trap.sample] [algorithm smp]
  #((:cont smp) (sample (:dist smp)) (:state smp)))

(defmethod checkpoint [::algorithm embang.trap.result] [algorithm res]
  res)

;;; Running a single particle until checkpoint

(defn exec
  "executes the program until a checkpoint"
  [algorithm prog & args]
  (loop [step (apply trampoline prog args)]
    (let [next (checkpoint algorithm step)]
      (if (fn? next)
        (recur (trampoline next))
        next))))

;;; Resampling particles in SMC-like algorithms

(defn resample
  "resamples particles proportionally to their current weights"
  ([particles] (resample particles (count particles)))
  ([particles number-of-new-particles]
   (let [weights (map (comp #(Math/exp %) get-log-weight :state)
                      particles)
         total-weight (reduce + weights)]

     (if (= total-weight 0.) particles   ; all particles have
       ; the same weight
       ;;; Systematic sampling

       ;; invariant bindings for sampling
       (let [step (/ total-weight number-of-new-particles)
             all-weights weights     ; particles are circular
             all-particles particles]

         (loop [x (rand total-weight)
                n 0      ; number of particles sampled so far
                acc 0    ; upper bound of the current segment
                weights weights
                particles particles
                new-particles nil]
           (if (= n number-of-new-particles)
             new-particles
             (let [[weight & next-weights] weights
                   [particle & next-particles] particles
                   next-acc (+ acc weight)]
               (if (< x next-acc)

                 ;; Found the wheel segment into which x has fallen.
                 ;; Advance x by step for the next particle's segment.
                 (recur (+ x step) (+ n 1) 
                        acc weights particles
                        (conj new-particles
                              (update-in
                                particle [:state]
                                set-log-weight 0.)))

                 ;; Otherwise, keep going through the particle's 
                 ;; segments, recycling the list of particles and
                 ;; their weights when necessary.
                 (recur x n
                        next-acc
                        (or next-weights all-weights)
                        (or next-particles all-particles)
                        new-particles))))))))))

;;; Output

(defmulti print-predict (fn [_ _ _ format] format))

(defmethod print-predict :anglican [label value weight format]
  (println (str/join "," [label value weight])))

(defmethod print-predict :clojure [label value weight format]
  (prn [label value weight]))

(defmethod print-predict :json [label value weight format]
  (json/pprint [(str label) value weight])
  (prn))
  
(defn print-predicts
  "print predicts as returned by a probabilistic program
  in the specified format"
  [state output-format]
  (let [log-weight (get-log-weight state)]
    (doseq [[name value] (get-predicts state)]
      (print-predict name value (Math/exp log-weight) output-format))))
