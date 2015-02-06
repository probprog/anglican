(ns angsrc.hdp-hmm-ks-original
  (:require [clojure.core.matrix
             :refer [identity-matrix join reshape
                     add sub mul mmul inverse]
             :rename {identity-matrix eye}])
  (:use [embang emit runtime]
        [angsrc crp dp-mem]
        hdp-hmm-ks-data))

(with-primitive-procedures [eye join reshape mul mmul inverse]
  (defanglican hdp-hmm-ks-original
    ;; next 4 directives are an HDP-HMM backbone in CRF representation
    [assume G-0 (crp 1.0)]
    [assume sticky 0.2]
    [assume trans-dist (mem (lambda (state)
                                    (DPmem 1.0 G-0)))]
    [assume transition (lambda (prev-state)
                         (if (sample (flip sticky))
                           prev-state 
                           ((trans-dist prev-state))))]
    [assume activity (mem (lambda (t)
                            (if (<= t 0) (G-0)
                              (transition (activity (- t 1))))))]

    ;; Could choose anything
    [assume latent-dimension 3]
    ;; Dimensionality of MOCAP data
    [assume observable-dimension 62]

    ;; Motion parameters for each activity Kalman Filter
    ;;---------------------------------------------------
    ;; prior on A - state transition matrix 
    [assume pose-transition-prior
            (lambda () (mul 0.9 (eye latent-dimension)))] 
    ;; prior on W where the process noise is drawn from Normal(w|0,W)
    [assume pose-transition-covariance-prior
            (lambda ()
              (inverse 
               (sample (wishart (+ 1 latent-dimension)
                                (mul 0.1 (eye latent-dimension))))))]
    ;; prior on H, the measurement matrix relating state to
    ;; measurement. This needs to be 62x3 with reasonable
    ;; prior params.
    [assume measurement-multiple-prior 
     (lambda () 
       (reshape
         ;;; 1 x 186
         (join 
           (sample (mvn (repeat observable-dimension 0) (eye observable-dimension))) ; 1x62
           (sample (mvn (repeat observable-dimension 0) (eye observable-dimension))) ; 1x62
           (sample (mvn (repeat observable-dimension 0) (eye observable-dimension)))) ; 1x62
         '(3 62)))]
    ;; prior on Q, the true value of the measurement noise,
    ;; drawn from Normal(v|0,Q)
    [assume measurement-covariance-prior 
            (lambda () (mul 0.1 (eye observable-dimension)))]
    [assume initial-pose 
            (sample (mvn (repeat latent-dimension 0)
                         (eye latent-dimension)))] ; prior on initial pose

    ;; lazy per-activity activity model parameter generator
    [assume activity-motion-model-params 
     (mem 
       (lambda (a) 
         (list 
           (pose-transition-prior) 
           (pose-transition-covariance-prior) 
           (measurement-multiple-prior) 
           (measurement-covariance-prior))))]

    ;; helper function for accessing activity model KF parameters
    [assume A (lambda (a) (nth (activity-motion-model-params a) 0))]
    [assume H (lambda (a) (nth (activity-motion-model-params a) 1))]
    [assume W (lambda (a) (nth (activity-motion-model-params a) 2))]
    [assume Q (lambda (a) (nth (activity-motion-model-params a) 3))]

    ;; generator of true latent pose (varying activity)
    [assume latent-pose
            (mem (lambda (t) 
                   (if (<= t 0)
                     initial-pose
                     (sample
                       (mvn (mmul (latent-pose (- t 1)) (A (activity (- t 1))))
                            (H (activity (- t 1))))))))]

    ;; generator of true latent pose (single activity for
    ;; whole time sequence)
    [assume latent-pose-single-activity
            (mem (lambda (a t) 
                   (if (<= t 0)
                     initial-pose
                     (sample
                       (mvn (mmul (latent-pose-single-activity a (- t 1)) (A a))
                            (H a))))))]

    ;; Activity model parameters learned in parallel,
    ;; allowing the top-tier model to draw samples from
    ;; their posterior distributions.

    ;; single activity observations
    (reduce (lambda (_ single-activity)
              (let ((activity (first single-activity))
                    (data (second single-activity)))
                (reduce (lambda (_ record)
                          (let ((time (first record))
                                (pose (second record)))
                            [observe (mvn (mmul (latent-pose-single-activity activity time)
                                                (W activity))
                                          (Q activity))
                                     pose]))
                        nil data)))
            nil (list (list 1 walking-data)
                      (list 2 running-data)
                      (list 3 jumping-data)
                      (list 4 pick-up-data)))

    ;; observe multiple activities with transitions in data
    (reduce (lambda (_ record)
              (let ((time (first record))
                    (pose (second record)))
                [observe (mvn (mmul (latent-pose time)
                                    (W (activity time)))
                              (Q (activity time)))
                         pose]))
            nil mixed-data)

    ;; predict latent activity class 
    [predict (map activity (range 1164))]))
