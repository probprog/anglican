(ns fred
  (use (embang runtime emit)))

;;; Is Fred guilty? Expanded and then counting version.

;; A note on categorical distribution --- weights do not
;; have to sum to 1., they are normalized. You are welcome
;; to play with probabilities by changing just one of the
;; weights.

;; Fred is person number 0
(defun is-fred? (person) (= person 0))

;; Helper function --- is the guilty person Fred?
(defun is-guilty-fred? (guilty)
  (assume sex (mem (lambda (person) 
                     (if (is-fred? person) 'male
                       (sample (categorical '((male 0.49)
                                              (female 0.51))))))))
  (assume size (mem
                 (lambda (person)
                   (if (is-fred? person) 'large
                     (let ((sex (sex person)))
                       (sample
                         ;; Males are large than females.
                         (cond ((= sex 'male)
                                (categorical '((small 0.2)
                                               (medium 0.3)
                                               (large 0.5))))
                               ((= sex 'female)
                                (categorical '((small 0.4)
                                               (medium 0.4)
                                               (large 0.2)))))))))))

  (assume hair (mem (lambda (person)
                      (if (is-fred? person) 'purple
                        (sample
                          ;; Only two people with purple hair.
                          (categorical '((black 0.7)
                                         (blond 0.28)
                                         (purple 0.02))))))))

  ;; Most witnesses say the thief was large.
  (observe (categorical '((small 0.1) (medium 0.2) (large 0.7)))
           (size guilty))
  ;; Most witnesses say the thief had purple hair.
  (observe (categorical '((black 0.15) (purple 0.8) (blond 0.05)))
           (hair guilty))

  (predict (is-fred? guilty)))

;; Expanded version
(defanglican fred
  (assume guilty (sample (uniform-discrete 0 99)))
  (is-guilty-fred? guilty))

;; Counting version
(defanglican counting
  (assume guilty (sample (discrete '(0.01 0.99))))
  (is-guilty-fred? guilty))
