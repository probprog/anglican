(ns neural-net
  (use [anglican runtime emit]))

; Define activation function
(defun activate-sigmoid (v) (- (/ 2 (+ 1 (exp (* -10 (+ v -0.5))))) 1))
(defun activate-threshold (v) (if (<= v 0.5) -1 1))

(declare make-neural-net)

(defanglican neural-net
  (make-neural-net activate-sigmoid))

(defanglican threshold
  (make-neural-net activate-threshold))

(defun make-neural-net (activate)
  ; Define expected inputs and outputs as lists
  [assume Input  (list (list -1 -1) (list -1  1) (list  1 -1) (list  1  1))]
  [assume Output (list           0            1            1            0)]

  ; Helper functions for accessing expected inputs / outputs
  [assume I (lambda (i l j) (nth (nth Input i) j))]
  [assume O (lambda (i)     (nth Output i))]

  ; Define number of layers
  [assume L-MAX 3]

  ; Define number of nodes in each layer
  [assume N-MAX (mem (lambda (l) (- (cond ((= l 0) 2)
                                          ((= l 1) 4)
                                          ((= l 2) 2)
                                          ((= l 3) 1)) 1)))]

  ; Define weights dynamically as required
  ; l = layer, n = node id; j = input node id in layer l-1
  [assume w (mem (lambda (l n j) (sample (normal 0 0.5))))]

  ; Evaluate a single weighted input
  ; f = function to evaluate to get input value, i = input training pattern id
  [assume get-input  (lambda (f i l n j) (* (f i l j) (w l n j)))]

  ; Sum all weighted inputs to a node
  [assume sum-inputs (lambda (f i l n j) (if (= j 0) (get-input f i l n j) (+ (sum-inputs f i l n (- j 1)) (get-input f i l n j))))]

  ; Evaluate node activation
  [assume N (mem (lambda (i l n) (activate (if (= l 1) (sum-inputs I i (- l 1) n (N-MAX 0))
                                                       (sum-inputs N i (- l 1) n (N-MAX (- l 1)))))))]

  ; Compute output value based on layer 2 nodes
  [assume p-O (mem (lambda (i) (if (>= (N i L-MAX (N-MAX L-MAX)) 0) 1 0)))]

  ; Train the network using our training data
  [observe (normal (p-O 0) 0.1) (O 0)]
  [observe (normal (p-O 1) 0.1) (O 1)]
  [observe (normal (p-O 2) 0.1) (O 2)]
  [observe (normal (p-O 3) 0.1) (O 3)]

  ; Predict response to the four input values
  [predict (list (I 0 0 0) (I 0 0 1) (p-O 0))]
  [predict (list (I 1 0 0) (I 1 0 1) (p-O 1))]
  [predict (list (I 2 0 0) (I 2 0 1) (p-O 2))]
  [predict (list (I 3 0 0) (I 3 0 1) (p-O 3))])
