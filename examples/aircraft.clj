(ns aircraft
  (:use [embang emit runtime]))

(defanglican aircraft
  ; Radar tracking model inspired by the aircraft example in BLOG.
  ; To keep things simple, all positions are one-dimensional.

  ; Aircraft are identified by integers from 0 to (- num-aircraft 1).
  [assume num-aircraft (+ 1 (sample (poisson 5)))]

  
  ; The true position of an aircraft.
  [assume aircraft-info 
    (mem (lambda (aircraft-id) 
      (let ((position  
              (sample (normal 2. 5.)))
            (num-blips
              (sample (discrete (list 0.1 0.4 0.5))))
            (blips
              (map (lambda (i)
                     (list aircraft-id i (sample (normal position 1.)))) 
                   (range num-blips))))
        (list position blips))))]
  
  [assume all-blips  
    (reduce (lambda (acc aircraft-id)
              (concat (second (aircraft-info aircraft-id)) acc))
            (repeat 3 '(0 0 0)) ; pad all-blips to avoid out-of-bounds
            (range num-aircraft))]

  ;; Observe three blips on the radar screen:
  [observe (normal (count all-blips) 1) 3] 

  ;; Observe the location of the three blips:
  [observe (normal (nth (nth all-blips 0) 2) 1) 1.]
  [observe (normal (nth (nth all-blips 1) 2) 1) 2.]
  [observe (normal (nth (nth all-blips 2) 2) 1) 3.]

  ;; Want to know the number of aircraft and their positions:
  [predict num-aircraft]
  [assume positions 
    (map (lambda (aircraft-id) (first (aircraft-info aircraft-id))) 
         (range num-aircraft))]
  [predict positions])
