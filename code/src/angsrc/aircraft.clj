(ns angsrc.aircraft
  (:use [embang emit runtime]))

(defanglican aircraft
  ; Radar tracking model inspired by the aircraft example in BLOG.
  ; To keep things simple, all positions are one-dimensional.
  
  ; Aircraft are identified by integers from 0 to (- num_aircraft 1).
  [assume num_aircraft (+ 1 (sample (poisson 5)))]
  
  ; The true position of an aircraft.
  [assume aircraft_position (lambda (aircraft_id) (sample (normal 0 10)))]
  
  ; Each aircraft produces either zero blips or one blip.
  [assume num_blips (mem (lambda (aircraft_id) (sample (discrete (list 0.1 0.9)))))]
  
  ; Blips are identified by lists like ('blip 0 'ofaircraft 45).
  [assume blip_ids (lambda (aircraft_id)
      (map (lambda (i) (list 'blip i 'ofaircraft aircraft_id)) (range 0 (num_blips aircraft_id)))
  )]
  
  ; Given a blip, return the aircraft that generated it.
  [assume source (mem (lambda (blip_id) (nth blip_id 2)))]
  
  ; The blip position is a noisy observation of the true aircraft position.
  [assume blip_position (mem (lambda (blip_id) (sample (normal (aircraft_position (source blip_id)) 1))))]
  
  ; Total number of blips (needed to express the observations).
  [assume num_blips_total (reduce + 0 (map num_blips (range 0 num_aircraft)))]
  
  ; List of all blips (needed to express the observations).
  [assume all_blips (map first
                      (reduce conj () (filter
                                        (lambda (lst) (if (nil? lst) false true))
                                        (map blip_ids (range 0 num_aircraft)))))]
 
  ; Observe three blips on the radar screen:
  [observe (normal num_blips_total 0.001) 3]
  
  ; Observe the location of the three blips:
  [observe (normal (blip_position (nth all_blips 0)) 0.001) 100.0]
  [observe (normal (blip_position (nth all_blips 1)) 0.001) 200.0]
  [observe (normal (blip_position (nth all_blips 2)) 0.001) 300.0]
  
  ; Want to know the number of aircraft and their positions:
  [predict num_aircraft]
  [predict (map aircraft_position (range 0 num_aircraft))])
