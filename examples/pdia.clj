(ns pdia
  (:use [embang emit runtime])
  (:use [angsrc crp]))

(defanglican pdia
  [assume vocabulary '(A B)]

  [assume top-level (crp 10.0)]

  [assume symbol->state-distribution
    (mem (lambda (symbol) (dp 10.0 top-level)))]

  [assume state-symbol->next-state
    (mem (lambda (state symbol) ((symbol->state-distribution symbol))))]

  [assume state->observation-model
    (mem (lambda (state) 
      (sample (dirichlet (repeat (count vocabulary) 1.0)))))]

  [assume observation (lambda (state) 
    (sample* (categorical (map list vocabulary (state->observation-model state)))))]

  [assume sample-words (lambda (last-state) 
    (if (sample* (flip 0.1))
      ()
      (let ((word (observation last-state)))
        (conj (sample-words (state-symbol->next-state last-state word))  
               word))))]

  [assume state-1 1]
  [observe (categorical (map list vocabulary (state->observation-model state-1))) 'A]
  [assume state-2 (state-symbol->next-state state-1 'A)]
  [observe (categorical (map list vocabulary (state->observation-model state-2))) 'B]
  [assume state-3 (state-symbol->next-state state-2 'B)]
  [observe (categorical (map list vocabulary (state->observation-model state-3))) 'B]
  [assume state-4 (state-symbol->next-state state-3 'B)]
  [observe (categorical (map list vocabulary (state->observation-model state-4))) 'A]
  [assume state-5 (state-symbol->next-state state-4 'A)]
  [observe (categorical (map list vocabulary (state->observation-model state-5))) 'B]
  [assume state-6 (state-symbol->next-state state-5 'B)]
  [observe (categorical (map list vocabulary (state->observation-model state-6))) 'B]
  [assume state-7 (state-symbol->next-state state-6 'B)]
  [observe (categorical (map list vocabulary (state->observation-model state-7))) 'B]
  [assume state-8 (state-symbol->next-state state-7 'B)]
  [observe (categorical (map list vocabulary (state->observation-model state-8))) 'B]
  [assume state-9 (state-symbol->next-state state-8 'B)]
  [observe (categorical (map list vocabulary (state->observation-model state-9))) 'A]
  [assume state-10 (state-symbol->next-state state-9 'A)]
  [observe (categorical (map list vocabulary (state->observation-model state-10))) 'A]
  [assume state-11 (state-symbol->next-state state-10 'B)]
  [observe (categorical (map list vocabulary (state->observation-model state-11))) 'A]
  [assume state-12 (state-symbol->next-state state-11 'B)]
  [observe (categorical (map list vocabulary (state->observation-model state-12))) 'B]
  [assume state-13 (state-symbol->next-state state-12 'B)]
  [observe (categorical (map list vocabulary (state->observation-model state-13))) 'B]
  [predict (sample-words state-1)])

(defanglican optimized
  [assume vocabulary '(A B)]

  [assume top-level (crp 10.0)]
  [assume model-prior (dirichlet (repeat (count vocabulary) 1.0))]
  [assume flipped-coin (flip 0.1)]

  [assume symbol->state-distribution
    (mem (lambda (symbol) (dp 10.0 top-level)))]

  [assume state-symbol->next-state
    (mem (lambda (state symbol) ((symbol->state-distribution symbol))))]

  [assume state->observation-model (mem (lambda (state) (sample model-prior)))]

  [assume observation-noise 
    (mem (lambda (state)
           (categorical (map list vocabulary (state->observation-model state)))))]

  [assume observation (lambda (state) (sample* (observation-noise state)))]

  [assume sample-words (lambda (last-state) 
    (if (sample* flipped-coin) 
      ()
      (let ((word (observation last-state)))
        (conj (sample-words (state-symbol->next-state last-state word))  
               word))))]

  [assume state-1 1]
  [observe (observation-noise state-1) 'A]
  [assume state-2 (state-symbol->next-state state-1 'A)]
  [observe (observation-noise state-2) 'B]
  [assume state-3 (state-symbol->next-state state-2 'B)]
  [observe (observation-noise state-3) 'B]
  [assume state-4 (state-symbol->next-state state-3 'B)]
  [observe (observation-noise state-4) 'A]
  [assume state-5 (state-symbol->next-state state-4 'A)]
  [observe (observation-noise state-5) 'B]
  [assume state-6 (state-symbol->next-state state-5 'B)]
  [observe (observation-noise state-6) 'B]
  [assume state-7 (state-symbol->next-state state-6 'B)]
  [observe (observation-noise state-7) 'B]
  [assume state-8 (state-symbol->next-state state-7 'B)]
  [observe (observation-noise state-8) 'B]
  [assume state-9 (state-symbol->next-state state-8 'B)]
  [observe (observation-noise state-9) 'A]
  [assume state-10 (state-symbol->next-state state-9 'A)]
  [observe (observation-noise state-10) 'A]
  [assume state-11 (state-symbol->next-state state-10 'B)]
  [observe (observation-noise state-11) 'A]
  [assume state-12 (state-symbol->next-state state-11 'B)]
  [observe (observation-noise state-12) 'B]
  [assume state-13 (state-symbol->next-state state-12 'B)]
  [observe (observation-noise state-13) 'B]
  [predict (sample-words state-1)])
