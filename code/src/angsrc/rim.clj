(ns angsrc.rim
  (:use [embang emit runtime]))

;;; Anglican example from fwood/anglican issue #134
(defanglican rim
  ;;; insert_nth - function
  ;; Inserts item into nth position in a collection, where head is position 0 and
  ;; end is position (count collection)
  [assume insert_nth
      (lambda (item n coll)
          (if (empty? coll)
              (list item)
              (loop   ((item item)
                       (i 0)
                       (fore (list))
                       (next (first coll))
                       (back (rest coll))
                      )
                      (cond
                          ((= i n) (concat fore (list item next) back))
                          ((empty? back) (concat fore (list next item)))
                          (else
                              (recur
                                  item
                                  (inc i)
                                  (concat fore (list next))
                                  (first back)
                                  (rest back)
                              )
                          )
                      )
              )
          )
      )
  ]
  
  ;;; shuffle - function
  ;; Takes a list and shuffles it randomly (using the rim - repeated insertion
  ;; model)
  [assume shuffle
      (lambda (orig)
          (loop 
              (
                  (n 0)
                  (shuffled (list))
                  (next (first orig))
                  (remains (rest orig))
              )
              (if (empty? remains)
                  (insert_nth next n shuffled)
                  (recur
                      (sample (uniform-discrete 0 (+ 2 (count shuffled))))
                      (insert_nth next n shuffled)
                      (first remains)
                      (rest remains)
                  )
              )
          )
      )
  ]
  
  ;;; pair_with_each - function
  ;; takes item and collection and generates collection of pairs with item in the
  ;; left position and each member of collection in the right position of a pair
  [assume pair_with_each 
      (lambda (item col)
          (loop  ((litem item)
                  (ritem (first col))
                  (remains (rest col))
                  (collected []))
              (if (empty? remains)
                  (conj collected (list litem ritem))
                  (recur litem
                         (first remains)
                         (rest remains)
                         (conj collected (list litem ritem)))
              )
          )
      )
  ]
  
  ;;; all_pairwise_prefs - function
  ;; turns a preference ranking into a complete set of pairwise orders, e.g.
  ;; ranking (list "a" "b" "c") produces pairwise preferences  (list "a" "b"),
  ;; (list "a" "c") and  (list "b" "c")
  [assume all_pairwise_prefs
      (lambda (ranking)
          (loop   ((item (first ranking))
                   (remains (rest ranking))
                   (results [])
                  )
                  (if (= 0 (count remains))
                      results
                      (recur (first remains)
                             (rest remains)
                             (concat results (pair_with_each item remains)))
                  )
          )
      )
  ]
  
  ;;; preference_sample - function
  ;; Takes a complete ranking over items and samples a pairwise preference from 
  ;; all possible pairwise preferences.
  [assume preference_sample
      (lambda (ranking)
          (begin
              (define pairs (all_pairwise_prefs ranking))
              (define N (count pairs))
              (define n (sample (uniform-discrete 0 N)))
              (nth pairs n)
          )
      )    
  ]
  
  
  ;;; Tests for insert_nth
  ;;[predict (insert_nth "z" 0 alphabet)]
  ;;[predict (insert_nth "z" 2 alphabet)]
  ;;[predict (insert_nth "z" (count alphabet) alphabet)]
  ;;[predict (insert_nth "z" 0 (list))]
  ;;[predict (insert_nth "b" 0 (list "a"))]
  ;;[predict (insert_nth "b" 1 (list "a"))]
  
  ;;; Tests for shuffle
  ;;[predict (shuffle (list "a" "b"))]
  ;;[predict (shuffle alphabet)]
  
  ;;; Tests pair_with_each
  ;; [predict (pair_with_each "a" (list "b" "c" "d"))]
  
  ;;; Tests for all_pairwise_prefs
  ;; [predict (all_pairwise_prefs (list "a" "b" "c" "d"))]
  
  ;;; Tests for preference_sample
  ;; [predict (preference_sample (list "a" "b" "c" "d"))]
  
  ;;; Tests for observed sample
  [assume random_order (shuffle (list "a" "b" "c" "d"))]
  ;; [observe (preference_sample random_order) (list "a" "b")]
  [observe (flip (if (= '("a" "b")
                        (preference_sample random_order))
                   0.99
                   0.01))
   true]
                        
  [predict random_order])
