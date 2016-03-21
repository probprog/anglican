(ns anglib.conj-exp
  (require [anglican.runtime :as run 
            :refer [defdist defproc distribution 
                    sample observe 
                    produce absorb
                    chi-squared discrete gamma mvn wishart 
                    log-gamma-fn]]
           [anglican.emit :as emit]
           [anglican.stat :as stat]
           [clojure.core.matrix :as mat]))





