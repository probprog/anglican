(ns anglib.xrp
  (:use [embang runtime emit]))

(defm XRP
  [proc] 
  (let [xrp (gensym "xrp")]
    (store xrp proc)
    xrp))

(defm OBSERVE
  [dist value]
  (if (symbol? dist)
    (let [proc (retrieve dist)]
      (observe (produce proc) value)
      (store dist (absorb proc value)))
    (observe dist value)))

(defm SAMPLE
  [dist value]
  (if (symbol? dist)
    (let [proc (retrieve dist)
          s (sample (produce proc))]
      (store dist (absorb proc s))
      s)
    (sample dist)))
