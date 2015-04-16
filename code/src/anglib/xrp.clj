(ns anglib.xrp
  "Sample wrappers for OBSERVE and SAMPLE
  treating ERP and XRP in the same way"
  (:use [anglican runtime emit]))

(defm XRP
  "binds a random process to a XRP,
  returns the XRP"
  [proc] 
  (let [xrp (gensym "xrp")]
    (store xrp proc)
    xrp))

(defm OBSERVE
  "observes a value w.r.t. either an ERP or an XRP"
  [dist value]
  (if (symbol? dist)
    (let [proc (retrieve dist)]
      (observe (produce proc) value)
      (store dist (absorb proc value)))
    (observe dist value)))

(defm SAMPLE
  "samples from either an ERP or an XRP"
  [dist] 
  (if (symbol? dist)
    (let [proc (retrieve dist)
          s (sample (produce proc))]
      (store dist (absorb proc s))
      s)
    (sample dist)))

(defm SAMPLE*
  "blindly (for LMH) samples from either ERP or XRP"
  [dist]
  (if (symbol? dist)
    (let [proc (retrieve dist)
          s (sample* (produce proc))]
      (store dist (absorb proc s))
      s)
    (sample* dist)))
