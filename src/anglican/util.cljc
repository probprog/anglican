(ns anglican.util)


#?(:cljs
   (defn format
     "Similar to Java String's format function for cljs."
     [s & args]
     (goog.string.format s (into-array args))))


