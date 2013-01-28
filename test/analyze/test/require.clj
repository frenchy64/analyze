(ns analyze.test.require
  ;; any :use or :refer is acceptable here, as long as the refer'd fn is not 
  ;; present in the user namespace (or wherever analyze is run from)
  (:require [clojure.set :refer (intersection)]))
    
(defn foo [x y]
  (intersection x y))
