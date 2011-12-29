(ns analyze.test.dynamic)

(def my-alias
  '[core clojure.core])

(defn dynamic-alias [the-alias]
  (apply alias the-alias))

;; Alias core -> clojure.core a side effect at runtime
;; How to compute this statically?
(dynamic-alias my-alias)

core/+
;=> clojure.core/+
