(ns analyze.examples.dynvars
  (:require [analyze.core :as analyze]))

(defn earmuffed? [sym]
  (let [s (name sym)]
    (and (< 2 (count s))
         (.startsWith s "*")
         (.endsWith s "*"))))

(defn check-def [expr]
  (let [v (:var expr)
        s (.sym v)]
    (when (and (earmuffed? s)
               (not (:is-dynamic expr)))
      (println "WARNING: Should" v "be marked dynamic?"))))

(defn find-and-check-defs [expr]
  (when (= :def (:op expr))
    (check-def expr))
  (doseq [child-expr (:children expr)]
    (find-and-check-defs child-expr)))

(find-and-check-defs
 (analyze/analyze-one {:ns {:name 'user} :context :eval}
                      '(def *a* 1)))

(def analyzed
  (map #(apply analyze/analyze-path %) 
       '[["clojure/test.clj" clojure.test]
         ["clojure/set.clj" clojure.set]
         ["clojure/java/io.clj" clojure.java.io]
         ["clojure/stacktrace.clj" clojure.stacktrace]
         ["clojure/pprint.clj" clojure.pprint]
         ["clojure/walk.clj" clojure.walk]
         ["clojure/string.clj" clojure.string]
         ["clojure/repl.clj" clojure.repl]
         ["clojure/core/protocols.clj" clojure.core.protocols]
         ["clojure/template.clj" clojure.template]]))

(doseq [exprs analyzed
        exp exprs]
  (find-and-check-defs exp))
