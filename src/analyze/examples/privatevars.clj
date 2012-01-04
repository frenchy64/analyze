(ns analyze.examples.privatevars
  (:require [analyze.core :as analyze]
            [clojure.set :as set]
            [clojure.pprint :as pp]))

(defn- unused-fn [] nil)
(def ^:private unused-var 0)

(defn defs [expr]
  (apply concat
         (when (= :def (:op expr)) [(:var expr)])
         (map defs (:children expr))))

(defn private-defs [expr]
  (filter #(:private (meta %))
          (defs expr)))

(defn var-count [expr]
  (if (= :var (:op expr))
    {(:var expr) 1}
    (apply merge-with +
           (map var-count (:children expr)))))

(defn check-usage-of-private-vars [exprs]
  (let [v-count (apply merge-with + (map var-count exprs))]
    (doseq [pvar (mapcat private-defs exprs)]
      (when-not (get v-count pvar)
        (println "Private variable" pvar "is never used")))))

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
         ["clojure/template.clj" clojure.template]
         ["analyze/examples/privatevars.clj" analyze.examples.privatevars]]))

(doseq [exprs analyzed]
  (check-usage-of-private-vars exprs))
