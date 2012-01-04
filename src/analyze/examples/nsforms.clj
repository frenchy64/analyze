(ns analyze.examples.nsforms
  (:require [analyze.core :as analyze]))

(defn warn-on-naked-use [use-expr]
  (doseq [s (map :val (:args use-expr))
          :when (symbol? s)]
    (println "Warning: Naked use of" (name s) "in" (-> use-expr :env :ns :name))))

(defn use? [expr]
  (and (= :invoke (:op expr))
       (= :var (-> expr :fexpr :op))
       (= 'use (-> expr :fexpr :var meta :name))))

(defn find-and-analyze-use-forms [expr]
  (when (use? expr)
    (warn-on-naked-use expr))
  (doseq [child-expr (:children expr)]
    (find-and-analyze-use-forms child-expr)))

(find-and-analyze-use-forms
 (analyze/analyze-one {:ns {:name 'user} :context :eval}
                      '(ns sjfis (:use [clojure.set :only [union]]
                                       clojure.repl))))


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
  (find-and-analyze-use-forms exp))
