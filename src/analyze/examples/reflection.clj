(set! *warn-on-reflection* false)

(ns analyze.examples.reflection
  "Same as *warn-on-reflection*"
  (:require [analyze.core :as analyze]
            clojure.test
            clojure.stacktrace
            clojure.template))

(defn check-new [exp]
  (when (not (:ctor exp))
    (println "WARNING: Unresolved constructor" (:class exp) (-> exp :env :ns :name))))

(defn check-static-method [exp]
  (when (not (:method exp))
    (println "WARNING: Unresolved static method" (:method-name exp) (:class exp) (-> exp :env :ns :name))))

(defn check-instance-method [exp]
  (when (not (:method exp))
    (println "WARNING: Unresolved instance method" (:method-name exp) (:class exp) (-> exp :env :ns :name))))

(defn check-static-field [exp]
  (when (not (:field exp))
    (println "WARNING: Unresolved static field" (:field-name exp) (:class exp) (-> exp :env :ns :name))))

(defn check-instance-field [exp]
  (when (not (:field exp))
    (println "WARNING: Unresolved instance field" (:field-name exp) (:class exp) (-> exp :env :ns :name))))


(defn check-for-reflection [exp]
  (condp = (:op exp)
    :new (check-new exp)
    :static-method (check-static-method exp)
    :instance-method (check-instance-method exp)
    :static-field (check-static-field exp)
    :instance-field (check-instance-field exp)
    nil)

  (doseq [c (:children exp)]
    (check-for-reflection c)))

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
  (check-for-reflection exp))

(comment
(analyze-one {:ns {:name 'clojure.core} :context :eval} '(Integer. (+ 1 1))
(analyze-one {:ns {:name 'clojure.core} :context :eval} '(Integer. (+ 1 1)))
(analyze-one {:ns {:name 'clojure.core} :context :eval} '(Integer. (+ 1 (even? 1))))
  )
)
