(ns analyze.examples.docstring
  (:require [analyze.core :as analyze]))

(defn check-def [exp]
  (println "Found def" (:var exp)))

(defn find-and-check-defs [exps]
  (doseq [exp exps]
    (println "exp" (:op exp))
    (when-let [children (seq (:children exp))]
      (find-and-check-defs children))
    (when (= :def (:op exp))
      (check-def exp))))

(def analyzed
  (map #(apply analyze/load-path %) 
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

(doseq [exprs analyzed]
  (find-and-check-defs exprs))

;; Output

;Analyzing clojure.set
;Found suspicious function clojure.set/bubble-max-key , in clojure.set

