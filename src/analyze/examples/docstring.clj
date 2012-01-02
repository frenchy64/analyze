(ns analyze.examples.docstring
  (:require [analyze.core :as analyze]))

(defn check-def [exp]
  (when (= :fn-expr (-> exp :init :op))
    (doseq [method (-> exp :init :methods)]
      (let [body (:body method)]
        (when (and (= :do (:op body))
                   (< 1 (count (-> body :exprs))))
          (let [first-exp (-> body :exprs first)]
            (when (and (= :literal (:op first-exp))
                       (string? (:val first-exp)))
              (binding [*out* *err*]
                (println "WARNING: Suspicious string, possibly misplaced docstring," (-> exp :var))))))))))

(defn find-and-check-defs [exp]
  (when (= :def (:op exp))
    (check-def exp))
  (doseq [child-exp (:children exp)]
    (find-and-check-defs child-exp)))

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

(doseq [exprs analyzed
        exp exprs]
  (find-and-check-defs exp))

(comment
(find-and-check-defs
  (analyze/analyze-one {:ns {:name 'clojure.repl} :context :eval}
                       '(defn a []
                          "asdf"
                          (+ 1 1))))
  )

