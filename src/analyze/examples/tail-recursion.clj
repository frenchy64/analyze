(ns analyze.examples.tail-recursion
  (:require [analyze.core :as analyze]))

;; ## Utility functions

(defn- safe-mapcat
  "Like `mapcat`, but works if the returned values aren't sequences."
  [f & colls]
  (apply concat (map #(if (seq? %) % [%]) (apply map f colls))))

;; ## Test for tail recursion

(defn find-tail-ops
  "Returns a list of the function calls that are in tail position."
  [tree]
  (case (:op tree)
    :def
    (safe-mapcat find-tail-ops (rest (:children tree)))
    :do
    (recur (last (:children tree)))
    :fn-expr
    (safe-mapcat find-tail-ops (:methods tree))
    :fn-method
    (recur (:body tree))
    :invoke
    (or (-> tree :fexpr :local-binding :sym)
        (-> tree :fexpr :var))
    :let
    (recur (:body tree))
    :if
    (map find-tail-ops [(:then tree) (:else tree)])
    nil))

(defn tail-recursive?
  "Returns `true` if there is a call to the function being defined
   in a tail position.  This does not necessarily mean that the tail call
   can be replaced with `recur`, since that does not work with functions of
   different arity, or across `try`."
  [fn-tree]
  (let [fn-name (or (-> fn-tree :name) (-> fn-tree :var))
        tail-ops (find-tail-ops fn-tree)]
    (boolean (when fn-name (some (partial = fn-name) tail-ops)))))

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
        exp (filter (comp #{:def :fn-expr} :op) exprs)]
  (if (tail-recursive? exp)
    (println "WARNING: possible tail recursive function not using recur"
             (or (-> exp :name)
                 (-> exp :var)))))

(comment
  (require 'clojure.pprint)
  (tail-recursive?
   (analyze/analyze-one {:ns {:name 'clojure.repl} :context :eval}
                        '(def foo (list))))
  (tail-recursive?
   (analyze/analyze-one {:ns {:name 'clojure.repl} :context :eval}
                        '(defn foo [x]
                           (if (< x 10)
                             (foo (inc x))
                             x))))

  (tail-recursive?
   (analyze/analyze-one {:ns {:name 'clojure.test} :context :eval}
                        '(defn testing-vars-str
  "Returns a string representation of the current test.  Renders names
  in *testing-vars* as a list, then the source file and line of
  current assertion."
  {:added "1.1"}
  [m]
  (let [{:keys [file line]} m]
    (str
     ;; Uncomment to include namespace in failure report:
     ;;(ns-name (:ns (meta (first *testing-vars*)))) "/ "
     (reverse (map #(:name (meta %)) *testing-vars*))
     " (" file ":" line ")"))))
   )


  )
