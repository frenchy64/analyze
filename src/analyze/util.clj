(ns analyze.util
  (:require [clojure.pprint :as pp]))

(defn- dissoc-rec
  "dissoc[iate] keys recursively."
  [m k]
  (into {}
        (for [[key val] (dissoc m k)]
          [key (if (map? val)
                 (dissoc-rec val k)
                 val)])))

(defn print-expr
  "Pretty-prints expr, without :children keys"
  [expr]
  (pp/pprint (dissoc-rec expr :children)))

(defn expr-seq
  "Given an expression, returns a lazy sequence of the expressions
  followed by its children (in a depth first manner)"
  [expr]
  (tree-seq #(-> % :children boolean)
            :children
            expr))
