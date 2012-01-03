(set! *warn-on-reflection* true)

(ns analyze.examples.infer
  (:require [analyze.core :as analyze]
            [clojure.java.io :as io]))

(def var-types (atom {}))
(def ^:dynamic *local-types* {})

;(add-hint 'clojure.core/map clojure.lang.LazySeq)

(defmulti infer-type :op)

(defmethod infer-type :constant
  [{:keys [val]}]
  (class val))

(defmethod infer-type :invoke
  [{:keys [env fexpr tag args]}]

(comment
(let [^clojure.lang.LazySeq fs (map io/file ["one1" "one2"])]
  (.getName (first fs)))
  )
