(ns analyze.examples.load-core
  (:import [clojure.lang Compiler RT DynamicClassLoader])
  (:require [analyze.core :as analyze]))

(defn wall-hack [what class-name member-name & args]
  (letfn [(wall-hack-field [class-name field-name obj]
                           (-> class-name (.getDeclaredField (name field-name))
                             (doto (.setAccessible true)) (.get obj)))
          (wall-hack-method [class-name method-name types obj args]
                            (-> class-name (.getDeclaredMethod (name method-name)
                                                               (into-array Class types))
                              (doto (.setAccessible true))
                              (.invoke obj (into-array Object args))))]
    (condp = what
      :method
      (wall-hack-method class-name member-name (first args) (fnext args) (nnext args))
      :field
      (wall-hack-field class-name member-name (first args)))))

(def a (analyze/analyze-path "clojure/core.clj" 'clojure.core))
