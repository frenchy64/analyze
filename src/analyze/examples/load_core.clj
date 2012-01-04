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

(defmacro deftesteval
  "Experimental - like defmacro, except defines a named function whose
  body is the expansion, calls to which may be expanded inline as if
  it were a macro. Cannot be used with variadic (&) args."
  {:added "1.0"}
  [name & decl]
  (let [[pre-args [args expr]] (split-with (comp not vector?) decl)]
    `(do
       (defn ~name ~@pre-args ~args )
       (fn ~name ~args ~expr))))

(deftesteval myfn [x] `(+ ~x))

(analyze/analyze-one '{:ns {:name analyze.examples.load-core} :context :eval}
                     '(deftesteval myfn [x] `(+ ~x)))


;(def a (analyze/analyze-path "clojure/core.clj" 'clojure.core))
