(ns analyze.examples.side-effect
  "Warns on invocations of `set!` inside transactions.
  Entry point `forbid-side-effects-in-transaction`"
  (:require [analyze.core :as analyze]
            [clojure.reflect :as reflect]
            clojure.test
            clojure.stacktrace
            clojure.template))

(def transaction-method
  "dosync reduces to a call to this method"
  (let [membrs (-> (reflect/reflect clojure.lang.LockingTransaction) :members)]
    (first (filter #(= 'runInTransaction (:name %)) membrs))))

(defn warn-on-side-effect [exp]
  (when (= :set! (:op exp))
    (binding [*out* *err*]
      (println "WARNING: Side effect in transaction")))
  (doseq [child-exp (:children exp)]
    (warn-on-side-effect child-exp)))

(defn forbid-side-effects-in-transaction [exp]
  (when (and (= :static-method (:op exp))
             (= transaction-method (:method exp)))
    (warn-on-side-effect (first (:args exp))))
  (doseq [child-exp (:children exp)]
    (forbid-side-effects-in-transaction child-exp)))

;; Examples

;; Check a chunk of the core library

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
  (forbid-side-effects-in-transaction exp))

;; Check individual form

(forbid-side-effects-in-transaction
  (analyze/analyze-one '{:ns {:name clojure.core} :context :eval}
                       '(dosync 
                          (do 
                            (fn [] (set! *ns* 'ww)) ; TODO need context information from compiler, or to find it
                            (set! *ns* 'ss)
                            (set! *ns* 'blah)))))
