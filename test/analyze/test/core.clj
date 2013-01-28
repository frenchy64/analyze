(ns analyze.test.core
  (:require [clojure.test :refer :all]
            [analyze.core :refer :all]))

(deftest test-ast
  (is (= (-> (ast 1) :op)
         :number)))

(deftest test-ast-in-ns
  (is (= (-> (ast-in-ns clojure.core 1) :op)
         :number)))

(deftest test-keyword-invoke-kw
  (is (= (-> (ast (let [a {}] (:abc a)))
           :fexpr :methods first :body :exprs first :body :exprs first :kw :val)
         :abc)))

(deftest test-refer
  ; changed Var mappings do not apply over members of the same `do`
  (is (= (-> (ast (do (require '[clojure.set])
                    (refer 'clojure.set 
                           :only '[intersection] 
                           :rename '{intersection require})
                    require))
           :exprs last :var)
         #'clojure.core/require)) ;should be `clojure.set/intersection`
  (is (analyze-ns (pb-reader-for-ns 'analyze.test.require) 'my-ns 'my-ns)))
