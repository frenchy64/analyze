(ns analyze.test.core
  (:require [clojure.test :refer :all]
            [analyze.core :refer :all]))

(deftest test-ast
  (is (= (-> (ast 1) :op)
         :number)))

(deftest test-ast-in-ns
  (is (= (-> (ast-in-ns clojure.core 1) :op)
         :number)))
