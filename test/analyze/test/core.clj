(ns analyze.test.core
  (:require [analyze.core :as a])
  (:use [midje.sweet]
        [clojure.test]))

(def testenv '{:top-level true
               :ns {:name test.ns
                    :requires {core clojure.core}
                    :imports {Int java.lang.Integer}}})

;; Resolving vars and classes

(deftest resolve-aliased-namespace
  (is (= (-> (a/analyze testenv 'core/foo)
           :info :name)
         'clojure.core/foo)))

(deftest resolve-aliased-class
  (is (= (-> (a/analyze testenv 'Int)
           :info :name)
         'java.lang.Integer)))

(deftest resolve-aliased-field
  (is (= (-> (a/analyze testenv 'Int/SIZE)
           :info :name)
         'java.lang.Integer/SIZE)))

(deftest implicit-javalang-import
  (is 
    (= (-> (a/analyze testenv 'Integer)
         :info :name)
       'java.lang.Integer))

  (is
    (= (-> (a/analyze testenv 'Exception)
         :info :name)
       'java.lang.Exception)))

(deftest implicit-clojurecore-use
  (is 
    (= (-> (a/analyze testenv '+)
         :info :name)
       'clojure.core/+)))

(deftest implicit-clojurecore-exclude
  (is 
    (= (-> (a/analyze (-> testenv 
                        (assoc-in [:ns :excludes] '#{+})
                        (assoc-in [:ns :defs] '{+ +})) 
                      '+)
         :info :name)
       'test.ns/+)))

(deftest implicit-clojurecore-shadowed
  (is 
    (= (-> (a/analyze (assoc-in testenv [:locals] '{+ {:name +}}) '+)
         :info :name)
       '+)))

(deftest def-name-in-correct-namespace
  (is 
    (= (-> (a/analyze '{:ns {:name test.ns}} '(def a 1))
         :name)
       'test.ns/a))

  (is 
    (= (-> (a/analyze '{:ns {:name test.ns}} 'Integer/SIZE)
         :name)
       'test.ns/a)))

;; special forms

(def referenv {:ns {:name test.ns} :top-level true})

(deftest refer-clojurecore-exclude
  (is 
    (= (-> (a/analyze referenv '(refer clojure.core :exclude [+]))
         :excludes 
         (get 'clojure.core))
       '[+])))

(deftest refer-clojure-exclude
  (is
    (= (-> (a/analyze referenv '(refer-clojure :exclude ['+]))
         :excludes 
         (get 'clojure.core))
       '[+])))
