(ns analyze.test.core
  (:require [analyze.core :as a])
  (:use [midje.sweet]))

(def testenv '{:ns {:name test.ns
                    :requires {core clojure.core}
                    :imports {Int java.lang.Integer}}})

(facts "resolve aliased namespace"
  (-> (a/analyze testenv 'core/foo)
    :info :name)
  => 'clojure.core/foo)

(facts "resolve aliased class"
  (-> (a/analyze testenv 'Int)
    :info :name)
  => 'java.lang.Integer)

(facts "resolve aliased field"
  (-> (a/analyze testenv 'Int/SIZE)
    :info :name)
  => 'java.lang.Integer/SIZE)

(facts "implicit java.lang import"
  (-> (a/analyze testenv 'Integer)
    :info :name)
  => 'java.lang.Integer

  (-> (a/analyze testenv 'Exception)
    :info :name)
  => 'java.lang.Exception)

(facts "implicit clojure.core use"
  (-> (a/analyze testenv '+)
    :info :name)
  => 'clojure.core/+)

(facts "implicit clojure.core exclude"
  (-> (a/analyze (-> testenv 
                   (assoc-in [:ns :excludes] '#{+})
                   (assoc-in [:ns :defs] '{+ +})) 
                 '+)
    :info :name)
  => 'test.ns/+)

(facts "implicit clojure.core shadowed"
  (-> (a/analyze (assoc-in testenv [:locals] '{+ {:name +}}) '+)
    :info :name)
  => '+)

(facts "def name in correct namespace"
  (-> (a/analyze '{:ns {:name test.ns}} '(def a 1))
    :name)
  => 'test.ns/a)

(facts "def name in correct namespace"
  (-> (a/analyze '{:ns {:name test.ns}} 'Integer/SIZE)
    :name)
  => 'test.ns/a)

;; special forms

(facts "refer clojure.core :exclude"
  (-> (a/analyze '{:ns {:name test.ns}} '(refer clojure.core :exclude [+]))
    :excludes 
    (get 'clojure.core))
  => '[+])

(facts "refer-clojure :exclude"
  (-> (a/analyze '{:ns {:name test.ns}} '(refer-clojure :exclude ['+]))
    :excludes 
    (get 'clojure.core))
  => '[+])
