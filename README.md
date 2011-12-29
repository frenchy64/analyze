# Analyzer for Clojure

This is a port of the ClojureScript analyzer, a ClojureScript compilation phase.

# Differences from ClojureScript

- Distinction between :statement / :return / :expr unnessessary 

# Challenges

* Clojure has load-file, require, use and refer
** Impossible to statically verify 
** callable at any time
*** some cases impossible to statically predict aliases, which classes are loaded
**** should emit warning in those cases
*** should use global atoms/vars to keep track of defs/imports/aliases
**** any static namespace description would quickly become out of date

(do 
  (in-ns 'my-ns)
  (require '[clojure.core :as core])  ;; should check current namespace via *analyzer-ns*
  (use '[clojure.repl :only [source]])) ;; should check current requires via `namespaces` atom

* Need to distinguish between forms in fn body and forms not in fn body

(def mynum 1)

mynum ;; need to know this refers to the above definition
;=> 1

(defn myfn []
  mynum) ;; need to know this looks up the current value of mynum

(def mynum 2)

(myfn)
;=> 2

# Usage

## Analyze statements

To see what the analyzer outputs for individual statements, use `analyze.core/analyze`.

```clojure
(analyze.core/analyze '{:ns {:name test.ns}} '(+ 1 1))
{:env {:line 10, :ns {:name test.ns}},
 :op :invoke,
 :f
 {:info {:name clojure.core/+},
  :op :var,
  :env {:context :expr, :line 10, :ns {:name test.ns}},
  :form +},
 :args
 [{:op :constant,
   :env {:context :expr, :line 10, :ns {:name test.ns}},
   :form 1}
  {:op :constant,
   :env {:context :expr, :line 10, :ns {:name test.ns}},
   :form 1}],
 :children
 [{:op :constant,
   :env {:context :expr, :line 10, :ns {:name test.ns}},
   :form 1}
  {:op :constant,
   :env {:context :expr, :line 10, :ns {:name test.ns}},
   :form 1}
  {:info {:name clojure.core/+},
   :op :var,
   :env {:context :expr, :line 10, :ns {:name test.ns}},
   :form +}]}
```

## Analyze namespaces

`analyze.core/analyze-namespace` takes a symbol represeting a namespace, and returns a vector of AST's representing macroexpanded
forms in that namespace.

This tends to give a lot of output. Consider `def`ing the the output to avoid printing at the REPL.

```clojure
(analyze.core/analyze-namespace 'clojure.test)
[...]
```
