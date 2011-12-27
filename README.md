# Analyzer for Clojure

This is a port of the ClojureScript analyzer, a ClojureScript compilation phase.

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
