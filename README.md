# Interface to Clojure's Analyzer

Clojure's analysis compilation phase holds rich information about Clojure forms, like type/reflection information.

_analyze_ provides an interface to this phase, callable a la carte. The output is similar to ClojureScript's analyzer.

# Progress

See [http://dev.clojure.org/pages/viewpage.action?pageId=4063367]

# Examples

## General Usage

This library takes a form, and some environment information, then passes it to the Compiler's analysis phase.

We then recursively convert the Java objects returned by the Compiler into maps, with :op and :env keys.
The actual keys that get converted are easy to check, just search the source at `analyze.core`, using the :op 
keys as reference.

This is kind of rich information the analyzer can derive from expressions.

```
(Integer. 2)
```

expands to:

```
{:op :new,
 :env {:locals {}, :ns {:name clojure.core}},
 :ctor
 {:name java.lang.Integer,
  :declaring-class java.lang.Integer,
  :parameter-types [int],
  :exception-types [],
  :flags #{:public}},
 :class java.lang.Integer,
 :args
 ({:op :literal,
   :env {:locals {}, :ns {:name clojure.core}},
   :val 2,
   :Expr-obj #<NumberExpr clojure.lang.Compiler$NumberExpr@17bf874>}),
 :children
 ({:op :literal,
   :env {:locals {}, :ns {:name clojure.core}},
   :val 2,
   :Expr-obj #<NumberExpr clojure.lang.Compiler$NumberExpr@17bf874>}),
 :Expr-obj #<NewExpr clojure.lang.Compiler$NewExpr@3dbe0>}
```

## Check for incorrectly placed docstrings

```
(defn myfn [a]
  "This is a fn"
  (doseq [a [1 2 3]
    (+ a (inc a))))
```

Surely a beginner mistake, right?

Run the checker in `analyze.examples.docstrings` to find out. :)

## Check for side-effects in transactions

```
(dosync
  (set! *ns* 'myns))
```

Test the checkers by running `analyze.examples.side-effects`
