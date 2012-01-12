# Interface to Clojure's Analyzer

Clojure's analysis compilation phase holds rich information about Clojure forms, like type/reflection information.

_analyze_ provides an interface to this phase, callable a la carte. The output is similar to ClojureScript's analyzer.

# Contributing

Pull requests accepted from registered Clojure contributers

http://clojure.org/contributing

# Download

Current version: 0.1.2

https://clojars.org/analyze

# Progress

See [http://dev.clojure.org/pages/viewpage.action?pageId=4063367]

# Todo

- work out how to analyze a leiningen `project.clj` file
- analyze `clojure.core`
- get rid of requirement to provide namespace
- does each expression have a line number attached?
- try evaling the Expr forms returned by the analyzer and see what happens
- Review SOURCE var
- use :locals if necessary

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
