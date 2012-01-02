# Interface to Clojure's Analyzer

Clojure's analysis compilation phase holds rich information about Clojure forms, like type/reflection information.

_analyze_ provides an interface to this phase, callable a la carte. The output is similar to ClojureScript's analyzer.

# Progress

See [http://dev.clojure.org/pages/viewpage.action?pageId=4063367]

# Examples

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

The checker in `analyze.examples.side-effects`
