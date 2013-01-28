Clojars Dependency: `[analyze "0.2.5"]`

# Interface to Clojure's Analyzer

Clojure's analysis compilation phase holds rich information about Clojure forms, like type/reflection information.

_analyze_ provides an interface to this phase, callable a la carte. The output is similar to ClojureScript's analyzer.

Supports Clojure 1.4.0 or later.

# Contributing

Pull requests accepted from registered Clojure contributers

http://clojure.org/contributing

# Changelog

0.2.6-SNAPSHOT
- More macroexpansion cases

0.2.5
- More cases for `map->form`
- Fix :fn-expr case for `map->form`, now emits `fn*` instead of `fn`

0.2.4
- More cases for `map->form`

0.2.3
- Add `analyze.emit-form/map->form`
- Support Clojure 1.4.0+

0.2.2
- Revert to not `eval`ing forms before analysing

0.2.1
- `eval` forms before analysing them

# Usage

## Generating AST from syntax

```clojure

analyze.core=> (ast [1])
{:op :constant, :env {:locals {}, :ns {:name analyze.core}}, :val [1]}

analyze.core=> (-> (ast (if true 1 2)) clojure.pprint/pprint)
{:op :if,
 :env {:line 5, :locals {}, :ns {:name analyze.core}},
 :test
 {:op :boolean,
  :env {:locals {}, :ns {:name analyze.core}},
  :val true},
 :then
 {:op :number, :env {:locals {}, :ns {:name analyze.core}}, :val 1},
 :else
 {:op :number, :env {:locals {}, :ns {:name analyze.core}}, :val 2}}
nil

analyze.core=> (-> (ast (fn [x] (+ x 1))) clojure.pprint/pprint)
{:op :fn-expr,
 :env {:locals {}, :ns {:name analyze.core}},
 :methods
 ({:op :fn-method,
   :env {:locals {}, :ns {:name analyze.core}},
   :body
   {:op :do,
    :env {:locals {}, :ns {:name analyze.core}},
    :exprs
    ({:op :static-method,
      :env
      {:line 6, :source "REPL", :locals {}, :ns {:name analyze.core}},
      :class clojure.lang.Numbers,
      :method-name "add",
      :method
      {:name add,
       :return-type java.lang.Number,
       :declaring-class clojure.lang.Numbers,
       :parameter-types [java.lang.Object long],
       :exception-types [],
       :flags #{:static :public}},
      :args
      ({:op :local-binding-expr,
        :env {:locals {}, :ns {:name analyze.core}},
        :local-binding
        {:op :local-binding,
         :env {:locals {}, :ns {:name analyze.core}},
         :sym x,
         :tag nil,
         :init nil},
        :tag nil}
       {:op :number,
        :env {:locals {}, :ns {:name analyze.core}},
        :val 1}),
      :tag nil})},
   :required-params
   ({:op :local-binding,
     :env {:locals {}, :ns {:name analyze.core}},
     :sym x,
     :tag nil,
     :init nil}),
   :rest-param nil}),
 :variadic-method nil,
 :tag nil}
nil
```

## Syntax from AST


```clojure
analyze.core=> (require '[analyze.emit-form :as e])
nil
analyze.core=> (-> (ast 1) e/map->form)
1
analyze.core=> (-> (ast [(+ 1 2)]) e/map->form)
[(. clojure.lang.Numbers add 1 2)]
```

# Known Issues

## Incorrect handling of Var mappings within the same form

`analyze` is a thin wrapper over `clojure.lang.Compiler`, so to get our
hands on analysis results some compromises are made.

The following form normally evaluates to the Var `clojure.set/intersection`, but
analyses to `clojure.core/require`.


```clojure
;normal evaluation
(eval
 '(do 
    (require '[clojure.set])
    (refer 'clojure.set 
           :only '[intersection] 
           :rename '{intersection require})
    require))
;=> #'clojure.set/intersection

;analysis result
(-> (ast 
      (do (require '[clojure.set])
        (refer 'clojure.set 
               :only '[intersection] 
               :rename '{intersection require})
        require))
  :exprs last :var)
;=> #'clojure.core/require
```

# Todo

- analyze a leiningen `project.clj` file
- analyze `clojure.core`
- use :locals if necessary

# Examples

See `analyze.examples.*` namespaces.

# Contributors

- Jonas Enlund (jonase)
- Nicola Mometto (Bronsa)
- Chris Gray (chrismgray)
