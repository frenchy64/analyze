# Interface to Clojure's Analyzer

Clojure's analysis compilation phase holds rich information about Clojure forms, like type/reflection information.

_analyze_ provides an interface to this phase, callable a la carte. The output is similar to ClojureScript's analyzer.

# Contributing

Pull requests accepted from registered Clojure contributers

http://clojure.org/contributing

# Usage

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

# Download

https://clojars.org/analyze

Current version: 0.1.4

# Todo

- analyze a leiningen `project.clj` file
- analyze `clojure.core`
- does each expression have a line number attached?
- try evaling the Expr forms returned by the analyzer and see what happens
- Review SOURCE var
- use :locals if necessary

# Examples

See `analyze.examples.*` namespaces.
