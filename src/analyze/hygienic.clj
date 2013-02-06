(ns analyze.hygienic
  (:require [analyze.fold :refer [derive-default-fold add-fold-case fold-expr]]
            [analyze.emit-form :refer [map->form derive-emit-default]]
            [analyze.core :refer [ast]]))

;; emit

(def hygienic-emit ::hygienic-emit)

(derive-emit-default hygienic-emit)

(defmethod map->form [:local-binding hygienic-emit]
  [expr _]
  (::hygienic-sym expr))

(defn emit-hy 
  "Emit an already-hygienic AST as a form"
  [expr]
  (map->form expr hygienic-emit))

;; fold

(derive-default-fold ::hygienic)

(declare hygienic-ast)

(defn ast-hy [expr]
  (hygienic-ast expr {}))

(defn hygienic-ast [expr scope]
  (assert scope)
  (fold-expr ::hygienic
    {:expr-rec #(hygienic-ast % scope)
     :locals {::scope scope}}
    expr))

;let
(add-fold-case ::hygienic
  :let
  (fn [{:keys [binding-inits body] :as expr}
       {{scope ::scope} :locals :as locals}]
    (assert scope locals)
    (let [[hy-binding-inits scope]
          (reduce (fn [[hy-binding-inits scope] binding-init]
                    {:pre [(vector? hy-binding-inits)]}
                    (let [sym (-> binding-init :local-binding :sym)
                          update-init #(when % (hygienic-ast % scope))
                          ;only generate unique when shadowing
                          hy-sym (if (scope sym)
                                   (gensym sym)
                                   sym)
                          hy-binding-init (-> binding-init
                                            (update-in [:init] update-init)
                                            (update-in [:local-binding :init] update-init)
                                            (assoc-in [:local-binding ::hygienic-sym] hy-sym))
                          new-scope (assoc scope sym hy-sym)]
                      [(conj hy-binding-inits hy-binding-init) new-scope]))
                  [[] scope] binding-inits)

          ;with new scope
          hy-body (hygienic-ast body scope)]
      (assoc expr 
             :binding-inits hy-binding-inits 
             :body hy-body))))

;fn-method
(add-fold-case ::hygienic
  :fn-method
  (fn [{:keys [required-params rest-param body] :as expr}
       {{scope ::scope} :locals}]
    (let [[hy-required-params scope]
          (reduce (fn [[hy-required-params scope] {:keys [sym] :as local-binding}]
                    {:pre [(vector? hy-required-params)]}
                    (let [hy-sym (if (scope sym)
                                   (gensym sym)
                                   sym)
                          new-scope (assoc scope sym hy-sym)
                          hy-local-binding (assoc local-binding
                                                  ::hygienic-sym hy-sym)]
                      [(conj hy-required-params hy-local-binding) new-scope]))
                  [[] scope] required-params)

          [hy-rest-param scope]
          (if-let [{:keys [sym]} rest-param]
            ; use new scope
            (let [hy-sym (if (scope sym)
                           (gensym sym)
                           sym)

                  new-scope (-> scope
                              (assoc sym hy-sym))
                  hy-local-binding (assoc rest-param
                                          ::hygienic-sym hy-sym)]
              [hy-local-binding new-scope])
            [rest-param scope])

          ; use new scope
          hy-body (hygienic-ast body scope)]
      (assoc expr
             :required-params hy-required-params
             :rest-param hy-rest-param
             :body hy-body))))

;local-binding-expr
(add-fold-case ::hygienic
  :local-binding
  (fn [{:keys [sym init] :as expr}
       {{scope ::scope} :locals}]
    (let [hy-init (when init
                    (hygienic-ast init scope))
          hy-sym (scope sym)
          _ (assert hy-sym (str "Local " sym " not in scope."))]
      (assoc expr
             :init hy-init
             ::hygienic-sym hy-sym))))

(comment
  (-> (ast (let [a 1 a a b a a a] a)) ast-hy emit-hy)

  (-> (ast (fn [a a] a)) ast-hy emit-hy)
  (-> (ast (fn [a a & a] a)) ast-hy emit-hy)
  )
