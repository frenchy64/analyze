(ns analyze.hygienic
  (:require [analyze.fold :refer [derive-default-fold add-fold-case fold-expr]]
            [analyze.emit-form :refer [map->form derive-emit-default]]
            [analyze.core :refer [ast]]))

(def hsym-key ::hygienic-sym)
(def hname-key ::hygienic-name)

;; emit

(def hygienic-emit ::hygienic-emit)

(derive-emit-default hygienic-emit)

(defmethod map->form [:local-binding hygienic-emit]
  [expr _]
  (hsym-key expr))


(defmethod map->form [:fn-expr hygienic-emit]
  [{:keys [methods] :as expr} mode]
  (list* 'fn* 
         (concat
           (when-let [name (hname-key expr)]
             [name])
           (map #(map->form % mode) methods))))

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
  (assert expr)
  (assert scope)
  (fold-expr ::hygienic
    {:expr-rec #(hygienic-ast % scope)
     :locals {::scope scope}}
    expr))

(defn hygienic-sym [scope sym]
  ;only generate unique when shadowing
  (if (scope sym)
    (gensym sym)
    sym))

(defn hygienic-local-binding [{:keys [op init sym] :as local-binding} scope new-sym?]
  {:pre [(= :local-binding op)]}
  (let [hy-init (when init
                  (hygienic-ast init scope))
        hy-sym (if new-sym?
                 (gensym sym)
                 (scope sym))
        _ (assert hy-sym (str "Local " sym " not in scope."))]
    (assoc local-binding
           :init hy-init
           hsym-key hy-sym)))

;[(IPersistentMap Symbol HSymbol) Symbol HSymbol -> (IPersistentMap Symbol HSymbol)]
(defn add-scope [scope sym hy-sym]
  {:pre [sym hy-sym scope]}
  (assoc scope sym hy-sym))

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
                          hy-sym (hygienic-sym scope sym)
                          hy-binding-init (-> binding-init
                                            (update-in [:init] update-init)
                                            (update-in [:local-binding :init] update-init)
                                            (assoc-in [:local-binding hsym-key] hy-sym))
                          new-scope (add-scope scope sym hy-sym)]
                      [(conj hy-binding-inits hy-binding-init) new-scope]))
                  [[] scope] binding-inits)

          ;with new scope
          hy-body (hygienic-ast body scope)]
      (assoc expr 
             :binding-inits hy-binding-inits 
             :body hy-body))))

;fn-expr
(add-fold-case ::hygienic
  :fn-expr
  (fn [{:keys [name methods] :as expr}
       {{scope ::scope} :locals}]
    (let [[hy-name scope] (let [hy-name (when name
                                          (hygienic-sym scope name))
                                new-scope (if hy-name
                                            (add-scope scope name hy-name)
                                            scope)]
                            [hy-name new-scope])
          hy-methods (map #(hygienic-ast % scope) methods)]
      (assoc expr
             hname-key hy-name
             :methods hy-methods))))

;fn-method
(add-fold-case ::hygienic
  :fn-method
  (fn [{:keys [required-params rest-param body] :as expr}
       {{scope ::scope} :locals}]
    (let [[hy-required-params scope]
          (reduce (fn [[hy-required-params scope] {:keys [sym] :as local-binding}]
                    {:pre [(vector? hy-required-params)]}
                    (let [hy-sym (hygienic-sym scope sym)
                          new-scope (add-scope scope sym hy-sym)
                          hy-local-binding (assoc local-binding
                                                  hsym-key hy-sym)]
                      [(conj hy-required-params hy-local-binding) new-scope]))
                  [[] scope] required-params)

          [hy-rest-param scope]
          (if-let [{:keys [sym]} rest-param]
            ; use new scope
            (let [hy-sym (hygienic-sym scope sym)

                  new-scope (add-scope scope sym hy-sym)
                  hy-local-binding (assoc rest-param
                                          hsym-key hy-sym)]
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
    (hygienic-local-binding expr scope false)))

(add-fold-case ::hygienic
  :catch
  (fn [{:keys [local-binding handler] :as expr}
       {{scope ::scope} :locals}]
    (let [hy-local-binding (hygienic-local-binding local-binding scope true)
          scope (add-scope scope (:sym hy-local-binding) (hsym-key hy-local-binding))
          hy-handler (hygienic-ast handler scope)]
    (assoc expr
           :local-binding hy-local-binding
           :handler hy-handler))))

(add-fold-case ::hygienic
  :deftype*
  (fn [{:keys [fields] :as expr}
       {{scope ::scope} :locals}]
    (let [[hy-field-vals scope
          (reduce (fn [[hy-lbs scope] lb]
                    (let [hy-lb (hygienic-local-binding lb scope true)
                          scope (add-scope scope (:sym hy-lb) (hsym-key hy-lb))]
                      [(conj hy-lbs hy-lb) scope]))
                  [[] scope] (vals fields))
          hy-fields (zipmap (keys fields) hy-field-vals)]

(comment
  (-> (ast (let [a 1 a a b a a a] a)) ast-hy emit-hy)

  (-> (ast (fn a [a a] a)) ast-hy emit-hy)
  (-> (ast (fn [a a & a] a)) ast-hy emit-hy)
  (-> (ast (let [a 1] (fn a [] a))) ast-hy emit-hy)
  (-> (ast (let [a 1] (try a (catch Exception a a)))) ast-hy emit-hy)
  )
