;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(set! *warn-on-reflection* false)

(ns analyze.core
  (:refer-clojure :exclude [munge macroexpand-1])
  (:import (java.io LineNumberReader InputStreamReader PushbackReader)
           (clojure.lang Compiler$DefExpr Compiler$LocalBinding Compiler$BindingInit Compiler$LetExpr
                         Compiler$LetFnExpr Compiler$StaticMethodExpr Compiler$InstanceMethodExpr Compiler$StaticFieldExpr
                         Compiler$NewExpr Compiler$LiteralExpr Compiler$EmptyExpr Compiler$VectorExpr Compiler$MonitorEnterExpr
                         Compiler$MonitorExitExpr Compiler$ThrowExpr Compiler$InvokeExpr Compiler$TheVarExpr Compiler$VarExpr
                         Compiler$UnresolvedVarExpr Compiler$ObjExpr Compiler$NewInstanceMethod Compiler$FnMethod Compiler$FnExpr
                         Compiler$NewInstanceExpr Compiler$MetaExpr Compiler$BodyExpr Compiler$ImportExpr Compiler$AssignExpr
                         Compiler$TryExpr$CatchClause Compiler$TryExpr Compiler$C))
  (:require [clojure.reflect :as reflect]
            [clojure.java.io :as io]
            [clojure.repl :as repl]))

(defn wall-hack [what class-name member-name & args]
  (letfn [(wall-hack-field [class-name field-name obj]
                           (-> class-name (.getDeclaredField (name field-name))
                             (doto (.setAccessible true)) (.get obj)))
          (wall-hack-method [class-name method-name types obj args]
                            (-> class-name (.getDeclaredMethod (name method-name)
                                                               (into-array Class types))
                              (doto (.setAccessible true))
                              (.invoke obj (into-array Object args))))]
    (condp = what
      :method
      (wall-hack-method class-name member-name (first args) (fnext args) (nnext args))
      :field
      (wall-hack-field class-name member-name (first args)))))

(defmulti Expr->map (fn [& args] (-> args first class)))

;; def

(defmethod Expr->map Compiler$DefExpr
  [^Compiler$DefExpr expr env]
  (letfn [(field [nm expr]
            (let [cobj Compiler$DefExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :def
     :env (assoc env 
                 :source (field 'source expr)
                 :line (field 'line expr))
     :var (field 'var expr)
     :init (Expr->map (field 'init expr))
     :meta (field 'meta expr)
     :init-provided (field 'initProvided expr)
     :is-dynamic (field 'isDynamic expr)
     :Expr-obj expr}))

;; let

(defn LocalBinding->map [^Compiler$LocalBinding lb env]
  {:op :local-binding
   :env env
   :sym (.sym lb)
   :tag (.tag lb)
   :init (when-let [init (.init lb)]
           (Expr->map init env))})

(defn BindingInit->vec [^Compiler$BindingInit bi env]
  [(LocalBinding->map (.binding bi) env)
   (Expr->map (.init bi) env)])

(defmethod Expr->map Compiler$LetExpr
  [^Compiler$LetExpr expr env]
  {:op :let
   :env env
   :bindings (map BindingInit->vec (.bindingInits expr) (repeat env))
   :body (Expr->map (.body expr) env)
   :Expr-obj expr})

;; letfn

(defmethod Expr->map Compiler$LetFnExpr
  [^Compiler$LetFnExpr expr env]
  {:op :letfn
   :env env
   :body (Expr->map (.body expr))
   :binding-inits (map BindingInit->vec (.bindingInits expr))})

;; Methods

(defmethod Expr->map Compiler$StaticMethodExpr
  [^Compiler$StaticMethodExpr expr env]
  (letfn [(field [nm expr]
            (let [cobj Compiler$StaticMethodExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :static-method 
     :env (assoc env
                 :source (field 'source expr)
                 :line (field 'line expr))
     :class (field 'c expr)
     :method-name (field 'methodName expr)
     :method (@#'reflect/method->map (field 'method expr))
     :args (map Expr->map (field 'args expr))
     :tag (field 'tag expr)
     :Expr-obj expr}))

(defmethod Expr->map Compiler$InstanceMethodExpr
  [^Compiler$InstanceMethodExpr expr env]
  (letfn [(field [nm expr]
            (let [cobj Compiler$InstanceMethodExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :instance-method 
     :env (assoc env
                 :source (field 'source expr)
                 :line (field 'line expr))
     :target (field 'target expr)
     :method-name (field 'methodName expr)
     :method (@#'reflect/method->map (field 'method expr))
     :args (map Expr->map (field 'args expr))
     :tag (field 'tag expr)
     :Expr-obj expr}))

;; Fields

(defmethod Expr->map Compiler$StaticFieldExpr
  [^Compiler$StaticFieldExpr expr env]
  (letfn [(field [nm expr]
            (let [cobj Compiler$StaticFieldExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :static-field
     :env (assoc env
                 :line (field 'line expr))
     :class (field 'c expr)
     :field-name (field 'fieldName expr)
     :field (@#'reflect/field->map (field 'field expr))
     :tag (field 'tag expr)
     :Expr-obj expr}))

(defmethod Expr->map Compiler$NewExpr
  [^Compiler$NewExpr expr env]
  (letfn [(field [nm expr]
            (let [cobj Compiler$NewExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :new 
     :env env
     :ctor (@#'reflect/constructor->map (field 'ctor expr))
     :class (field 'c expr)
     :args (map Expr->map (field 'args expr) (repeat env))
     :Expr-obj expr}))

;; Literals

(defmethod Expr->map Compiler$LiteralExpr
  [^Compiler$LiteralExpr expr env]
  (letfn [(method [nm expr typ]
            (let [cobj Compiler$LiteralExpr]
              (wall-hack :method cobj nm typ expr)))]
    {:op :literal
     :env env
     :val (method 'val expr [])
     :Expr-obj expr}))

(defmethod Expr->map Compiler$EmptyExpr
  [^Compiler$EmptyExpr expr env]
  {:op :empty-expr
   :env env
   :coll (.coll expr)
   :Expr-obj expr})

;; vector literal

(defmethod Expr->map Compiler$VectorExpr
  [^Compiler$VectorExpr expr env]
  {:op :vector
   :env env
   :args (map Expr->map (.args expr) (repeat env))
   :Expr-obj expr})

;; Untyped

(defmethod Expr->map Compiler$MonitorEnterExpr
  [^Compiler$MonitorEnterExpr expr env]
  (letfn [(field [nm expr]
            (let [cobj Compiler$MonitorEnterExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :monitor-enter
     :env env
     :target (Expr->map (field 'target expr) env)
     :Expr-obj expr}))

(defmethod Expr->map Compiler$MonitorExitExpr
  [^Compiler$MonitorExitExpr expr env]
  (letfn [(field [nm expr]
            (let [cobj Compiler$MonitorExitExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :monitor-exit
     :env env
     :target (Expr->map (field 'target expr) env)
     :Expr-obj expr}))

(defmethod Expr->map Compiler$ThrowExpr
  [^Compiler$ThrowExpr expr env]
  (letfn [(field [nm expr]
            (let [cobj Compiler$ThrowExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :throw
     :env env
     :target (Expr->map (field 'excExpr expr) env)
     :Expr-obj expr}))

;; Invokes

(defmethod Expr->map Compiler$InvokeExpr
  [^Compiler$InvokeExpr expr env]
  (letfn [(field [nm expr]
            (let [cobj Compiler$InvokeExpr]
              (wall-hack :field cobj nm expr)))]
    (merge
      {:op :invoke
       :env (assoc env
                   :line (field 'line expr)
                   :source (field 'source expr))
       :fexpr (Expr->map (field 'fexpr expr) env)
       :tag (field 'tag expr)
       :args (map Expr->map (field 'args expr) (repeat env))
       :is-protocol (field 'isProtocol expr)
       :is-direct (field 'isDirect expr)
       :site-index (field 'siteIndex expr)
       :protocol-on (field 'protocolOn expr)
       :Expr-obj expr}
      (when-let [m (field 'onMethod expr)]
        {:method (@#'reflect/method->map m)}))))

;; TheVarExpr

(defmethod Expr->map Compiler$TheVarExpr
  [^Compiler$TheVarExpr expr env]
  {:op :the-var
   :env env
   :var (.var expr)
   :Expr-obj expr})

;; VarExpr

(defmethod Expr->map Compiler$VarExpr
  [^Compiler$VarExpr expr env]
  {:op :var
   :env env
   :var (.var expr)
   :tag (.tag expr)
   :Expr-obj expr})

;; UnresolvedVarExpr

(defmethod Expr->map Compiler$UnresolvedVarExpr
  [^Compiler$UnresolvedVarExpr expr env]
  (letfn [(field [nm expr]
            (let [cobj Compiler$UnresolvedVarExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :unresolved-var
     :env env
     :sym (field 'symbol expr)
     :Expr-obj expr}))

;; ObjExprs

(defmethod Expr->map Compiler$ObjExpr
  [^Compiler$ObjExpr expr env]
  {:op :obj-expr
   :env env
   :tag (.tag expr)
   :Expr-obj expr})

;; FnExpr (extends ObjExpr)

(defmulti ObjMethod->map (fn [& args] (-> args first class)))

(defmethod ObjMethod->map Compiler$NewInstanceMethod 
  [^Compiler$NewInstanceMethod obm env]
  {:op :new-instance-method
   :env env
   :body (Expr->map (.body obm) env)
   :ObjMethod-obj obm})

(defmethod ObjMethod->map Compiler$FnMethod 
  [^Compiler$FnMethod obm env]
  {:op :fn-method
   :env env
   :body (Expr->map (.body obm) env)
   :locals (.locals obm)
   :required-params (map LocalBinding->map (.reqParms obm) (repeat env))
   :rest-param (let [rest-param (.restParm obm)]
                 (if rest-param
                   (LocalBinding->map rest-param env)
                   rest-param))
   :ObjMethod-obj obm})

(defmethod Expr->map Compiler$FnExpr
  [^Compiler$FnExpr expr env]
  {:op :fn-expr
   :env env
   :methods (map ObjMethod->map (.methods expr) (repeat env))
   :variadic-method (when-let [variadic-method (.variadicMethod expr)]
                      (ObjMethod->map variadic-method env))
   :tag (.tag expr)
   :Expr-obj expr})

;; NewInstanceExpr

(defmethod Expr->map Compiler$NewInstanceExpr
  [^Compiler$NewInstanceExpr expr env]
  {:op :new-instance-expr
   :env env
   :methods (map ObjMethod->map (.methods expr) (repeat env))
   :mmap (.mmap expr)
   :covariants (.covariants expr)
   :tag (.tag expr)
   :Expr-obj expr})

;; MetaExpr

(defmethod Expr->map Compiler$MetaExpr
  [^Compiler$MetaExpr expr env]
  {:op :meta
   :env env
   :expr (Expr->map (.expr expr) env)
   :meta (Expr->map (.meta expr) env)
   :Expr-obj expr})

;; do

(defmethod Expr->map Compiler$BodyExpr
  [^Compiler$BodyExpr expr env]
  {:op :do
   :env env
   :exprs (map Expr->map (.exprs expr) (repeat env))
   :Expr-obj expr})

;; ImportExpr

(defmethod Expr->map Compiler$ImportExpr
  [^Compiler$ImportExpr expr env]
  {:op :import
   :env env
   :class-str (.c expr)
   :Expr-obj expr})

;; AssignExpr (set!)

(defmethod Expr->map Compiler$AssignExpr
  [^Compiler$AssignExpr expr env]
  {:op :set!
   :env env
   :target (Expr->map (.target expr) env)
   :val (Expr->map (.val expr) env)
   :Expr-obj expr})

;; TryExpr

(defn CatchClause->map [^Compiler$TryExpr$CatchClause ctch env]
  {:op :catch
   :env env
   :class (.c ctch)
   :local-binding (LocalBinding->map (.lb ctch) env)
   :handler (Expr->map (.handler ctch) env)
   :CatchClause-obj ctch})

(defmethod Expr->map Compiler$TryExpr
  [^Compiler$TryExpr expr env]
  {:op :try
   :env env
   :try-expr (Expr->map (.tryExpr expr) env)
   :finally-expr (Expr->map (.finallyExpr expr) env)
   :catch-exprs (map CatchClause->map (.catchExprs expr) (repeat env))
   :ret-local (.retLocal expr)
   :finally-local (.finallyLocal expr)})

(defmethod Expr->map :default
  [expr]
  (println expr)
  (throw (Exception. (str "No method in multimethod 'Expr->map' for dispatch value: " (class expr)))))






(defn wall-hack-method [class-name method-name types obj & args]
  (-> class-name (.getDeclaredMethod (name method-name)
                                     (into-array Class (seq types)))
    (doto (.setAccessible true))
    (.invoke obj (into-array Object args))))

(defn analyze [env form]
  (letfn [(invoke-analyze [context form]
            (wall-hack :method Compiler 'analyze [Compiler$C Object String] Compiler 
              context form nil))]
    (let [context (case (:context env)
                    :statement Compiler$C/STATEMENT
                    :expression Compiler$C/EXPRESSION
                    :return Compiler$C/RETURN
                    :eval Compiler$C/EVAL)
          exprs (binding [*ns* (find-ns (-> env :ns :name))]
                  (try
                    (invoke-analyze context form)
                    (catch RuntimeException e
                      (throw (repl/root-cause e)))))]
      (Expr->map exprs (merge-with conj (dissoc env :context) {:locals {}})))))

(defn forms-seq
  "Seq of forms in a Clojure or ClojureScript file."
  ([f]
     (forms-seq f (java.io.PushbackReader. (io/reader f))))
  ([f ^java.io.PushbackReader rdr]
     (if-let [form (read rdr nil nil)]
       (lazy-seq (cons form (forms-seq f rdr)))
       (.close rdr))))

(comment
(analyze {:ns {:name 'clojure.core} :context :eval} '(try (throw (Exception.)) (catch Exception e (throw e)) (finally 33)))

;; Expecting more output from things like :fn-method
(analyze {:ns {:name 'clojure.core} :context :eval} '(try (println 1 23) (throw (Exception.)) (catch Exception e (throw e)) ))

(analyze {:ns {:name 'clojure.core} :context :eval} '(let [b 1] (fn [& a] 1)))
)
