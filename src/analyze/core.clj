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
           (clojure.lang RT Compiler))
  (:require [clojure.reflect :as reflect]
            [clojure.java.io :as io]
            [clojure.core :as core]
            [clojure.repl :as repl]
            [clojure.pprint :as pprint]
            [clojure.string :as string]))

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

(defmethod Expr->map clojure.lang.Compiler$DefExpr
  [^clojure.lang.Compiler$DefExpr expr env]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$DefExpr]
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

(defn LocalBinding->map [^clojure.lang.Compiler$LocalBinding lb env]
  (letfn [(field ([nm expr] (field nm expr clojure.lang.Compiler$LocalBinding))
                 ([nm expr cobj] (wall-hack :field cobj nm expr)))]
    {:op :local-binding
     :env env
     :sym (field 'sym lb)
     :tag (field 'tag lb)
     :init (when-let [init (field 'init lb)]
             (Expr->map init env))}))

(defn BindingInit->vec [^clojure.lang.Compiler$BindingInit bi env]
  (letfn [(field ([nm expr] (field nm expr clojure.lang.Compiler$BindingInit))
                 ([nm expr cobj] (wall-hack :field cobj nm expr)))]
    [(-> (field 'localBinding bi) (LocalBinding->map env))
     (-> (field 'init bi) (Expr->map env))]))

(defmethod Expr->map clojure.lang.Compiler$LetExpr
  [^clojure.lang.Compiler$LetExpr expr env]
  (letfn [(field 
            ([nm expr] (field nm expr clojure.lang.Compiler$LetExpr))
            ([nm expr cobj] (wall-hack :field cobj nm expr)))]
    {:op :let
     :env (assoc env
                 :source (field 'source expr)
                 :line (field 'line expr))
     :bindings (map BindingInit->vec (field 'bindingInits expr))
     :body (Expr->map (field 'body expr))
     :Expr-obj expr}))

;; letfn

(defmethod Expr->map clojure.lang.Compiler$LetFnExpr
  [^clojure.lang.Compiler$LetFnExpr expr env]
  (letfn [(field 
            ([nm expr] (field nm expr clojure.lang.Compiler$LetFnExpr))
            ([nm expr cobj] (wall-hack :field cobj nm expr)))]
    {:op :letfn
     :env env
     :body (Expr->map (field 'body expr))
     :binding-inits (map BindingInit->vec (field 'bindingInits expr))}))

;; Methods

(defmethod Expr->map clojure.lang.Compiler$StaticMethodExpr
  [^clojure.lang.Compiler$StaticMethodExpr expr env]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$StaticMethodExpr]
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

(defmethod Expr->map clojure.lang.Compiler$InstanceMethodExpr
  [^clojure.lang.Compiler$InstanceMethodExpr expr env]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$InstanceMethodExpr]
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

(defmethod Expr->map clojure.lang.Compiler$StaticFieldExpr
  [^clojure.lang.Compiler$StaticFieldExpr expr env]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$StaticFieldExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :static-field
     :env (assoc env
                 :line (field 'line expr))
     :class (field 'c expr)
     :field-name (field 'fieldName expr)
     :field (@#'reflect/field->map (field 'field expr))
     :tag (field 'tag expr)
     :Expr-obj expr}))

(defmethod Expr->map clojure.lang.Compiler$NewExpr
  [^clojure.lang.Compiler$NewExpr expr env]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$NewExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :new 
     :env env
     :ctor (@#'reflect/constructor->map (field 'ctor expr))
     :class (field 'c expr)
     :args (map Expr->map (field 'args expr) (repeat env))
     :Expr-obj expr}))

;; Literals

(defmethod Expr->map clojure.lang.Compiler$LiteralExpr
  [^clojure.lang.Compiler$LiteralExpr expr env]
  (letfn [(method [nm expr typ]
            (let [cobj clojure.lang.Compiler$LiteralExpr]
              (wall-hack :method cobj nm typ expr)))
          (field [nm expr]
            (let [cobj clojure.lang.Compiler$LiteralExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :literal
     :env env
     :val (method 'val expr [])
     :Expr-obj expr}))

(defmethod Expr->map clojure.lang.Compiler$EmptyExpr
  [^clojure.lang.Compiler$EmptyExpr expr env]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$EmptyExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :empty-expr
     :env env
     :coll (field 'coll expr)
     :Expr-obj expr}))

;; vector literal

(defmethod Expr->map clojure.lang.Compiler$VectorExpr
  [^clojure.lang.Compiler$VectorExpr expr env]
  (letfn [(field 
            ([nm expr] (field nm expr clojure.lang.Compiler$VectorExpr))
            ([nm expr cobj] (wall-hack :field cobj nm expr)))]
    {:op :vector
     :env env
     :args (map Expr->map (field 'args expr) (repeat env))
     :Expr-obj expr}))

;; Untyped

(defmethod Expr->map clojure.lang.Compiler$MonitorEnterExpr
  [^clojure.lang.Compiler$MonitorEnterExpr expr env]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$MonitorEnterExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :monitor-enter
     :env env
     :target (Expr->map (field 'target expr) env)
     :Expr-obj expr}))

(defmethod Expr->map clojure.lang.Compiler$MonitorExitExpr
  [^clojure.lang.Compiler$MonitorExitExpr expr env]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$MonitorExitExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :monitor-exit
     :env env
     :target (Expr->map (field 'target expr) env)
     :Expr-obj expr}))

(defmethod Expr->map clojure.lang.Compiler$ThrowExpr
  [^clojure.lang.Compiler$ThrowExpr expr env]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$ThrowExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :throw
     :env env
     :target (Expr->map (field 'excExpr expr) env)
     :Expr-obj expr}))

;; Invokes

(defmethod Expr->map clojure.lang.Compiler$InvokeExpr
  [^clojure.lang.Compiler$InvokeExpr expr env]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$InvokeExpr]
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

(defmethod Expr->map clojure.lang.Compiler$TheVarExpr
  [^clojure.lang.Compiler$TheVarExpr expr env]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$TheVarExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :the-var
     :env env
     :var (field 'var expr)
     :Expr-obj expr}))

;; VarExpr

(defmethod Expr->map clojure.lang.Compiler$VarExpr
  [^clojure.lang.Compiler$VarExpr expr env]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$VarExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :var
     :env env
     :var (field 'var expr)
     :tag (field 'tag expr)
     :Expr-obj expr}))

;; UnresolvedVarExpr

(defmethod Expr->map clojure.lang.Compiler$UnresolvedVarExpr
  [^clojure.lang.Compiler$UnresolvedVarExpr expr env]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$UnresolvedVarExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :unresolved-var
     :env env
     :sym (field 'symbol expr)
     :Expr-obj expr}))

;; ObjExprs

(defmethod Expr->map clojure.lang.Compiler$ObjExpr
  [^clojure.lang.Compiler$ObjExpr expr env]
  (letfn [(field 
            ([nm expr] (field nm expr clojure.lang.Compiler$ObjExpr))
            ([nm expr cobj] (wall-hack :field cobj nm expr)))]
    {:op :obj-expr
     :env env
     :tag (field 'tag expr)
     :Expr-obj expr}))

;; FnExpr (extends ObjExpr)

(defmulti ObjMethod->map (fn [& args] (-> args first class)))

(defmethod ObjMethod->map clojure.lang.Compiler$NewInstanceMethod 
  [^clojure.lang.Compiler$NewInstanceMethod obm env]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$NewInstanceMethod]
              (wall-hack :field cobj nm expr)))]
    {:op :new-instance-method
     :env env
     :ObjMethod-obj obm}))

(defmethod ObjMethod->map clojure.lang.Compiler$FnMethod 
  [^clojure.lang.Compiler$FnMethod obm env]
  (letfn [(field 
            ([nm expr] (field nm expr clojure.lang.Compiler$FnMethod))
            ([nm expr cobj] (wall-hack :field cobj nm expr)))]
    {:op :fn-method
     :env env
     :objx (field 'objx obm clojure.lang.Compiler$ObjMethod)
     :parent (field 'parent obm clojure.lang.Compiler$ObjMethod)
     :required-params (map LocalBinding->map (field 'reqParms obm) (repeat env))
     :rest-param (let [rest-param (field 'restParm obm)]
                   (if rest-param
                     (LocalBinding->map rest-param env)
                     rest-param))
     :ObjMethod-obj obm}))

(defmethod Expr->map clojure.lang.Compiler$FnExpr
  [^clojure.lang.Compiler$FnExpr expr env]
  (letfn [(field 
            ([nm expr] (field nm expr clojure.lang.Compiler$FnExpr))
            ([nm expr cobj] (wall-hack :field cobj nm expr)))]
    {:op :fn-expr
     :env env
     :methods (map ObjMethod->map (field 'methods expr) (repeat env))
     :tag (field 'tag expr clojure.lang.Compiler$ObjExpr)
     :Expr-obj expr}))

(defmethod Expr->map clojure.lang.Compiler$NewInstanceExpr
  [^clojure.lang.Compiler$NewInstanceExpr expr env]
  (letfn [(field 
            ([nm expr] (field nm expr clojure.lang.Compiler$NewInstanceExpr))
            ([nm expr cobj] (wall-hack :field cobj nm expr)))]
    {:op :new-instance-expr
     :env env
     :methods (map ObjMethod->map (field 'methods expr) (repeat env))
     :mmap (field 'mmap expr)
     :covariants (field 'covariants expr)
     :tag (field 'tag expr clojure.lang.Compiler$ObjExpr)
     :Expr-obj expr}))

;; MetaExpr

(defmethod Expr->map clojure.lang.Compiler$MetaExpr
  [^clojure.lang.Compiler$MetaExpr expr env]
  (letfn [(field 
            ([nm expr] (field nm expr clojure.lang.Compiler$MetaExpr))
            ([nm expr cobj] (wall-hack :field cobj nm expr)))]
    {:op :meta
     :env env
     :expr (Expr->map (field 'expr expr) env)
     :meta (Expr->map (field 'meta expr) env)
     :Expr-obj expr}))

;; do

(defmethod Expr->map clojure.lang.Compiler$BodyExpr
  [^clojure.lang.Compiler$BodyExpr expr env]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$BodyExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :do
     :env env
     :exprs (map Expr->map (field 'exprs expr) (repeat env))
     :Expr-obj expr}))

;; ImportExpr

(defmethod Expr->map clojure.lang.Compiler$ImportExpr
  [^clojure.lang.Compiler$ImportExpr expr env]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$ImportExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :import
     :env env
     :class-str (field 'c expr)
     :Expr-obj expr}))

;; AssignExpr (set!)

(defmethod Expr->map clojure.lang.Compiler$AssignExpr
  [^clojure.lang.Compiler$AssignExpr expr env]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$AssignExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :set!
     :env env
     :target (Expr->map (field 'target expr) env)
     :val (Expr->map (field 'val expr) env)
     :Expr-obj expr}))

;; TryExpr

(defn CatchClause->map [^clojure.lang.Compiler$TryExpr$CatchClause ctch env]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$TryExpr$CatchClause]
              (wall-hack :field cobj nm expr)))]
    {:op :catch
     :env env
     :class (field 'class ctch)
     :local-binding (LocalBinding->map (field 'localBinding ctch) env)
     :handler (Expr->map (field 'handler ctch) env)
     :CatchClause-obj ctch}))

(defmethod Expr->map clojure.lang.Compiler$TryExpr
  [^clojure.lang.Compiler$TryExpr expr env]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$TryExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :try
     :env env
     :try-expr (Expr->map (field 'tryExpr expr) env)
     :finally-expr (Expr->map (field 'finallyExpr expr) env)
     :catch-exprs (map CatchClause->map (field 'catchExprs expr) (repeat env))
     :ret-local (field 'retLocal expr)
     :finally-local (field 'finally-local)}))

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
            (wall-hack :method clojure.lang.Compiler 'analyze [clojure.lang.Compiler$C Object String] clojure.lang.Compiler 
              context form nil))]
    (let [context (case (:context env)
                    :statement clojure.lang.Compiler$C/STATEMENT
                    :expression clojure.lang.Compiler$C/EXPRESSION
                    :return clojure.lang.Compiler$C/RETURN
                    :eval clojure.lang.Compiler$C/EVAL)
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
)
