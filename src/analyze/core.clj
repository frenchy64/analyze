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

(defmulti Expr->map class)

(defmethod Expr->map clojure.lang.Compiler$DefExpr
  [^clojure.lang.Compiler$DefExpr expr]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$DefExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :def
     :env {:source (field 'source expr)
           :line (field 'line expr)}
     :var (field 'var expr)
     :init (Expr->map (field 'init expr))
     :meta (field 'meta expr)
     :init-provided (field 'initProvided expr)
     :is-dynamic (field 'isDynamic expr)
     :Expr-obj expr}))

(defn LocalBinding->map [^clojure.lang.Compiler$LocalBinding lb]
  (letfn [(field ([nm expr] (field nm expr clojure.lang.Compiler$LocalBinding))
                 ([nm expr cobj] (wall-hack :field cobj nm expr)))]
    {:op :local-binding
     :sym (field 'sym lb)
     :tag (field 'tag lb)
     :init (Expr->map (field 'init lb))}))

(defn BindingInit->vec [^clojure.lang.Compiler$BindingInit bi]
  (letfn [(field ([nm expr] (field nm expr clojure.lang.Compiler$BindingInit))
                 ([nm expr cobj] (wall-hack :field cobj nm expr)))]
    [(-> (field 'localBinding bi) LocalBinding->map)
     (-> (field 'init bi) Expr->map)]))

(defmethod Expr->map clojure.lang.Compiler$LetExpr
  [^clojure.lang.Compiler$LetExpr expr]
  (letfn [(field 
            ([nm expr] (field nm expr clojure.lang.Compiler$LetExpr))
            ([nm expr cobj] (wall-hack :field cobj nm expr)))]
    {:op :let
     :env {:source (field 'source expr)
           :line (field 'line expr)}
     :bindings (map BindingInit->vec (field 'bindingInits expr))
     :body (Expr->map (field 'body expr))
     :Expr-obj expr}))

;; Methods

(defmethod Expr->map clojure.lang.Compiler$StaticMethodExpr
  [^clojure.lang.Compiler$StaticMethodExpr expr]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$StaticMethodExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :static-method 
     :env {:source (field 'source expr)
           :line (field 'line expr)}
     :class (field 'c expr)
     :method-name (field 'methodName expr)
     :method (@#'reflect/method->map (field 'method expr))
     :args (map Expr->map (field 'args expr))
     :tag (field 'tag expr)
     :Expr-obj expr}))

(defmethod Expr->map clojure.lang.Compiler$InstanceMethodExpr
  [^clojure.lang.Compiler$InstanceMethodExpr expr]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$InstanceMethodExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :instance-method 
     :env {:source (field 'source expr)
           :line (field 'line expr)}
     :target (field 'target expr)
     :method-name (field 'methodName expr)
     :method (@#'reflect/method->map (field 'method expr))
     :args (map Expr->map (field 'args expr))
     :tag (field 'tag expr)
     :Expr-obj expr}))

;; Fields

(defmethod Expr->map clojure.lang.Compiler$StaticFieldExpr
  [^clojure.lang.Compiler$StaticFieldExpr expr]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$StaticFieldExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :static-field
     :env {:line (field 'line expr)}
     :class (field 'c expr)
     :field-name (field 'fieldName expr)
     :field (@#'reflect/field->map (field 'field expr))
     :tag (field 'tag expr)
     :Expr-obj expr}))

(defmethod Expr->map clojure.lang.Compiler$NewExpr
  [^clojure.lang.Compiler$NewExpr expr]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$NewExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :new 
     :ctor (@#'reflect/constructor->map (field 'ctor expr))
     :class (field 'c expr)
     :args (map Expr->map (field 'args expr))
     :Expr-obj expr}))

;; Literals

(defmethod Expr->map clojure.lang.Compiler$LiteralExpr
  [^clojure.lang.Compiler$LiteralExpr expr]
  (letfn [(method [nm expr typ]
            (let [cobj clojure.lang.Compiler$LiteralExpr]
              (wall-hack :method cobj nm typ expr)))
          (field [nm expr]
            (let [cobj clojure.lang.Compiler$LiteralExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :literal
     :val (method 'val expr [])
     :Expr-obj expr}))

(defmethod Expr->map clojure.lang.Compiler$EmptyExpr
  [^clojure.lang.Compiler$EmptyExpr expr]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$EmptyExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :empty-expr
     :coll (field 'coll expr)
     :Expr-obj expr}))

;; Untyped

(defmethod Expr->map clojure.lang.Compiler$MonitorEnterExpr
  [^clojure.lang.Compiler$MonitorEnterExpr expr]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$MonitorEnterExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :monitor-enter
     :target (Expr->map (field 'target expr))
     :Expr-obj expr}))

(defmethod Expr->map clojure.lang.Compiler$MonitorExitExpr
  [^clojure.lang.Compiler$MonitorExitExpr expr]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$MonitorExitExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :monitor-exit
     :target (Expr->map (field 'target expr))
     :Expr-obj expr}))

(defmethod Expr->map clojure.lang.Compiler$ThrowExpr
  [^clojure.lang.Compiler$ThrowExpr expr]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$ThrowExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :throw
     :target (Expr->map (field 'excExpr expr))
     :Expr-obj expr}))

;; Invokes

(defmethod Expr->map clojure.lang.Compiler$InvokeExpr
  [^clojure.lang.Compiler$InvokeExpr expr]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$InvokeExpr]
              (wall-hack :field cobj nm expr)))]
    (merge
      {:op :invoke
       :env {:line (field 'line expr)
             :source (field 'source expr)}
       :fexpr (Expr->map (field 'fexpr expr))
       :tag (field 'tag expr)
       :args (map Expr->map (field 'args expr))
       :is-protocol (field 'isProtocol expr)
       :is-direct (field 'isDirect expr)
       :site-index (field 'siteIndex expr)
       :protocol-on (field 'protocolOn expr)
       :Expr-obj expr}
      (when-let [m (field 'onMethod expr)]
        {:method (@#'reflect/method->map m)}))))

;; TheVarExpr

(defmethod Expr->map clojure.lang.Compiler$TheVarExpr
  [^clojure.lang.Compiler$TheVarExpr expr]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$TheVarExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :the-var
     :var (field 'var expr)
     :Expr-obj expr}))

;; VarExpr

(defmethod Expr->map clojure.lang.Compiler$VarExpr
  [^clojure.lang.Compiler$VarExpr expr]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$VarExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :var
     :var (field 'var expr)
     :tag (field 'tag expr)
     :Expr-obj expr}))

;; UnresolvedVarExpr

(defmethod Expr->map clojure.lang.Compiler$UnresolvedVarExpr
  [^clojure.lang.Compiler$UnresolvedVarExpr expr]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$UnresolvedVarExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :unresolved-var
     :sym (field 'symbol expr)
     :Expr-obj expr}))

;; ObjExprs

(defmethod Expr->map clojure.lang.Compiler$ObjExpr
  [^clojure.lang.Compiler$ObjExpr expr]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$ObjExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :obj-expr
     :tag (field 'tag expr)
     :Expr-obj expr}))

;; FnExpr (extends ObjExpr)

(defmulti ObjMethod->map class)

(defmethod ObjMethod->map clojure.lang.Compiler$NewInstanceMethod 
  [^clojure.lang.Compiler$NewInstanceMethod obm]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$NewInstanceMethod]
              (wall-hack :field cobj nm expr)))]
    {:op :new-instance-method
     :ObjMethod-obj obm}))

(defmethod ObjMethod->map clojure.lang.Compiler$FnMethod 
  [^clojure.lang.Compiler$FnMethod obm]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$FnMethod]
              (wall-hack :field cobj nm expr)))]
    {:op :fn-method
     :ObjMethod-obj obm}))

(defmethod Expr->map clojure.lang.Compiler$FnExpr
  [^clojure.lang.Compiler$FnExpr expr]
  (letfn [(field 
            ([nm expr] (field nm expr clojure.lang.Compiler$FnExpr))
            ([nm expr cobj] (wall-hack :field cobj nm expr)))]
    {:op :obj-expr
     :methods (map ObjMethod->map (field 'methods expr))
     :tag (field 'tag expr clojure.lang.Compiler$ObjExpr)
     :Expr-obj expr}))


;; do

(defmethod Expr->map clojure.lang.Compiler$BodyExpr
  [^clojure.lang.Compiler$BodyExpr expr]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$BodyExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :do
     :exprs (map Expr->map (field 'exprs expr))
     :Expr-obj expr}))

;; ImportExpr

(defmethod Expr->map clojure.lang.Compiler$ImportExpr
  [^clojure.lang.Compiler$ImportExpr expr]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$ImportExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :import
     :class-str (field 'c expr)
     :Expr-obj expr}))

;; TryExpr

(defn CatchClause->map [^clojure.lang.Compiler$TryExpr$CatchClause ctch]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$TryExpr$CatchClause]
              (wall-hack :field cobj nm expr)))]
    {:op :catch
     :class (field 'class ctch)
     :local-binding (LocalBinding->map (field 'localBinding ctch))
     :handler (Expr->map (field 'handler ctch))
     :CatchClause-obj ctch}))

(defmethod Expr->map clojure.lang.Compiler$TryExpr
  [^clojure.lang.Compiler$TryExpr expr]
  (letfn [(field [nm expr]
            (let [cobj clojure.lang.Compiler$TryExpr]
              (wall-hack :field cobj nm expr)))]
    {:op :try
     :try-expr (Expr->map (field 'tryExpr expr))
     :finally-expr (Expr->map (field 'finallyExpr expr))
     :catch-exprs (map CatchClause->map (field 'catchExprs expr))
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
      (Expr->map exprs))))

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
