(set! *warn-on-reflection* false)

(ns analyze.core
  "Interface to Compiler's analyze.
  Entry point `analyze-path` and `analyze-one`"
  (:import (java.io LineNumberReader InputStreamReader PushbackReader)
           (clojure.lang RT LineNumberingPushbackReader Compiler$DefExpr Compiler$LocalBinding Compiler$BindingInit Compiler$LetExpr
                         Compiler$LetFnExpr Compiler$StaticMethodExpr Compiler$InstanceMethodExpr Compiler$StaticFieldExpr
                         Compiler$NewExpr Compiler$EmptyExpr Compiler$VectorExpr Compiler$MonitorEnterExpr
                         Compiler$MonitorExitExpr Compiler$ThrowExpr Compiler$InvokeExpr Compiler$TheVarExpr Compiler$VarExpr
                         Compiler$UnresolvedVarExpr Compiler$ObjExpr Compiler$NewInstanceMethod Compiler$FnMethod Compiler$FnExpr
                         Compiler$NewInstanceExpr Compiler$MetaExpr Compiler$BodyExpr Compiler$ImportExpr Compiler$AssignExpr
                         Compiler$TryExpr$CatchClause Compiler$TryExpr Compiler$C Compiler$LocalBindingExpr Compiler$RecurExpr
                         Compiler$MapExpr Compiler$IfExpr Compiler$KeywordInvokeExpr Compiler$InstanceFieldExpr Compiler$InstanceOfExpr
                         Compiler$CaseExpr Compiler$Expr Compiler$SetExpr Compiler$MethodParamExpr Compiler$KeywordExpr
                         Compiler$ConstantExpr Compiler$NumberExpr Compiler$NilExpr Compiler$BooleanExpr Compiler$StringExpr))
  (:require [clojure.reflect :as reflect]
            [clojure.java.io :as io]
            [clojure.repl :as repl]
            [clojure.string :as string]
            [analyze.util :as util]))

(def CHILDREN (atom false))
(def JAVA-OBJ (atom false))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface

(declare analyze-one)

(defn analyze-form-in-ns [nsym form]
  (analyze-one {:ns {:name nsym} :context :eval}
               form))

(defn analyze-form [form]
  (analyze-form-in-ns (ns-name *ns*) form))

(defmacro ast-in-ns
  "Returns the abstract syntax tree representation of the given form,
  evaluated in the given namespace"
  [nsym form]
  `(analyze-form-in-ns '~nsym '~form))

(defmacro ast 
  "Returns the abstract syntax tree representation of the given form,
  evaluated in the current namespace"
  [form]
  `(analyze-form '~form))

;;;;;;;;;;;;;;;;;;;;;;;
;; Utils

(defn- inherit-env [expr env]
  (merge env
         (when-let [line (-> expr :env :line)]
           {:line line})
         (when-let [source (-> expr :env :source)]
           {:source source})))

(defn- field-accessor [class-obj field obj]
  (let [field (.getDeclaredField class-obj (name field))]
    (.setAccessible field true)
    (let [ret (.get field obj)]
      (if (instance? Boolean ret)
        (boolean ret)
        ret))))

(defn- method-accessor [class-obj method obj types & args]
  (let [method (.getDeclaredMethod class-obj (name method) (into-array Class types))]
    (.setAccessible method true)
    (.invoke method obj (object-array args))))

(defprotocol AnalysisToMap
  (analysis->map [aobj env]
    "Recursively converts the output of the Compiler's analysis to a map"))

;; Literals extending abstract class Compiler$LiteralExpr

(defmacro literal-dispatch [disp-class op-keyword]
  `(extend-protocol AnalysisToMap
     ~disp-class
     (~'analysis->map
       [expr# env#]
       (let [method# (partial method-accessor ~disp-class)]
         (merge
           {:op ~op-keyword
            :env env#
            :val (method# '~'val expr# [])}
           (when @JAVA-OBJ
             {:Expr-obj expr#}))))))

(literal-dispatch Compiler$KeywordExpr :keyword)
(literal-dispatch Compiler$ConstantExpr :constant)
(literal-dispatch Compiler$NumberExpr :number)
(literal-dispatch Compiler$NilExpr :nil)
(literal-dispatch Compiler$StringExpr :string)
(literal-dispatch Compiler$BooleanExpr :boolean)

(extend-protocol AnalysisToMap

  ;; def
  Compiler$DefExpr
  (analysis->map
    [expr env]
    (let [field (partial field-accessor Compiler$DefExpr)
          init (analysis->map (field 'init expr) env)
          meta (when-let [meta (field 'meta expr)]
                 (analysis->map meta env))]
      (merge 
        {:op :def
         :env (assoc env
                     :source (field 'source expr)
                     :line (field 'line expr))
         :var (field 'var expr)
         :meta meta
         :init init
         :init-provided (field 'initProvided expr)
         :is-dynamic (field 'isDynamic expr)}
        (when @CHILDREN
          {:children [meta init]})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  ;; let
  Compiler$LocalBinding
  (analysis->map
    [lb env]
    (let [init (when-let [init (.init lb)]
                 (analysis->map init env))]
      (merge
        {:op :local-binding
         :env (inherit-env init env)
         :sym (.sym lb)
         :tag (.tag lb)
         :init init}
        (when @CHILDREN
          {:children (when init [init])})
        (when @JAVA-OBJ
          {:LocalBinding-obj lb}))))

  Compiler$BindingInit
  (analysis->map
    [bi env]
    (let [local-binding (analysis->map (.binding bi) env)
          init (analysis->map (.init bi) env)]
      (merge
        {:op :binding-init
         :env (inherit-env init env)
         :local-binding local-binding
         :init init}
        (when @CHILDREN
          {:children [local-binding init]})
        (when @JAVA-OBJ
          {:BindingInit-obj bi}))))

  Compiler$LetExpr
  (analysis->map
    [expr env]
    (let [body (analysis->map (.body expr) env)
          binding-inits (doall (map analysis->map (.bindingInits expr) (repeat env)))]
      (merge
        {:op :let
         :env (inherit-env body env)
         :binding-inits binding-inits
         :body body
         :is-loop (.isLoop expr)}
        (when @CHILDREN
          {:children (conj (vec binding-inits) body)})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  ;; letfn
  Compiler$LetFnExpr
  (analysis->map
    [expr env]
    (let [body (analysis->map (.body expr) env)
          binding-inits (doall (map analysis->map (.bindingInits expr) (repeat env)))]
      (merge
        {:op :letfn
         :env (inherit-env body env)
         :body body
         :binding-inits binding-inits}
        (when @CHILDREN
          {:children (conj (vec binding-inits) body)})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  ;; LocalBindingExpr
  Compiler$LocalBindingExpr
  (analysis->map
    [expr env]
    (let [local-binding (analysis->map (.b expr) env)]
      (merge
        {:op :local-binding-expr
         :env (inherit-env local-binding env)
         :local-binding local-binding
         :tag (.tag expr)}
        (when @CHILDREN
          {:children [local-binding]})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  ;; Methods
  Compiler$StaticMethodExpr
  (analysis->map
    [expr env]
    (let [field (partial field-accessor Compiler$StaticMethodExpr)
          args (doall (map analysis->map (field 'args expr) (repeat env)))]
      (merge
        {:op :static-method
         :env (assoc env
                     :source (field 'source expr)
                     :line (field 'line expr))
         :class (field 'c expr)
         :method-name (field 'methodName expr)
         :method (when-let [method (field 'method expr)]
                   (@#'reflect/method->map method))
         :args args
         :tag (field 'tag expr)}
        (when @CHILDREN
          {:children args})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  Compiler$InstanceMethodExpr
  (analysis->map
    [expr env]
    (let [field (partial field-accessor Compiler$InstanceMethodExpr)
          target (analysis->map (field 'target expr) env)
          args (doall (map analysis->map (field 'args expr) (repeat env)))]
      (merge
        {:op :instance-method
         :env (assoc env
                     :source (field 'source expr)
                     :line (field 'line expr))
         :target target
         :method-name (field 'methodName expr)
         :method (when-let [method (field 'method expr)]
                   (@#'reflect/method->map method))
         :args args
         :tag (field 'tag expr)}
        (when @CHILDREN
          {:children (cons target args)})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  ;; Fields
  Compiler$StaticFieldExpr
  (analysis->map
    [expr env]
    (let [field (partial field-accessor Compiler$StaticFieldExpr)]
      (merge
        {:op :static-field
         :env (assoc env
                     :line (field 'line expr))
         :class (field 'c expr)
         :field-name (field 'fieldName expr)
         :field (when-let [field (field 'field expr)]
                  (@#'reflect/field->map field))
         :tag (field 'tag expr)}
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  Compiler$InstanceFieldExpr
  (analysis->map
    [expr env]
    (let [field (partial field-accessor Compiler$InstanceFieldExpr)
          target (analysis->map (field 'target expr) env)]
      (merge
        {:op :instance-field
         :env (assoc env
                     :line (field 'line expr))
         :target target
         :target-class (field 'targetClass expr)
         :field (when-let [field (field 'field expr)]
                  (@#'reflect/field->map field))
         :field-name (field 'fieldName expr)
         :tag (field 'tag expr)}
        (when @CHILDREN
          {:children [target]})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  Compiler$NewExpr
  (analysis->map
    [expr env]
    (let [args (doall (map analysis->map (.args expr) (repeat env)))]
      (merge
        {:op :new
         :env env
         :ctor (when-let [ctor (.ctor expr)]
                 (@#'reflect/constructor->map ctor))
         :class (.c expr)
         :args args}
        (when @CHILDREN
          {:children args})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  Compiler$EmptyExpr
  (analysis->map
    [expr env]
    (merge
      {:op :empty-expr
       :env env
       :coll (.coll expr)}
      (when @JAVA-OBJ
        {:Expr-obj expr})))

  ;; set literal
  Compiler$SetExpr
  (analysis->map
    [expr env]
    (let [keys (doall (map analysis->map (.keys expr) (repeat env)))]
      (merge
        {:op :set
         :env env
         :keys keys}
        (when @CHILDREN
          {:children keys})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  ;; vector literal
  Compiler$VectorExpr
  (analysis->map
    [expr env]
    (let [args (doall (map analysis->map (.args expr) (repeat env)))]
      (merge
        {:op :vector
         :env env
         :args args}
        (when @CHILDREN
          {:children args})
        (when @JAVA-OBJ 
          {:Expr-obj expr}))))

  ;; map literal
  Compiler$MapExpr
  (analysis->map
    [expr env]
    (let [keyvals (doall (map analysis->map (.keyvals expr) (repeat env)))]
      (merge
        {:op :map
         :env env
         :keyvals keyvals}
        (when @CHILDREN
          {:children keyvals})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  ;; Untyped
  Compiler$MonitorEnterExpr
  (analysis->map
    [expr env]
    (let [field (partial field-accessor Compiler$MonitorEnterExpr)
          target (analysis->map (field 'target expr) env)]
      (merge
        {:op :monitor-enter
         :env env
         :target target}
        (when @CHILDREN
          {:children [target]})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  Compiler$MonitorExitExpr
  (analysis->map
    [expr env]
    (let [field (partial field-accessor Compiler$MonitorExitExpr)
          target (analysis->map (field 'target expr) env)]
      (merge
        {:op :monitor-exit
         :env env
         :target target}
        (when @CHILDREN
          {:children [target]})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  Compiler$ThrowExpr
  (analysis->map
    [expr env]
    (let [field (partial field-accessor Compiler$ThrowExpr)
          exception (analysis->map (field 'excExpr expr) env)]
      (merge
        {:op :throw
         :env env
         :exception exception}
        (when @CHILDREN
          {:children [exception]})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  ;; Invokes
  Compiler$InvokeExpr
  (analysis->map
    [expr env]
    (let [field (partial field-accessor Compiler$InvokeExpr)
          fexpr (analysis->map (field 'fexpr expr) env)
          args (doall (map analysis->map (field 'args expr) (repeat env)))]
      (merge
        {:op :invoke
         :env (assoc env
                     :line (field 'line expr)
                     :source (field 'source expr))
         :fexpr fexpr
         :tag (field 'tag expr)
         :args args
         :is-protocol (field 'isProtocol expr)
         :is-direct (field 'isDirect expr)
         :site-index (field 'siteIndex expr)
         :protocol-on (field 'protocolOn expr)}
        (when-let [m (field 'onMethod expr)]
          {:method (@#'reflect/method->map m)})
        (when @CHILDREN
          {:children (cons fexpr args)})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  Compiler$KeywordInvokeExpr
  (analysis->map
    [expr env]
    (let [field (partial field-accessor Compiler$KeywordInvokeExpr)
          target (analysis->map (field 'target expr) env)]
      (merge
        {:op :keyword-invoke
         :env (assoc env
                     :line (field 'line expr)
                     :source (field 'source expr))
         :kw (field 'kw expr)
         :tag (field 'tag expr)
         :target target}
        (when @CHILDREN
          {:children [target]})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  ;; TheVarExpr
  Compiler$TheVarExpr
  (analysis->map
    [expr env]
    (merge
      {:op :the-var
       :env env
       :var (.var expr)}
      (when @JAVA-OBJ
        {:Expr-obj expr})))

  ;; VarExpr
  Compiler$VarExpr
  (analysis->map
    [expr env]
    (merge
      {:op :var
       :env env
       :var (.var expr)
       :tag (.tag expr)}
      (when @JAVA-OBJ
        {:Expr-obj expr})))

  ;; UnresolvedVarExpr
  Compiler$UnresolvedVarExpr
  (analysis->map
    [expr env]
    (let [field (partial field-accessor Compiler$UnresolvedVarExpr)]
      (merge
        {:op :unresolved-var
         :env env
         :sym (field 'symbol expr)}
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  ;; ObjExprs
  Compiler$ObjExpr
  (analysis->map
    [expr env]
    (merge
      {:op :obj-expr
       :env env
       :tag (.tag expr)}
      (when @JAVA-OBJ
        {:Expr-obj expr})))

  ;; FnExpr (extends ObjExpr)
  Compiler$NewInstanceMethod
  (analysis->map
    [obm env]
    (let [body (analysis->map (.body obm) env)]
      (merge
        {:op :new-instance-method
         :env env
         :body body}
        (when @CHILDREN
          {:children [body]})
        (when @JAVA-OBJ
          {:ObjMethod-obj obm}))))

  Compiler$FnMethod
  (analysis->map
    [obm env]
    (let [body (analysis->map (.body obm) env)
          required-params (doall (map analysis->map (.reqParms obm) (repeat env)))]
      (merge
        {:op :fn-method
         :env env
         :body body
         ;; Map LocalExpr@xx -> LocalExpr@xx
         ;;:locals (map analysis->map (keys (.locals obm)) (repeat env))
         :required-params required-params
         :rest-param (let [rest-param (.restParm obm)]
                       (if rest-param
                         (analysis->map rest-param env)
                         rest-param))}
        (when @CHILDREN
          {:children [body]})
        (when @JAVA-OBJ
          {:ObjMethod-obj obm}))))

  Compiler$FnExpr
  (analysis->map
    [expr env]
    (let [methods (doall (map analysis->map (.methods expr) (repeat env)))
          parent-field (partial field-accessor Compiler$ObjExpr)]
      (merge
        {:op :fn-expr
         :env (assoc env :line (parent-field 'line expr))
         :methods methods
         :variadic-method (when-let [variadic-method (.variadicMethod expr)]
                            (analysis->map variadic-method env))
         :tag (.tag expr)}
        (when-let [nme (.thisName expr)]
          {:name (symbol nme)})
        (when @CHILDREN
          {:children methods})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  ;; NewInstanceExpr
  Compiler$NewInstanceExpr
  (analysis->map
    [expr env]
    (let [field (partial field-accessor Compiler$NewInstanceExpr)
          parent-field (partial field-accessor Compiler$ObjExpr)
          methods (doall (map analysis->map (field 'methods expr) (repeat env)))]
      (merge
        {:op :deftype*
         :env (assoc env :line (parent-field 'line expr))
         :methods methods
         :mmap (field 'mmap expr)
         :covariants (field 'covariants expr)
         :tag (.tag expr)}
        (when @CHILDREN
          {:children methods})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  ;; InstanceOfExpr
  Compiler$InstanceOfExpr
  (analysis->map
    [expr env]
    (let [field (partial field-accessor Compiler$InstanceOfExpr)
          exp (analysis->map (field 'expr expr) env)]
      (merge
        {:op :instance-of
         :env env
         :class (field 'c expr)
         :the-expr exp}
        (when @CHILDREN
          {:children [exp]})
        (when @JAVA-OBJ 
          {:Expr-obj expr}))))

  ;; MetaExpr
  Compiler$MetaExpr
  (analysis->map
    [expr env]
    (let [meta (analysis->map (.meta expr) env)
          the-expr (analysis->map (.expr expr) env)]
      (merge
        {:op :meta
         :env env
         :meta meta
         :expr the-expr}
        (when @CHILDREN
          {:children [meta the-expr]})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  ;; do
  Compiler$BodyExpr
  (analysis->map
    [expr env]
    (let [exprs (doall (map analysis->map (.exprs expr) (repeat env)))]
      (merge
        {:op :do
         :env (inherit-env (last exprs) env)
         :exprs exprs}
        (when @CHILDREN
          {:children exprs})
        (when @JAVA-OBJ 
          {:Expr-obj expr}))))

  ;; if
  Compiler$IfExpr
  (analysis->map
    [expr env]
    (let [test (analysis->map (.testExpr expr) env)
          then (analysis->map (.thenExpr expr) env)
          else (analysis->map (.elseExpr expr) env)]
      (merge
        {:op :if
         :env (assoc env
                     :line (.line expr))
         :test test
         :then then
         :else else}
        (when @CHILDREN
          {:children [test then else]})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  ;; case
  Compiler$CaseExpr
  (analysis->map
    [expr env]
    (let [the-expr (analysis->map (.expr expr) env)
          tests (doall (map analysis->map (vals (.tests expr)) (repeat env)))
          thens (doall (map analysis->map (vals (.thens expr)) (repeat env)))
          default (analysis->map (.defaultExpr expr) env)]
      (merge
        {:op :case*
         :env env
         :the-expr the-expr
         :tests tests
         :thens thens
         :default default}
        (when @CHILDREN
          {:children (concat [the-expr] tests thens [default])})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))


  ;; ImportExpr
  Compiler$ImportExpr
  (analysis->map
    [expr env]
    (merge
      {:op :import*
       :env env
       :class-str (.c expr)}
       (when @JAVA-OBJ
         {:Expr-obj expr})))

  ;; AssignExpr (set!)
  Compiler$AssignExpr
  (analysis->map
    [expr env]
    (let [target (analysis->map (.target expr) env)
          val (analysis->map (.val expr) env)]
      (merge
        {:op :set!
         :env env
         :target target
         :val val}
        (when @CHILDREN
          {:children [target val]})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  ;;TryExpr
  Compiler$TryExpr$CatchClause
  (analysis->map
    [ctch env]
    (let [local-binding (analysis->map (.lb ctch) env)
          handler (analysis->map (.handler ctch) env)]
      (merge
        {:op :catch
         :env env
         :class (.c ctch)
         :local-binding local-binding
         :handler handler}
        (when @CHILDREN
          {:children [local-binding handler]})
        (when @JAVA-OBJ
          {:CatchClause-obj ctch}))))

  Compiler$TryExpr
  (analysis->map
    [expr env]
    (let [try-expr (analysis->map (.tryExpr expr) env)
          finally-expr (when-let [finally-expr (.finallyExpr expr)]
                         (analysis->map finally-expr env))
          catch-exprs (doall (map analysis->map (.catchExprs expr) (repeat env)))]
      (merge
        {:op :try
         :env env
         :try-expr try-expr
         :finally-expr finally-expr
         :catch-exprs catch-exprs
         :ret-local (.retLocal expr)
         :finally-local (.finallyLocal expr)}
        (when @CHILDREN
          {:children (concat [try-expr] (when finally-expr [finally-expr]) catch-exprs)})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  ;; RecurExpr
  Compiler$RecurExpr
  (analysis->map
    [expr env]
    (let [field (partial field-accessor Compiler$RecurExpr)
          loop-locals (doall (map analysis->map (.loopLocals expr) (repeat env)))
          args (doall (map analysis->map (.args expr) (repeat env)))]
      (merge
        {:op :recur
         :env (assoc env
                     :line (field 'line expr)
                     :source (field 'source expr))
         :loop-locals loop-locals
         :args args}
        (when @CHILDREN
          {:children (concat loop-locals args)})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  Compiler$MethodParamExpr
  (analysis->map
    [expr env]
    (let [field (partial field-accessor Compiler$MethodParamExpr)
          method (partial method-accessor Compiler$MethodParamExpr)]
      (merge
        {:op :method-param
         :env env
         :class (field 'c expr)
         :can-emit-primitive (method 'canEmitPrimitive expr [])}
        (when @JAVA-OBJ
          {:Expr-obj expr})))))


(defn- analyze*
  "Don't call directly without rebinding *ns*"
  [env form]
  (letfn [(invoke-analyze [context form]
            (push-thread-bindings {Compiler/LOADER (RT/makeClassLoader)})
            (try
              (Compiler/analyze context form)
              (finally
                (pop-thread-bindings))))]
    (let [context (case (:context env)
                    :statement Compiler$C/STATEMENT
                    :expression Compiler$C/EXPRESSION
                    :return Compiler$C/RETURN
                    :eval Compiler$C/EVAL)
          exprs (try
                  (invoke-analyze context form)
                  (catch RuntimeException e
                    (throw (repl/root-cause e))))]
      (analysis->map exprs (merge-with conj (dissoc env :context) {:locals {}})))))

(defn analyze-one
  "Analyze a single form"
  [env form]
  (binding [*ns* (find-ns (-> env :ns :name))]
    (analyze* env form)))

(def ^:private eof (Object.))

(defn forms-seq
  "Lazy seq of forms in a Clojure or ClojureScript file."
  [^java.io.PushbackReader rdr]
  (lazy-seq
   (let [form (read rdr nil eof)]
     (when-not (identical? form eof)
       (lazy-seq (cons form (forms-seq rdr)))))))
       

(defn analyze-path
  "Takes a path and a namespace symbol.
  Returns a seq of maps, with keys :op, :env. If expressions
  have children, will have :children entry."
  ([ns-sym]
   (analyze-path (-> (name ns-sym)
                   (string/replace "." "/")
                   (string/replace "-" "_")
                   (str ".clj"))
                 ns-sym))
  ([source-path ns-sym]
   (require ns-sym)
   (let [uri (io/resource source-path)]
     (with-open [rdr (LineNumberingPushbackReader. (io/reader uri))]
       (let [frms (doall (forms-seq rdr))
             afn #(let [env {:ns {:name ns-sym} :context :eval :locals {}}]
                    (binding [*ns* (find-ns ns-sym)]
                      (analyze* env %)))]
         (map afn frms))))))

(comment
  (ast 
    (try (throw (Exception.)) 
      (catch Exception e (throw e)) 
      (finally 33)))

  (ast
    (let [b 1] 
      (fn [& a] 1)))

  (ast (Integer. (+ 1 1)))

  (ast (map io/file [1 2]))

  )
