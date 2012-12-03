(set! *warn-on-reflection* true)

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
                         Compiler$ConstantExpr Compiler$NumberExpr Compiler$NilExpr Compiler$BooleanExpr Compiler$StringExpr
                         Compiler$ObjMethod))
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

(defmacro field 
  "Call a private field, must be known at compile time. Throws an error
  if field is already publicly accessible."
  ([class-obj field] `(field ~class-obj ~field nil))
  ([class-obj field obj]
   (let [{class-flags :flags :keys [members]} (reflect/reflect (resolve class-obj))
         {field-flags :flags} (some #(and (= (:name %) field) %) members)]
     (assert field-flags
             (str "Class " (resolve class-obj) " does not have field " field))
     (assert (not (and (:public class-flags)
                       (:public field-flags)))
             (str "Class " (resolve class-obj) " and field " field " is already public")))
   `(field-accessor ~class-obj '~field ~obj)))

(defn- field-accessor [^Class class-obj field obj]
  (let [^java.lang.reflect.Field 
        field (.getDeclaredField class-obj (name field))]
    (.setAccessible field true)
    (let [ret (.get field obj)]
      (if (instance? Boolean ret)
        (boolean ret)
        ret))))

#_(defn- method-accessor [^Class class-obj method obj types & args]
  (let [^java.lang.reflect.Method 
        method (.getDeclaredMethod class-obj (name method) (into-array Class types))]
    (.setAccessible method true)
    (.invoke method obj (object-array args))))

(defn- inherit-env [expr env]
  (merge env
         (when-let [line (-> expr :env :line)]
           {:line line})
         (when-let [source (-> expr :env :source)]
           {:source source})))

(defprotocol AnalysisToMap
  (analysis->map [aobj env]
    "Recursively converts the output of the Compiler's analysis to a map"))

;; Literals extending abstract class Compiler$LiteralExpr and have public value fields

(defmacro literal-dispatch [disp-class op-keyword]
  `(extend-protocol AnalysisToMap
     ~disp-class
     (~'analysis->map
       [expr# env#]
       (let []
         (merge
           {:op ~op-keyword
            :env env#
            :val (.eval expr#)}
           (when @JAVA-OBJ
             {:Expr-obj expr#}))))))

(literal-dispatch Compiler$KeywordExpr :keyword)
(literal-dispatch Compiler$ConstantExpr :constant)
(literal-dispatch Compiler$NumberExpr :number)
(literal-dispatch Compiler$StringExpr :string)
(literal-dispatch Compiler$NilExpr :nil)
(literal-dispatch Compiler$BooleanExpr :boolean)

(extend-protocol AnalysisToMap

  ;; def
  Compiler$DefExpr
  (analysis->map
    [expr env]
    (let [init (analysis->map (field Compiler$DefExpr init expr) env)
          meta (when-let [meta (field Compiler$DefExpr meta expr)]
                 (analysis->map meta env))]
      (merge 
        {:op :def
         :env (assoc env
                     :source (field Compiler$DefExpr source expr)
                     :line (field Compiler$DefExpr line expr))
         :var (field Compiler$DefExpr var expr)
         :meta meta
         :init init
         :init-provided (field Compiler$DefExpr initProvided expr)
         :is-dynamic (field Compiler$DefExpr isDynamic expr)}
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
          binding-inits (map analysis->map (.bindingInits expr) (repeat env))]
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
          binding-inits (map analysis->map (.bindingInits expr) (repeat env))]
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
    (let [args (map analysis->map (field Compiler$StaticMethodExpr args expr) (repeat env))]
      (merge
        {:op :static-method
         :env (assoc env
                     :source (field Compiler$StaticMethodExpr source expr)
                     :line (field Compiler$StaticMethodExpr line expr))
         :class (field Compiler$StaticMethodExpr c expr)
         :method-name (field Compiler$StaticMethodExpr methodName expr)
         :method (when-let [method (field Compiler$StaticMethodExpr method expr)]
                   (@#'reflect/method->map method))
         :args args
         :tag (field Compiler$StaticMethodExpr tag expr)}
        (when @CHILDREN
          {:children args})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  Compiler$InstanceMethodExpr
  (analysis->map
    [expr env]
    (let [target (analysis->map (field Compiler$InstanceMethodExpr target expr) env)
          args (map analysis->map (field Compiler$InstanceMethodExpr args expr) (repeat env))]
      (merge
        {:op :instance-method
         :env (assoc env
                     :source (field Compiler$InstanceMethodExpr source expr)
                     :line (field Compiler$InstanceMethodExpr line expr))
         :target target
         :method-name (field Compiler$InstanceMethodExpr methodName expr)
         :method (when-let [method (field Compiler$InstanceMethodExpr method expr)]
                   (@#'reflect/method->map method))
         :args args
         :tag (field Compiler$InstanceMethodExpr tag expr)}
        (when @CHILDREN
          {:children (cons target args)})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  ;; Fields
  Compiler$StaticFieldExpr
  (analysis->map
    [expr env]
    (let []
      (merge
        {:op :static-field
         :env (assoc env
                     :line (field Compiler$StaticFieldExpr line expr))
         :class (field Compiler$StaticFieldExpr c expr)
         :field-name (field Compiler$StaticFieldExpr fieldName expr)
         :field (when-let [field (field Compiler$StaticFieldExpr field expr)]
                  (@#'reflect/field->map field))
         :tag (field Compiler$StaticFieldExpr tag expr)}
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  Compiler$InstanceFieldExpr
  (analysis->map
    [expr env]
    (let [target (analysis->map (field Compiler$InstanceFieldExpr target expr) env)]
      (merge
        {:op :instance-field
         :env (assoc env
                     :line (field Compiler$InstanceFieldExpr line expr))
         :target target
         :target-class (field Compiler$InstanceFieldExpr targetClass expr)
         :field (when-let [field (field Compiler$InstanceFieldExpr field expr)]
                  (@#'reflect/field->map field))
         :field-name (field Compiler$InstanceFieldExpr fieldName expr)
         :tag (field Compiler$InstanceFieldExpr tag expr)}
        (when @CHILDREN
          {:children [target]})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  Compiler$NewExpr
  (analysis->map
    [expr env]
    (let [args (map analysis->map (.args expr) (repeat env))]
      (merge
        {:op :new
         :env env 
              ; should be there but isn't
              ;(assoc env
        ;             :line (.line expr)
        ;             )
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
    (let [keys (map analysis->map (.keys expr) (repeat env))]
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
    (let [args (map analysis->map (.args expr) (repeat env))]
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
    (let [keyvals (map analysis->map (.keyvals expr) (repeat env))]
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
    (let [target (analysis->map (field Compiler$MonitorEnterExpr target expr) env)]
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
    (let [target (analysis->map (field Compiler$MonitorExitExpr target expr) env)]
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
    (let [exception (analysis->map (field Compiler$ThrowExpr excExpr expr) env)]
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
    (let [fexpr (analysis->map (field Compiler$InvokeExpr fexpr expr) env)
          args (map analysis->map (field Compiler$InvokeExpr args expr) (repeat env))]
      (merge
        {:op :invoke
         :env (assoc env
                     :line (field Compiler$InvokeExpr line expr)
                     :source (field Compiler$InvokeExpr source expr))
         :fexpr fexpr
         :tag (field Compiler$InvokeExpr tag expr)
         :args args
         :is-protocol (field Compiler$InvokeExpr isProtocol expr)
         :is-direct (field Compiler$InvokeExpr isDirect expr)
         :site-index (field Compiler$InvokeExpr siteIndex expr)
         :protocol-on (field Compiler$InvokeExpr protocolOn expr)}
        (when-let [m (field Compiler$InvokeExpr onMethod expr)]
          {:method (@#'reflect/method->map m)})
        (when @CHILDREN
          {:children (cons fexpr args)})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  Compiler$KeywordInvokeExpr
  (analysis->map
    [expr env]
    (let [target (analysis->map (field Compiler$KeywordInvokeExpr target expr) env)
          kw (analysis->map (field Compiler$KeywordInvokeExpr kw expr) env)]
      (merge
        {:op :keyword-invoke
         :env (assoc env
                     :line (field Compiler$KeywordInvokeExpr line expr)
                     :source (field Compiler$KeywordInvokeExpr source expr))
         :kw kw
         :tag (field Compiler$KeywordInvokeExpr tag expr)
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
    (let []
      (merge
        {:op :unresolved-var
         :env env
         :sym (.symbol expr)}
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
         :env (assoc env
                     :line (.line obm))
         :name (symbol (field Compiler$NewInstanceMethod name obm))
         :required-params (map analysis->map 
                               (concat [((field Compiler$ObjMethod indexlocals obm) 0)]
                                       (field Compiler$ObjMethod argLocals obm))
                               (repeat env))
         :body body}
        (when @CHILDREN
          {:children [body]})
        (when @JAVA-OBJ
          {:ObjMethod-obj obm}))))

  Compiler$FnMethod
  (analysis->map
    [obm env]
    (let [body (analysis->map (.body obm) env)
          required-params (map analysis->map (.reqParms obm) (repeat env))]
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
    (let [methods (map analysis->map (.methods expr) (repeat env))]
      (merge
        {:op :fn-expr
         :env (assoc env 
                     :line (.line expr))
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
    (let [methods (map analysis->map (field Compiler$NewInstanceExpr methods expr) (repeat env))]
      (merge
        {:op :deftype*
         :name (symbol (.name expr))
         :env env
            ; non-existent 
;            (assoc env 
;                     :line (.line expr))
         :methods methods
         :mmap (field Compiler$NewInstanceExpr mmap expr)

         ;Reflection
         ;TODO unordered? where is the ordered field list
;         :fields (into {} (for [[k v] (.fields expr)] ;this is private!
;                            [k (analysis->map v env)]))
         
         ;Reflection
         ;What is this?
         ;:covariants (field 'covariants expr)

         :tag (.tag expr)}
        (when @CHILDREN
          {:children methods})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  ;; InstanceOfExpr
  Compiler$InstanceOfExpr
  (analysis->map
    [expr env]
    (let [exp (analysis->map (field Compiler$InstanceOfExpr expr expr) env)]
      (merge
        {:op :instance-of
         :env env
         :class (field Compiler$InstanceOfExpr c expr)
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
    (let [exprs (map analysis->map (.exprs expr) (repeat env))]
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
          tests (map analysis->map (vals (.tests expr)) (repeat env))
          thens (map analysis->map (vals (.thens expr)) (repeat env))
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
          catch-exprs (map analysis->map (.catchExprs expr) (repeat env))]
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
    (let [loop-locals (map analysis->map (.loopLocals expr) (repeat env))
          args (map analysis->map (.args expr) (repeat env))]
      (merge
        {:op :recur
         :env (assoc env
                     :line (field Compiler$RecurExpr line expr)
                     :source (field Compiler$RecurExpr source expr))
         :loop-locals loop-locals
         :args args}
        (when @CHILDREN
          {:children (concat loop-locals args)})
        (when @JAVA-OBJ
          {:Expr-obj expr}))))

  Compiler$MethodParamExpr
  (analysis->map
    [expr env]
    (let []
      (merge
        {:op :method-param
         :env env
         :class (.getJavaClass expr)
         :can-emit-primitive (.canEmitPrimitive expr)}
        (when @JAVA-OBJ
          {:Expr-obj expr})))))

(defn- analyze*
  "Must be called after binding the appropriate Compiler and RT dynamic Vars."
  [env form]
  (letfn [(invoke-analyze [context form]
            (Compiler/analyze context form))]
    (let [context (case (:context env)
                    :statement Compiler$C/STATEMENT
                    :expression Compiler$C/EXPRESSION
                    :return Compiler$C/RETURN
                    :eval Compiler$C/EVAL)
          expr-ast (try
                     (invoke-analyze context form)
                     (catch RuntimeException e
                       (throw (repl/root-cause e))))]
      (analysis->map expr-ast (merge-with conj (dissoc env :context) {:locals {}})))))

(defn analyze-one
  "Analyze a single form"
  [env form]
  (analyze* env #_(find-ns (-> env :ns :name)) form))

(defn forms-seq
  "Lazy seq of forms in a Clojure or ClojureScript file."
  [^java.io.PushbackReader rdr]
  (let [eof (reify)]
    (lazy-seq
      (let [form (read rdr nil eof)]
        (when-not (identical? form eof)
          (lazy-seq (cons form (forms-seq rdr))))))))
       
(defn uri-for-ns 
  "Returns a URI representing the namespace"
  [ns-sym]
  (let [source-path (-> (name ns-sym)
                      (string/replace "." "/")
                      (string/replace "-" "_")
                      (str ".clj"))]
    (io/resource source-path)))

(defn pb-reader-for-ns
  "Returns a LineNumberingPushbackReader for namespace ns-sym"
  [ns-sym]
  (let [uri (uri-for-ns ns-sym)]
    (LineNumberingPushbackReader. (io/reader uri))))

(defn analyze-ns
  "Takes a LineNumberingPushbackReader and a namespace symbol.
  Returns a vector of maps, with keys :op, :env. If expressions
  have children, will have :children entry.
  
  eg. (analyze-path (pb-reader-for-ns 'my.ns) 'my-ns)"
  [rdr source-path source-nsym]
  (let [eof (reify)
        ^LineNumberingPushbackReader 
        pushback-reader (if (instance? LineNumberingPushbackReader rdr)
                          rdr
                          (LineNumberingPushbackReader. rdr))]
    (do
      (push-thread-bindings {Compiler/LOADER (RT/makeClassLoader)
                             Compiler/SOURCE_PATH (str source-path)
                             Compiler/SOURCE (str source-nsym)
                             Compiler/METHOD nil
                             Compiler/LOCAL_ENV nil
                             Compiler/LOOP_LOCALS nil
                             Compiler/NEXT_LOCAL_NUM 0
                             RT/CURRENT_NS @RT/CURRENT_NS
                             Compiler/LINE_BEFORE (.getLineNumber pushback-reader)
                             Compiler/COLUMN_BEFORE (.getColumnNumber pushback-reader)
                             Compiler/LINE_AFTER (.getLineNumber pushback-reader)
                             Compiler/COLUMN_AFTER (.getColumnNumber pushback-reader)
                             RT/UNCHECKED_MATH @RT/UNCHECKED_MATH
                             (field RT WARN_ON_REFLECTION) @(field RT WARN_ON_REFLECTION)
                             RT/DATA_READERS @RT/DATA_READERS})
      (try
        (let [eof (reify)]
          (loop [form (read pushback-reader nil eof)
                 out []]
            (if (identical? form eof)
              out
              ;; FIXME shouldn't be source-nsym here
              (let [env {:ns {:name source-nsym} :context :eval :locals {}}
                    m (analyze* env form)]
                (recur (read pushback-reader nil eof) (conj out m))))))
        (finally
          (pop-thread-bindings))))))

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

  (ast (do 
         (require '[clojure.repl :refer [pst]])
         (pst)))

  )
