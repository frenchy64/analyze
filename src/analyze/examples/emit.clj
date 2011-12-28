(ns analyze.examples.emit
  "Emit phase from ClojureScript compiler"
  (:require [analyze.core :as a])
  (:require [clojure.string :as string]))

(defn- comma-sep [xs]
  (apply str (interpose "," xs)))

(defmulti emit-constant class)
(defmethod emit-constant nil [x] (print "null"))
(defmethod emit-constant Long [x] (print x))
(defmethod emit-constant Integer [x] (print x)) ; reader puts Integers in metadata
(defmethod emit-constant Double [x] (print x))
(defmethod emit-constant String [x] (pr x))
(defmethod emit-constant Boolean [x] (print (if x "true" "false")))
(defmethod emit-constant Character [x] (pr (str x)))

(defmethod emit-constant java.util.regex.Pattern [x]
  (let [[_ flags pattern] (re-find #"^(?:\(\?([idmsux]*)\))?(.*)" (str x))]
    (print (str \/ (.replaceAll (re-matcher #"/" pattern) "\\\\/") \/ flags))))

(defmethod emit-constant clojure.lang.Keyword [x]
           (pr (str \uFDD0 \'
                    (if (namespace x)
                      (str (namespace x) "/") "")
                    (name x))))

(defmethod emit-constant clojure.lang.Symbol [x]
           (pr (str \uFDD1 \'
                    (if (namespace x)
                      (str (namespace x) "/") "")
                    (name x))))

(defn- emit-meta-constant [x string]
  (if (meta x)
    (do
      (print (str "cljs.core.with_meta(" string ","))
      (emit-constant (meta x))
      (print ")"))
    (print string)))

(defmethod emit-constant clojure.lang.PersistentList$EmptyList [x]
  (emit-meta-constant x "cljs.core.List.EMPTY"))

(defmethod emit-constant clojure.lang.PersistentList [x]
  (emit-meta-constant x
    (str "cljs.core.list("
         (comma-sep (map #(with-out-str (emit-constant %)) x))
         ")")))

(defmethod emit-constant clojure.lang.Cons [x]
  (emit-meta-constant x
    (str "cljs.core.list("
         (comma-sep (map #(with-out-str (emit-constant %)) x))
         ")")))

(defmethod emit-constant clojure.lang.IPersistentVector [x]
  (emit-meta-constant x
    (str "(new cljs.core.Vector(null, ["
         (comma-sep (map #(with-out-str (emit-constant %)) x))
         "]))")))

(defmethod emit-constant clojure.lang.IPersistentMap [x]
  (emit-meta-constant x
    (str "cljs.core.hash_map("
         (comma-sep (map #(with-out-str (emit-constant %))
                         (apply concat x)))
         ")")))

(defmethod emit-constant clojure.lang.PersistentHashSet [x]
  (emit-meta-constant x
    (str "cljs.core.set(["
         (comma-sep (map #(with-out-str (emit-constant %)) x))
         "])")))

(defmulti emit :op)

(defn ^String emits [expr]
  (with-out-str (emit expr)))

(defn emit-block
  [context statements ret]
  (if statements
    (let [body (str (apply str (map emits statements)) (emits ret))]
      (print body))
    (emit ret)))

(defmacro emit-wrap [env & body]
  `(let [env# ~env]
     (when (= :return (:context env#)) (print "return "))
     ~@body
     (when-not (= :expr (:context env#)) (print ";\n"))))

(defmethod emit :var
  [{:keys [info env] :as arg}]
  (emit-wrap env (print (munge (:name info)))))

(defmethod emit :meta
  [{:keys [expr meta env]}]
  (emit-wrap env
    (print (str "cljs.core.with_meta(" (emits expr) "," (emits meta) ")"))))

(defmethod emit :map
  [{:keys [children env simple-keys? keys vals]}]
  (emit-wrap env
    (if simple-keys?
      (print (str "cljs.core.ObjMap.fromObject(["
                  (comma-sep (map emits keys)) ; keys
                  "],{"
                  (comma-sep (map (fn [k v] (str (emits k) ":" (emits v)))
                                  keys vals)) ; js obj
                  "})"))
      (print (str "cljs.core.HashMap.fromArrays(["
                  (comma-sep (map emits keys))
                  "],["
                  (comma-sep (map emits vals))
                  "])")))))

(defmethod emit :vector
  [{:keys [children env]}]
  (emit-wrap env
    (print (str "cljs.core.Vector.fromArray(["
                (comma-sep (map emits children)) "])"))))

(defmethod emit :set
  [{:keys [children env]}]
  (emit-wrap env
    (print (str "cljs.core.set(["
                (comma-sep (map emits children)) "])"))))

(defmethod emit :constant
  [{:keys [form env]}]
  (when-not (= :statement (:context env))
    (emit-wrap env (emit-constant form))))

(defmethod emit :if
  [{:keys [test then else env]}]
  (let [context (:context env)]
    (if (= :expr context)
      (print (str "(cljs.core.truth_(" (emits test) ")?" (emits then) ":" (emits else) ")"))
      (print (str "if(cljs.core.truth_(" (emits test) "))\n{" (emits then) "} else\n{" (emits else) "}\n")))))

(defmethod emit :throw
  [{:keys [throw env]}]
  (if (= :expr (:context env))
    (print (str "(function(){throw " (emits throw) "})()"))
    (print (str "throw " (emits throw) ";\n"))))

(defn emit-comment
  "Emit a nicely formatted comment string."
  [doc jsdoc]
  (let [docs (when doc [doc])
        docs (if jsdoc (concat docs jsdoc) docs)
        docs (remove nil? docs)]
    (letfn [(print-comment-lines [e] (doseq [next-line (string/split-lines e)]
                                       (println "*" (string/trim next-line))))]
      (when (seq docs)
        (println "/**")
        (doseq [e docs]
          (when e
            (print-comment-lines e)))
        (println "*/")))))

(defmethod emit :def
  [{:keys [name init env doc export]}]
  (when init
    (emit-comment doc (:jsdoc init))
    (print name)
    (print (str " = " (emits init)))
    (when-not (= :expr (:context env)) (print ";\n"))
    (when export
      (println (str "goog.exportSymbol('" export "', " name ");")))))

(defn emit-apply-to
  [{:keys [name params env]}]
  (let [arglist (gensym "arglist__")
        delegate-name (str name "__delegate")]
    (println (str "(function (" arglist "){"))
    (doseq [[i param] (map-indexed vector (butlast params))]
      (print (str "var " param " = cljs.core.first("))
      (dotimes [_ i] (print "cljs.core.next("))
      (print (str arglist ")"))
      (dotimes [_ i] (print ")"))
      (println ";"))
    (if (< 1 (count params))
      (do
        (print (str "var " (last params) " = cljs.core.rest("))
        (dotimes [_ (- (count params) 2)] (print "cljs.core.next("))
        (print arglist)
        (dotimes [_ (- (count params) 2)] (print ")"))
        (println ");")
        (println (str "return " delegate-name ".call(" (string/join ", " (cons "this" params)) ");")))
      (do
        (print (str "var " (last params) " = "))
        (print "cljs.core.seq(" arglist ");")
        (println ";")
        (println (str "return " delegate-name ".call(" (string/join ", " (cons "this" params)) ");"))))
    (print "})")))

(defn emit-fn-method
  [{:keys [gthis name variadic params statements ret env recurs max-fixed-arity]}]
  (emit-wrap env
             (print (str "(function " name "(" (comma-sep params) "){\n"))
             (when gthis
               (println (str "var " gthis " = this;")))
             (when recurs (print "while(true){\n"))
             (emit-block :return statements ret)
             (when recurs (print "break;\n}\n"))
             (print "})")))

(defn emit-variadic-fn-method
  [{:keys [gthis name variadic params statements ret env recurs max-fixed-arity] :as f}]
  (emit-wrap env
             (let [name (or name (gensym))
                   delegate-name (str name "__delegate")]
               (println "(function() { ")
               (println (str "var " delegate-name " = function (" (comma-sep params) "){"))
               (when recurs (print "while(true){\n"))
               (emit-block :return statements ret)
               (when recurs (print "break;\n}\n"))
               (println "};")

               (print (str "var " name " = function (" (comma-sep
                                                        (if variadic
                                                          (concat (butlast params) ['var_args])
                                                          params)) "){\n"))
               (when gthis
                 (println (str "var " gthis " = this;")))
               (when variadic
                 (println (str "var " (last params) " = null;"))
                 (println (str "if (goog.isDef(var_args)) {"))
                 (println (str "  " (last params) " = cljs.core.array_seq(Array.prototype.slice.call(arguments, " (dec (count params)) "),0);"))
                 (println (str "} ")))
               (println (str "return " delegate-name ".call(" (string/join ", " (cons "this" params)) ");"))
               (println "};")

               (println (str name ".cljs$lang$maxFixedArity = " max-fixed-arity ";"))
               (println (str name ".cljs$lang$applyTo = "
                             (with-out-str
                               (emit-apply-to (assoc f :name name)))
                             ";"))
               (println (str "return " name ";"))
               (println "})()"))))

(defmethod emit :fn
  [{:keys [name env methods max-fixed-arity variadic recur-frames]}]
  ;;fn statements get erased, serve no purpose and can pollute scope if named
  (when-not (= :statement (:context env))
    (let [loop-locals (seq (mapcat :names (filter #(and % @(:flag %)) recur-frames)))]
      (when loop-locals
        (when (= :return (:context env))
            (print "return "))
        (println (str "((function (" (comma-sep loop-locals) "){"))
        (when-not (= :return (:context env))
            (print "return ")))
      (if (= 1 (count methods))
        (if variadic
          (emit-variadic-fn-method (assoc (first methods) :name name))
          (emit-fn-method (assoc (first methods) :name name)))
        (let [name (or name (gensym))
              maxparams (apply max-key count (map :params methods))
              mmap (zipmap (repeatedly #(gensym (str name  "__"))) methods)
              ms (sort-by #(-> % second :params count) (seq mmap))]
          (when (= :return (:context env))
            (print "return "))
          (println "(function() {")
          (println (str "var " name " = null;"))
          (doseq [[n meth] ms]
            (println (str "var " n " = " (with-out-str (if (:variadic meth)
                                                         (emit-variadic-fn-method meth)
                                                         (emit-fn-method meth))) ";")))
          (println (str name " = function(" (comma-sep (if variadic
                                                         (concat (butlast maxparams) ['var_args])
                                                         maxparams)) "){"))
          (when variadic
            (println (str "var " (last maxparams) " = var_args;")))
          (println "switch(arguments.length){")
          (doseq [[n meth] ms]
            (if (:variadic meth)
              (do (println "default:")
                  (println (str "return " n ".apply(this,arguments);")))
              (let [pcnt (count (:params meth))]
                (println "case " pcnt ":")
                (println (str "return " n ".call(this" (if (zero? pcnt) nil
                                                           (str "," (comma-sep (take pcnt maxparams)))) ");")))))
          (println "}")
          (println "throw('Invalid arity: ' + arguments.length);")
          (println "};")
          (when variadic
            (println (str name ".cljs$lang$maxFixedArity = " max-fixed-arity ";"))
            (println (str name ".cljs$lang$applyTo = " (some #(let [[n m] %] (when (:variadic m) n)) ms) ".cljs$lang$applyTo;")))
          (println (str "return " name ";"))
          (println "})()")))
      (when loop-locals
        (println (str ";})(" (comma-sep loop-locals) "))"))))))

(defmethod emit :do
  [{:keys [statements ret env]}]
  (let [context (:context env)]
    (when (and statements (= :expr context)) (print "(function (){"))
    ;(when statements (print "{\n"))
    (emit-block context statements ret)
    ;(when statements (print "}"))
    (when (and statements (= :expr context)) (print "})()"))))

(defmethod emit :try*
  [{:keys [env try catch name finally]}]
  (let [context (:context env)
        subcontext (if (= :expr context) :return context)]
    (if (or name finally)
      (do
        (when (= :expr context) (print "(function (){"))
        (print "try{")
        (let [{:keys [statements ret]} try]
          (emit-block subcontext statements ret))
        (print "}")
        (when name
          (print (str "catch (" name "){"))
          (when catch
            (let [{:keys [statements ret]} catch]
              (emit-block subcontext statements ret)))      
          (print "}"))
        (when finally
          (let [{:keys [statements ret]} finally]
            (assert (not= :constant (:op ret)) "finally block cannot contain constant")
            (print "finally {")
            (emit-block subcontext statements ret)
            (print "}")))
        (when (= :expr context) (print "})()")))
      (let [{:keys [statements ret]} try]
        (when (and statements (= :expr context)) (print "(function (){"))
        (emit-block subcontext statements ret)
        (when (and statements (= :expr context)) (print "})()"))))))

(defmethod emit :let
  [{:keys [bindings statements ret env loop]}]
  (let [context (:context env)
        bs (map (fn [{:keys [name init]}]
                  (str "var " name " = " (emits init) ";\n"))
                bindings)]
    (when (= :expr context) (print "(function (){"))
    (print (str (apply str bs) "\n"))
    (when loop (print "while(true){\n"))
    (emit-block (if (= :expr context) :return context) statements ret)
    (when loop (print "break;\n}\n"))
    ;(print "}")
    (when (= :expr context) (print "})()"))))

(defmethod emit :recur
  [{:keys [frame exprs env]}]
  (let [temps (vec (take (count exprs) (repeatedly gensym)))
        names (:names frame)]
    (print "{\n")
    (dotimes [i (count exprs)]
      (print (str "var " (temps i) " = " (emits (exprs i)) ";\n")))
    (dotimes [i (count exprs)]
      (print (str (names i) " = " (temps i) ";\n")))
    (print "continue;\n")
    (print "}\n")))

(defmethod emit :invoke
  [{:keys [f args env]}]
  (emit-wrap env
             (print (str (emits f) ".call("
                         (comma-sep (cons "null" (map emits args)))
                         ")"))))

(defmethod emit :new
  [{:keys [ctor args env]}]
  (emit-wrap env
             (print (str "(new " (emits ctor) "("
                         (comma-sep (map emits args))
                         "))"))))

(defmethod emit :set!
  [{:keys [target val env]}]
  (emit-wrap env (print (str (emits target) " = "(emits val)))))

(defmethod emit :ns
  [{:keys [name requires uses requires-macros env]}]
  (println (str "goog.provide('" (munge name) "');"))
  (when-not (= name 'cljs.core)
    (println (str "goog.require('cljs.core');")))
  (doseq [lib (into (vals requires) (vals uses))]
    (println (str "goog.require('" (munge lib) "');"))))

(defmethod emit :deftype*
  [{:keys [t fields]}]
  (let [fields (map munge fields)]
    (println "\n/**\n* @constructor\n*/")
    (println (str t " = (function (" (comma-sep (map str fields)) "){"))
    (doseq [fld fields]
      (println (str "this." fld " = " fld ";")))
    (println "})")))

(defmethod emit :defrecord*
  [{:keys [t fields]}]
  (let [fields (map munge fields)]
    (println "\n/**\n* @constructor")
    (doseq [fld fields]
      (println (str "* @param {*} " fld)))
    (println "* @param {*=} __meta \n* @param {*=} __extmap\n*/")
    (println (str t " = (function (" (comma-sep (map str fields)) ", __meta, __extmap){"))
    (doseq [fld fields]
      (println (str "this." fld " = " fld ";")))
    (println (str "if(arguments.length>" (count fields) "){"))
    ;; (println (str "this.__meta = arguments[" (count fields) "];"))
    ;; (println (str "this.__extmap = arguments[" (inc (count fields)) "];"))
    (println (str "this.__meta = __meta;"))
    (println (str "this.__extmap = __extmap;"))
    (println "} else {")
    (print (str "this.__meta="))
    (emit-constant nil)
    (println ";")
    (print (str "this.__extmap="))
    (emit-constant nil)
    (println ";")
    (println "}")
    (println "})")))

(defmethod emit :dot
  [{:keys [target field method args env]}]
  (emit-wrap env
             (if field
               (print (str (emits target) "." field))
               (print (str (emits target) "." method "("
                           (comma-sep (map emits args))
                           ")")))))

(comment
(map emit (a/analyze-namespace 'clojure.set))
  )
