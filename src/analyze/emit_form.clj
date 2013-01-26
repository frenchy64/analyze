(ns analyze.emit-form
  (:require [analyze.core :refer [ast]]))

(defmulti map->form :op)

(defmethod map->form :nil [{:keys [val]}] val)
(defmethod map->form :number [{:keys [val]}] val)
(defmethod map->form :constant [{:keys [val]}] (list 'quote val))
(defmethod map->form :string [{:keys [val]}] val)
(defmethod map->form :boolean [{:keys [val]}] val)
(defmethod map->form :keyword [{:keys [val]}] val)

(defmethod map->form :static-method 
  [{:keys [class method-name args]}] 
  `(~(symbol (.getName class) (str method-name))
       ~@(map map->form args)))

(defmethod map->form :static-field 
  [{:keys [class field-name]}]
  (symbol (.getName class) (str field-name)))

(defmethod map->form :invoke
  [{:keys [fexpr args]}]
  `(~(map->form fexpr)
       ~@(map map->form args)))

(defn- var->symbol [var]
  (symbol (str (ns-name (.ns var))) (str (.sym var))))

(defmethod map->form :the-var
  [{:keys [var]}]
  (list 'var (var->symbol var)))

(defmethod map->form :var
  [{:keys [var]}]
  (var->symbol var))

(defmethod map->form :instance-method
  [{:keys [target method-name args]}]
  `(~(symbol (str "." method-name))
       ~(map->form target)
       ~@(map map->form args)))

(defmethod map->form :new
  [{:keys [class args]}]
  `(new ~(symbol (.getName class))
        ~@(map map->form args)))

(defmethod map->form :empty-expr [{:keys [coll]}] coll)
(defmethod map->form :vector [{:keys [args]}] (vec (map map->form args)))
(defmethod map->form :map [{:keys [keyvals]}] (apply hash-map (map map->form keyvals)))
(defmethod map->form :set [{:keys [keys]}] (set (map map->form keys)))

(defmethod map->form :fn-expr
  [{:keys [methods variadic-method]}]
  (list* 'fn* (map map->form (concat methods (when variadic-method [variadic-method])))))

(defmethod map->form :fn-method
  [{:keys [body required-params rest-param]}]
  `(~(vec (concat (map map->form required-params)
                  (when rest-param
                    ['& (map->form rest-param)])))
       ~(map->form body)))

(defmethod map->form :do
  [{:keys [exprs]}]
  (cond
    (empty? exprs) nil
    (= 1 (count exprs)) (map->form (first exprs))
    :else `(do ~@(map map->form exprs))))

(defmethod map->form :let
  [{:keys [is-loop binding-inits body]}]
  `(~(if is-loop
       'loop*
       'let*)
       ~(vec (apply concat (map map->form binding-inits)))
       ~(map->form body)))

(defmethod map->form :recur
  [{:keys [args]}]
  `(recur ~@(map map->form args)))
          
;to be spliced
(defmethod map->form :binding-init
  [{:keys [local-binding init]}]
  (map map->form [local-binding init]))

(defmethod map->form :local-binding [{:keys [sym]}] sym)
(defmethod map->form :local-binding-expr [{:keys [local-binding]}] (map->form local-binding))

(defmethod map->form :if
  [{:keys [test then else]}] 
  `(if ~@(map map->form [test then else])))

(defmethod map->form :instance-of
  [{:keys [class the-expr]}] 
  `(clojure.core/instance? ~(symbol (.getName class))
                           ~(map->form the-expr)))

(defmethod map->form :def
  [{:keys [var init init-provided]}] 
  `(def ~(.sym var) ~(when init-provided
                       (map->form init))))

;FIXME: methods don't print protocol/interface name
(defmethod map->form :deftype*
  [{:keys [name methods]}] 
  (list* 'deftype* name 'FIXME
         (map map->form methods)))

(defmethod map->form :new-instance-method
  [{:keys [name required-params body]}] 
  (list name (vec (map map->form required-params))
        (map->form body)))

(defmethod map->form :import*
  [{:keys [class-str]}] 
  (list 'import* class-str))

(defmethod map->form :keyword-invoke
  [{:keys [kw target]}] 
  (list (map->form kw) (map->form target)))

(defmethod map->form :throw
  [{:keys [exception]}] 
  (list 'throw (map->form exception)))

(defmethod map->form :try
  [{:keys [try-expr catch-exprs finally-expr ]}] 
  (list* 'try (map->form try-expr)
         (concat
           (map map->form catch-exprs)
           (when finally-expr [(list 'finally (map->form finally-expr))]))))

(defmethod map->form :catch
  [{:keys [class local-binding handler]}]
  (list 'catch (map->form local-binding) 
        (map->form handler)))

;; (from Compiler.java)
;;  //(case* expr shift mask default map<minhash, [test then]> table-type test-type skip-check?)
(defmethod map->form :case*
  [{:keys [the-expr tests thens default tests-hashes shift mask low high switch-type test-type skip-check]}]
  (list 'case*
        (map->form the-expr)
        shift
        mask
        (map->form default)
        (zipmap tests-hashes
                (map vector
                     (map map->form tests)
                     (map map->form thens)))
        switch-type
        test-type
        skip-check))


(comment
  (defmacro frm [f]
    `(-> (ast ~f) map->form))

  (frm 1)
  (frm :a)
  
  (frm (+ 1 2))
  (frm (- 1 2))

  (frm (apply - 1 2))

  (frm (.var - 1 2))

  (frm (Integer. 1))

  (frm ())

  (frm [1])
  (frm [(- 1)])
  (frm {(- 1) 1})
  (frm #{(- 1) 1})

  (frm (let [a '(1 2)]
         (first 1)))
  (frm (loop [a '(1 2)]
         (first 1)))

  (frm (fn [{:keys [a]} b] 1))
  (frm (instance? Class 1))

  (frm nil)
  (frm (def a 1))
  (frm (defn ab [a] a))

  (frm (loop [a 1] (recur 1)))

  ; FIXME
  (frm (deftype A []
         clojure.lang.ISeq
         (first [this] this)))

  (frm (:a {}))
  (frm (throw (Exception. "a")))
  (frm (try 1 2 
         (catch Exception e 
           4)
         (catch Error e
           5)
         (finally 3.2)))
  (frm (Integer/toHexString 1))
  (frm (Integer/TYPE))
  (frm #'conj)
  
  (frm 'a)
  (frm (let [b 1] 
         [b 'a 1]))

  (frm #{1 2 3})
  (frm (case 1 2 3 4))
  (frm (case 1 :a 3 4))
  )
