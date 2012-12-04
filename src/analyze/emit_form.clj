(ns analyze.emit-form
  (:require [analyze.core :refer [ast]]))

(defmulti map->form :op)

(defmethod map->form :nil [{:keys [val]}] val)
(defmethod map->form :number [{:keys [val]}] val)
(defmethod map->form :constant [{:keys [val]}] val)
(defmethod map->form :string [{:keys [val]}] val)
(defmethod map->form :boolean [{:keys [val]}] val)
(defmethod map->form :keyword [{:keys [val]}] val)

(defmethod map->form :static-method 
  [{:keys [class method-name args]}] 
  `(. ~(symbol (.getName class))
      ~(symbol method-name)
      ~@(map map->form args)))

(defmethod map->form :invoke
  [{:keys [fexpr args]}]
  `(~(map->form fexpr)
       ~@(map map->form args)))

(defmethod map->form :var
  [{:keys [var]}]
  (symbol (str (ns-name (.ns var))) (str (.sym var))))

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
(defmethod map->form :set [{:keys [keyvals]}] (set (map map->form keyvals)))

(defmethod map->form :fn-expr
  [{:keys [methods variadic-method]}]
  (list* 'fn (map map->form (concat methods (when variadic-method [variadic-method])))))

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
  )
