(ns analyze.port
  (:require [clojure.reflect :as reflect]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [java.security PrivilegedAction AccessController]
           [clojure.lang RT]))

(defonce namespaces (atom {}))


(defn current-reflector []
  (reflect/->JavaReflector (.getContextClassLoader (Thread/currentThread))))

;; port of `analyze` method, line 6213, clojure.lang.RT

(defn maybe-class [refl sym]
  (try
    (do-reflect refl sym)
    (catch Exception e)))

(comment
(defn analyze-symbol
  "Finds the var associated with sym"
  [env sym]
  (let [ret {:env env :form sym}
        lb (-> env :locals sym)]
    (if lb 
      (assoc ret :op :local :info lb)
      (if-let [reflclass (and (not (namespace sym))
                              (maybe-class (current-reflector) form))]
        (assoc ret {:op :class :class reflclass})

      (assoc ret :op :var :info (resolve-existing-var env sym)))))
)
  )

(defn analyze
  "Given an environment, a map containing {:locals (mapping of names to bindings), :context
  (one of :statement, :expr, :return), :ns (a symbol naming the
  compilation ns)}, and form, returns an expression object (a map
  containing at least :form, :op and :env keys). If expr has any (immediately)
  nested exprs, must have :children [exprs...] entry. This will
  facilitate code walking without knowing the details of the op set."
  ([env form] (analyze env form nil))
  ([env form name]
     (let [form (if (instance? clojure.lang.LazySeq form)
                  (or (seq form) ())
                  form)]
       (cond
        (symbol? form) (analyze-symbol env form)
        (and (seq? form) (seq form)) (analyze-seq env form name)
        (map? form) (analyze-map env form name)
        (vector? form) (analyze-vector env form name)
        (set? form) (analyze-set env form name)
        :else {:op :constant :env env :form form}))))


(defn forms-seq
  "Seq of forms in a Clojure or ClojureScript file."
  ([f]
     (forms-seq f (java.io.PushbackReader. (io/reader f))))
  ([f ^java.io.PushbackReader rdr]
     (if-let [form (read rdr nil nil)]
       (lazy-seq (cons form (forms-seq f rdr)))
       (.close rdr))))

(defn load-path [source-path]
  (binding [*analyzer-ns* 'clojure.core]
    (let [res (.getResource (RT/baseLoader) file-name)
          _ (assert res) 
          strm (.getResourceAsStream (RT/baseLoader) file-name)]
      (with-open [rdr (PushbackReader. (InputStreamReader. strm))]
        (doall
          (map #(let [env {:ns (@namespaces *analyzer-ns*) :context :statement :locals {} :bindings {::warn-on-reflection true
                                                                                                     ::unchecked-math false}}]
                  (analyze env %))
               (forms-seq nil rdr)))))))
