(ns analyze.examples.check-hint
  (:require [analyze.core :as a]))

(defmethod check-hint :var
  [{:keys [info env] :as arg}]
  (env (:name info)))

