(ns clj-grapher.application
  (:gen-class :extends javafx.application.Application)
  (:require [clj-grapher.gui :refer [start]])
  (:import [javafx.application Application]))

(defn -start [this stage]
  (start stage))

(defn launch-application [& args]
  (Application/launch (Class/forName "clj_grapher.application")
                      (into-array String args)))
