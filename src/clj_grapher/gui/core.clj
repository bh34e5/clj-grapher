(ns clj-grapher.gui.core
  (:gen-class :extends javafx.application.Application)
  (:require [clj-grapher.gui.application :refer [start]])
  (:import [javafx.application Application]))

(defn -start [this stage]
  (start stage))

(defn launch-application [& args]
  (Application/launch (Class/forName "clj_grapher.gui.core")
                      (into-array String args)))
