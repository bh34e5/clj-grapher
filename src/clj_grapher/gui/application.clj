(ns clj-grapher.gui.application
  (:gen-class :extends javafx.application.Application)
  (:require [clj-grapher.gui.new_application :refer [start]])
  (:import [javafx.application Application]))

(defn -start [this stage]
  (start stage))

(defn launch-application [& args]
  (Application/launch (Class/forName "clj_grapher.gui.application")
                      (into-array String args)))
