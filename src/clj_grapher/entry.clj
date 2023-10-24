(ns clj-grapher.entry
  (:gen-class)
  (:require [clj-grapher.gui.application :refer [launch-application]]))

(defn -main [& args]
  (launch-application args))
