(ns clj-grapher.entry
  (:gen-class)
  (:require
    [clj-grapher.gui.core :refer [launch-application]]))

(defn -main [& args]
  (.start (Thread. #(launch-application args))))
