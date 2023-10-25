(ns clj-grapher.gui.utils
  (:import [javafx.scene Node]))

(defmacro initialize
  "Creates a new instance of `cls', binds that instance to `this', and then runs
  the initializers on that instance. Returns the instance."
  [cls [& args] & initializers]
  `(let [~'this (new ~cls ~@args)]
     (doto ~'this ~@initializers)))

(defn node-arr [& nodes]
  (into-array Node nodes))
