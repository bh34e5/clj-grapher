(ns clj-grapher.gui.utils
  (:import [javafx.scene Node]
           [javafx.scene.control Alert ButtonType]
           [javafx.scene.text Font FontWeight]))

(def default-font-bold
  (let [default (Font/getDefault)]
    (Font/font (.getFamily default)
               FontWeight/BOLD
               (.getSize default))))

(defmacro initialize
  "Creates a new instance of `cls', binds that instance to `this', and then runs
  the initializers on that instance. Returns the instance."
  [cls [& args] & initializers]
  `(let [~'this (new ~cls ~@args)]
     (doto ~'this ~@initializers)))

(defn node-arr [& nodes]
  (into-array Node nodes))

;;; TODO: fill this in to alert the user their function is bad
(defn make-alert
  ([alert-type] (initialize Alert [alert-type]))
  ([alert-type content-text & buttons]
   (initialize Alert [alert-type content-text (into-array ButtonType
                                                          buttons)])))
(defn show-alert
  ([alert-type content-text & buttons]
   (let [alert (apply make-alert alert-type content-text buttons)]
     (.showAndWait alert))))
