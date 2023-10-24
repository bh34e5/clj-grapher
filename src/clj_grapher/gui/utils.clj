(ns clj-grapher.gui.utils)

(defmacro initialize
  "Creates a new instance of `cls', and then runs the initializers on that
  instance."
  [cls [& args] & initializers]
  `(let [instance# (new ~cls ~@args)]
     (doto instance# ~@initializers)))
