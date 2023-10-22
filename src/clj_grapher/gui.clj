(ns clj-grapher.gui
  (:import [javafx.scene Node Scene]
           [javafx.scene.control Label]
           [javafx.scene.layout StackPane]
           [javafx.stage Stage]))

(defn start [stage]
  (let [label (Label. "hello world")
        pane (StackPane. (into-array Node (list label)))
        scene (Scene. pane 600 300)]
    (doto stage
      (.setScene scene)
      (.show))))
