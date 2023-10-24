(ns clj-grapher.gui.core
  (:require [clj-grapher.gui.utils :refer [initialize]])
  (:import [javafx.event ActionEvent EventHandler]
           [javafx.scene Node Scene]
           [javafx.scene.control Button Label TextField]
           [javafx.scene.layout HBox]
           [javafx.stage Stage]))

(def ^{:private true} function-prompt-text "(+ 2 z)")

(defonce ^{:private true} initial-appliction-map
  {:function nil})

(defn initialize-application []
  (ref initial-appliction-map))

(defn set-function-text! [application text]
  (dosync
    (alter application assoc :function text)))

(defn make-function-panel [application]
  (let [label (Label. "f(z) = ")
        field (initialize TextField []
                (.setPromptText function-prompt-text))
        button (initialize Button ["Graph"]
                  (.setOnAction
                   (reify
                     EventHandler
                     (handle [this event]
                       (set-function-text! application (.getText field))))))]
    (HBox. 5.0 (into-array Node (list label field button)))))

(defn start [stage]
  (let [application (initialize-application)
        pane (make-function-panel application)
        scene (Scene. pane 600 300)]
    (doto stage
      (.setScene scene)
      (.show))))
