(ns clj-grapher.gui.application
  (:require [clj-grapher.gui.graph :refer [make-graph-panel]]
            [clj-grapher.gui.inputs
             :refer [make-function-panel make-selection-panel]]
            [clj-grapher.gui.types
             :refer [register-event-listener! map->Application notify]]
            [clj-grapher.gui.utils :refer [initialize node-arr]])
  (:import [javafx.event EventHandler]
           [javafx.scene Scene]
           [javafx.scene.control Button]
           [javafx.scene.layout BorderPane StackPane]))

(defn initialize-application []
  (ref (map->Application {:function nil
                          :show-mod-lines true
                          :show-arg-lines true
                          :event-system {}})))

(defn restart-application [application]
  (notify application ::restart-application))

(defn make-restart-button [application]
  (initialize Button ["click to restart"]
    (.setOnAction
      (reify
        EventHandler
        (handle [_ this]
          (restart-application application))))))

(defn make-application-panel [application]
  (let [fn-panel (make-function-panel application)
        graph-panel (make-graph-panel application)
        selection-panel (make-selection-panel application)
        restart-pane (StackPane. (node-arr (make-restart-button application)))]
    (initialize BorderPane []
      (.setCenter graph-panel)
      (.setTop fn-panel)
      (.setRight selection-panel)
      (.setBottom restart-pane))))

(def scene-width 600)
(def scene-height 400)

(defonce app-vars (ref {}))

(defn start [stage]
  (let [application (initialize-application)
        pane (make-application-panel application)
        scene (Scene. pane scene-width scene-height)]
    (dosync
      (alter app-vars assoc :application application :stage stage))
    (when-not (get @application :restart-id nil)
      (let [restart-handler (fn []
                              (start stage))
            handler-id (register-event-listener! application
                                                 ::restart-application
                                                 restart-handler)]
        (dosync
          (alter app-vars assoc :restart-id handler-id))))
    (doto stage
      (.setTitle "Complex Function Grapher - In Clojure")
      (.setScene scene)
      (.show))))
