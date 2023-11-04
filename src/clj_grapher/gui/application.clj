(ns clj-grapher.gui.application
  (:require [clj-grapher.gui.graph :refer [make-graph-panel]]
            [clj-grapher.gui.inputs
             :refer [make-function-panel make-selection-panel]]
            [clj-grapher.gui.types
             :refer [deregister-all-event-listeners!
                     register-event-listener!
                     map->Application notify]]
            [clj-grapher.gui.utils :refer [initialize node-arr]])
  (:import [javafx.event EventHandler]
           [javafx.scene Scene]
           [javafx.scene.control Button]
           [javafx.scene.layout BorderPane HBox]))

(defn initialize-application []
  (ref (map->Application {:function nil
                          :show-mod-lines true
                          :show-arg-lines true
                          :scale 250
                          :event-system {}})))

(defn restart-application
  ([application] (restart-application application nil))
  ([application complete] (notify application ::restart-application complete)))

(defn make-restart-button
  ([application] (make-restart-button application {:complete false}))
  ([application {complete :complete :or {complete false}}]
   (let [msg (if complete "click to restart completely" "reset gui")]
     (initialize Button [msg]
       (.setOnAction
         (reify
           EventHandler
           (handle [_ event]
             (apply restart-application (list application complete)))))))))

(defn make-application-panel [application]
  (let [fn-panel (make-function-panel application)
        graph-panel (make-graph-panel application)
        selection-panel (make-selection-panel application)
        restart-pane (HBox. (node-arr (make-restart-button application {})
                                      (make-restart-button application
                                                           {:complete true})))]
    (initialize BorderPane []
      (.setCenter graph-panel)
      (.setTop fn-panel)
      (.setRight selection-panel)
      (.setBottom restart-pane))))

(def scene-width 600)
(def scene-height 400)

(defonce app-vars (ref {}))

(declare start) ;; forward declaration
(defn get-restart-handler
  [application stage]
  (fn [& args]
    (let [complete (first args)
          e-sys (:event-system @application)
          events (keys e-sys)
          not-restart (remove #(= % ::restart-application) events)]
      (apply deregister-all-event-listeners!
             application
             (if complete events not-restart))
      (println not-restart)
      (println "Args:" args)
      (if complete
        (do
          (dosync (alter app-vars dissoc :restart-id))
          (start stage)) ;; complete restart
        (start stage application)))))

(defn start
  ([stage] (start stage (initialize-application)))
  ([stage application]
   (let [pane (make-application-panel application)
         scene (Scene. pane scene-width scene-height)]
     (dosync
       (alter app-vars assoc :application application :stage stage))
     (when-not (get @app-vars :restart-id nil)
       (let [restart-handler (get-restart-handler application stage)
             handler-id (register-event-listener! application
                                                  ::restart-application
                                                  restart-handler)]
         (dosync
           (alter app-vars assoc :restart-id handler-id))))
     (doto stage
       (.setTitle "Complex Function Grapher - In Clojure")
       (.setScene scene)
       (.show)))))
