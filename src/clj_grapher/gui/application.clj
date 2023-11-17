(ns clj-grapher.gui.application
  (:require
    [clj-grapher.gui.graph :as graph]
    [clj-grapher.gui.inputs :as inputs]
    [clj-grapher.gui.types :as types]
    [clj-grapher.gui.utils :as utils])
  (:import
    [javafx.application Platform]
    [javafx.event EventHandler]
    [javafx.scene Scene]
    [javafx.scene.control Button]
    [javafx.scene.layout BorderPane HBox]))

(defn initialize-application []
  (ref (types/map->Application {:function nil
                                :show-mod-lines true
                                :show-arg-lines true
                                :scale 250
                                :event-system {}})))

(defn restart-application
  ([application] (restart-application application nil))
  ([application complete]
   (types/notify application ::restart-application complete)))

(defn make-restart-button
  ([application] (make-restart-button application {:complete false}))
  ([application {complete :complete :or {complete false}}]
   (let [msg (if complete "click to restart completely" "reset gui")]
     (utils/initialize Button [msg]
       (.setOnAction
         (reify
           EventHandler
           (handle [_ event]
             (apply restart-application (list @application complete)))))))))

(defn make-application-panel [application]
  (let [fn-panel (inputs/make-function-panel application)
        graph-panel (graph/make-graph-panel application)
        selection-panel (inputs/make-selection-panel application)
        restart-pane (HBox. (utils/node-arr
                             (make-restart-button application {})
                             (make-restart-button application
                                                  {:complete true})))]
    (utils/initialize BorderPane []
      (.setCenter graph-panel)
      (.setTop fn-panel)
      (.setRight selection-panel)
      (.setBottom restart-pane))))

(def scene-width 900)
(def scene-height 600)

(defonce app-vars (ref {}))

(declare start) ;; forward declaration
(defn get-restart-handler
  [application stage]
  (fn [& args]
    (let [complete (first args)
          e-sys (.event-system @application)
          events (keys e-sys)
          not-restart (remove #(= % ::restart-application) events)]
      (dosync
        (alter application types/drop-events (if complete events not-restart)))
      (println not-restart)
      (println "Args:" args)
      (Platform/runLater
       #(if complete
          (do
            (dosync (alter app-vars dissoc :restart-fn))
            (start stage)) ;; complete restart
          (start stage application))))))

;;; TODO: investigate the app not closing completely? I thought
;;;       I fixed that one, but it may not actually be done...

(defn start
  ([stage] (start stage (initialize-application)))
  ([stage application]
   (let [pane (make-application-panel application)
         scene (Scene. pane scene-width scene-height)]
     (dosync
       (alter app-vars assoc :application application :stage stage))
     (when-not (get @app-vars :restart-fn nil)
       (let [restart-handler (get-restart-handler application stage)]
         (dosync
           (alter application
                  types/add-event-listener!
                  ::restart-application
                  restart-handler)
           (alter app-vars assoc :restart-fn restart-handler))))
     (doto stage
       (.setTitle "Complex Function Grapher - In Clojure")
       (.setScene scene)
       (.show)))))
