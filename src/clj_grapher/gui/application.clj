(ns clj-grapher.gui.application
  (:require [clj-grapher.gui.utils :refer [initialize node-arr]]
            [clj-grapher.gui.inputs
             :refer
             [make-function-panel make-selection-panel]]
            [clj-grapher.math
             :refer
             [->ComplexNumber Zero calculate-rectangle get-color-type]]
            [clj-grapher.gui.types
             :refer
             [register-event-listener! map->Application notify]])
  (:import [javafx.event EventHandler]
           [javafx.scene Scene]
           [javafx.scene.canvas Canvas]
           [javafx.scene.control Button]
           [javafx.scene.layout BorderPane StackPane]
           [javafx.scene.paint Color]))

(defn initialize-application []
  (ref (map->Application {:function nil
                          :show-mod-lines true
                          :show-arg-lines true
                          :event-system {}})))

;;; TODO: fill this in to alert the user their function is bad
(defn show-alert [] nil)

(defn color-context
  [context result-mat x-start y-start]
  (loop [mat-view result-mat
         y-ind y-start]
    (let [row (first mat-view)]
      (when row
        (loop [row-view row
               x-ind x-start]
          (let [pixel (first row-view)]
            (when pixel
              (doto context
                (.setFill (Color/rgb (int (:red pixel))
                                     (int (:green pixel))
                                     (int (:blue pixel))
                                     (:alpha pixel)))
                (.fillRect x-ind y-ind 1 1))
              (recur (rest row-view) (inc x-ind)))))
        (recur (rest mat-view) (inc y-ind))))))

(defn make-graph-panel [application]
  (let [width 250
        height 250
        half-width (/ width 2)
        half-height (/ height 2)
        canvas (Canvas. width height)
        context (.getGraphicsContext2D canvas)]
    (doto context
      (.setFill Color/BLUE)
      (.fillRect 0 0 width height))
    (letfn [(handle-update-line-type [line-type]
              (fn [line-type]
                (println "Got change in " line-type
                         ". Current application " @application)))
            (handle-update-function []
              (println "Got change in function. Current application"
                       @application)
              (let [input-fn (:function @application)
                    color-type (get-color-type (:show-mod-lines @application)
                                               (:show-arg-lines @application))]
                (if input-fn
                  (let [res (calculate-rectangle
                             input-fn
                             color-type
                             (->ComplexNumber (- half-width) (- half-height))
                             width
                             height
                             1)]
                    (.clearRect context
                                0 0
                                width height)
                    ;;; FIXME: this is drawing top down
                    (color-context context res 0 0))
                  (show-alert))))]
      (register-event-listener! application
                                ::update-line-type
                                handle-update-line-type)
      (register-event-listener! application
                                ::update-function
                                handle-update-function))
    (StackPane. (node-arr canvas))))

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

(defonce app-vars (ref {}))

(defn start [stage]
  (let [application (initialize-application)
        pane (make-application-panel application)
        scene (Scene. pane 600 300)]
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
      (.setScene scene)
      (.show))))
