(ns clj-grapher.gui.graph
  (:require [clj-grapher.math
             :refer [->ComplexNumber calculate-rectangle get-color-type]]
            [clj-grapher.gui.types :refer [register-event-listener!]]
            [clj-grapher.gui.utils :refer [node-arr show-alert]])
  (:import [javafx.scene.canvas Canvas]
           [javafx.scene.control Alert Alert$AlertType ButtonType]
           [javafx.scene.layout StackPane]
           [javafx.scene.paint Color]))

(alias 'gui.app 'clj-grapher.gui.application)

(defn color-context
  [context result-mat x-start y-start]
  (loop [result-mat result-mat
         y-ind y-start]
    (when (seq result-mat)
      (let [row (first result-mat)]
        (loop [row row
               x-ind x-start]
          (when (seq row)
            (let [pixel (first row)]
              (doto context
                (.setFill (Color/rgb (int (:red pixel))
                                     (int (:green pixel))
                                     (int (:blue pixel))
                                     (:alpha pixel)))
                (.fillRect x-ind y-ind 1 1)))
            (recur (rest row) (inc x-ind)))))
      (recur (rest result-mat) (inc y-ind)))))

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
              (let [input-fn (get-in @application [:function :object])
                    color-type (get-color-type (:show-mod-lines @application)
                                               (:show-arg-lines @application))]
                (if input-fn
                  (let [res (calculate-rectangle
                             input-fn
                             color-type
                             (:scale @application)
                             (->ComplexNumber (- half-width) (- half-height))
                             width
                             height
                             1)]
                    (.clearRect context
                                0 0
                                width height)
                    ;;; FIXME: this is drawing top down
                    (color-context context res 0 0))
                  (show-alert Alert$AlertType/ERROR
                              "Invalid function supplied"
                              ButtonType/OK))))]
      (register-event-listener! application
                                ::gui.app/update-line-type
                                handle-update-line-type)
      (register-event-listener! application
                                ::gui.app/update-function
                                handle-update-function)
      ;; call the function to graph initially, if the function exists
      (when (:function @application) (handle-update-function)))
    (StackPane. (node-arr canvas))))
