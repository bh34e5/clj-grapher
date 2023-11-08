(ns clj-grapher.gui.graph
  (:require
    [clj-grapher.math :as math]
    [clj-grapher.gui.types :as types]
    [clj-grapher.gui.utils :as utils])
  (:import
    [clj_grapher.math ComplexNumber]
    [javafx.geometry Pos]
    [javafx.scene.canvas Canvas]
    [javafx.scene.control Alert Alert$AlertType ButtonType]
    [javafx.scene.layout GridPane StackPane]
    [javafx.scene.paint Color]
    [javafx.scene.shape Line Shape]))

(alias 'gui.app 'clj-grapher.gui.application)

(def^{:private true} dash-height 10) ;; default height of dashes, 10 pixels

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

(defn- get-axis-mappings
  [direction]
  (let [direction-coords (clj-utils.core/ecase direction
                           ::hor {:with-axis :x :against-axis :y}
                           ::ver {:with-axis :y :against-axis :x})
        {with-axis :with-axis against-axis :against-axis} direction-coords
        to-name (fn [prefix axis] (keyword (str prefix (name axis))))
        start-with    (to-name "start-" with-axis)
        start-against (to-name "start-" against-axis)
        end-with      (to-name "end-"   with-axis)
        end-against   (to-name "end-"   against-axis)]
    [start-with start-against end-with end-against]))

(defn- ruled-line-dash
  [direction x y height]
  (let [half-height (/ height 2)
        [s-with s-against e-with e-against] (get-axis-mappings direction)
        points (hash-map s-with    x
                         s-against (- y half-height)
                         e-with    x
                         e-against (+ y half-height))]
    (utils/initialize Line [(:start-x points)
                            (:start-y points)
                            (:end-x   points)
                            (:end-y   points)])))

(defn- to-pixel-dim
  [ind step offset ratio]
  (let [number-space-off (+ offset (* ind step))
        pixel-space-off (* number-space-off ratio)]
    pixel-space-off))

(defn make-ruled-line
  [direction start-x start-y length min-val max-val step offset]
  (let [ratio (/ length (- max-val min-val))
        end-x (+ start-x length)
        end-y start-y
        ;;; TODO: investigate this 1+, decide wether it's necessary,
        ;;;       if if I just off-ed (by one) myself
        num-rules (clojure.math/floor (+ 1 (/ (- max-val offset min-val) step)))
        rules (map #(ruled-line-dash direction
                                     (to-pixel-dim %1 step offset ratio)
                                     start-y
                                     dash-height)
                   (range num-rules))
        [s-with s-against e-with e-against] (get-axis-mappings direction)
        points (hash-map s-with    start-x
                         s-against start-y
                         e-with    end-x
                         e-against end-y)
        main-line (utils/initialize Line [(:start-x points)
                                          (:start-y points)
                                          (:end-x   points)
                                          (:end-y   points)])]
    (reduce #(Shape/union %1 %2) (concat rules (list main-line)))))

(defn make-axis-pane
  [application direction length min-val max-val step]
  ;;; TODO: this will interact with registered listener
  (let [axis-pane (make-ruled-line direction
                                   0
                                   (/ dash-height 2)
                                   length
                                   min-val
                                   max-val
                                   step
                                   0)]
    ;;; TODO: add a scroll listener... not sure how I want to do that.
    ;;;       I don't know if I need to change this to be a class so
    ;;;       that when scrolling I can just change the shapes? Otherwise,
    ;;;       I have to create a new pane every scroll event... which
    ;;;       seems like that would be expensive.
    axis-pane))

(defn make-graph-panel [application]
  (let [width 250
        height 250
        half-width (/ width 2)
        half-height (/ height 2)
        canvas (Canvas. width height)
        context (.getGraphicsContext2D canvas)
        h-axis (make-axis-pane application
                               ::hor
                               width
                               (- half-width)
                               half-width
                               25)
        v-axis (make-axis-pane application
                               ::ver
                               height
                               (- half-height)
                               half-height
                               25)
        border-pane (utils/initialize GridPane []
                      (.add canvas 0 0)
                      (.add v-axis 1 0)
                      (.add h-axis 0 1)
                      (.setGridLinesVisible false) ;; TODO: remove after debug
                      ;; This alignment might mean that the StackPane is not
                      ;; necessary... because it seems to be the alignment of
                      ;; the actual grid, not the items in the grid
                      (.setAlignment Pos/CENTER)
                      (.setHgap 10.0)
                      (.setVgap 10.0))]
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
                    color-type (math/get-color-type
                                (:show-mod-lines @application)
                                (:show-arg-lines @application))]
                (if input-fn
                  (let [res (math/calculate-rectangle
                             input-fn
                             color-type
                             (:scale @application)
                             (ComplexNumber. (- half-width) (- half-height))
                             width
                             height
                             1)]
                    (.clearRect context
                                0 0
                                width height)
                    ;;; FIXME: this is drawing top down
                    (color-context context res 0 0))
                  (utils/show-alert Alert$AlertType/ERROR
                                    "Invalid function supplied"
                                    ButtonType/OK))))]
      (types/register-event-listener! application
                                      ::gui.app/update-line-type
                                      handle-update-line-type)
      (types/register-event-listener! application
                                      ::gui.app/update-function
                                      handle-update-function)
      ;; call the function to graph initially, if the function exists
      (when (:function @application) (handle-update-function)))
    (StackPane. (utils/node-arr border-pane))))
