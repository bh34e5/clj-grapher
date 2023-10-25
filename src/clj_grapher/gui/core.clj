(ns clj-grapher.gui.core
  (:require [clj-grapher.gui.utils :refer [initialize node-arr]]
            [clj-grapher.math :refer [Zero calculate-rectangle]])
  (:import [javafx.event ActionEvent EventHandler]
           [javafx.scene Node Scene]
           [javafx.scene.canvas Canvas]
           [javafx.scene.control Button CheckBox Label TextField]
           [javafx.scene.layout BorderPane GridPane HBox StackPane]
           [javafx.scene.text Font FontWeight Text TextFlow]
           [javafx.stage Stage]))

(def ^{:private true} function-prompt-text "(+ 2 z)")

(def default-font-bold
  (let [default (Font/getDefault)]
    (Font/font (.getFamily default)
               FontWeight/BOLD
               (.getSize default))))

(defrecord Application [function show-mod-lines show-arg-lines event-system])

(defn initialize-application []
  (ref (map->Application {:function nil
                          :show-mod-lines true
                          :show-arg-lines true
                          :event-system {}})))

(defn set-function-text! [application text]
  (dosync
    (alter application assoc :function text)))

(defn set-show-lines! [application line-type show?]
  (dosync
    (alter application assoc line-type show?)))

(defn register-event-listener!
  [application event-name listener]
  (dosync
    (let [id (gensym)
          modify (fn [application]
                   (let [e-sys (:event-system application)
                         found-or-map (get e-sys event-name {})
                         upd-listener (assoc found-or-map id listener)
                         upd (assoc e-sys event-name upd-listener)]
                     (assoc application :event-system upd)))]
      (alter application modify)
      id)))

(defn deregister-event-listener!
  [application event-name id]
  (dosync
    (let [modify (fn [application]
                   (let [cur (get (:event-system application) event-name)
                         upd (dissoc cur id)]
                     (assoc application :event-system upd)))]
      (alter application modify))))

(defn notify [application event-name & args]
  (doseq [listener (vals (get (:event-system @application) event-name))]
    (apply listener args)))

;;; TODO: Need to figure out what I want to do with this. Right now, I don't
;;; think I have the ability to use arithmetic functions because to do so would
;;; require having access to c-add, etc... But in the future, I want to parse
;;; anyway, so when I get to that point, that is when I can conver them into the
;;; correct multiplication/addition, etc...
;;; And right now, because I can access `:real' and `:imaginary', this is less
;;; of a problem than I thought. I just have to do a little bit more work in
;;; testing... no big deal there though.
(defn compile-function-text [text]
  (try
    (let [wrapped (str "(fn [z] " text ")")
          parsed (load-string wrapped)]
      parsed)
    (catch Exception e
      nil)))

;;; TODO: fill this in to alert the user their function is bad
(defn show-alert [] nil)

(defn make-function-panel [application]
  (let [field (initialize TextField []
                (.setPromptText function-prompt-text))
        label-text (initialize Text ["f(z) = "]
                     (.setFont default-font-bold))
        label-flow (TextFlow. (node-arr label-text))
        button (initialize Button ["Graph"]
                 (.setOnAction
                  (reify
                    EventHandler
                    (handle [_ event]
                      (set-function-text! application (.getText field))
                      (notify application ::update-function)))))]
    (HBox. 5.0 (node-arr label-flow field button))))

(defn make-graph-panel [application]
  (let [canvas (Canvas. 50 50)
        context (.getGraphicsContext2D canvas)]
    (doto context
      (.setFill javafx.scene.paint.Color/BLUE)
      (.fillRect 0 0 (.getWidth canvas) (.getHeight canvas)))
    (letfn [(handle-update-line-type [line-type]
              (fn [line-type]
                (println "Got change in " line-type
                         ". Current application " @application)))
            (handle-update-function []
              (println "Got change in function. Current application"
                       @application)
              (let [input-fn (compile-function-text (:function @application))]
                (if (input-fn)
                  (calculate-rectangle input-fn Zero 1 1 0.2)
                  (show-alert))))]
      (register-event-listener! application
                                ::update-line-type
                                handle-update-line-type)
      (register-event-listener! application
                                ::update-function
                                handle-update-function))
    (StackPane. (node-arr canvas))))

(defn make-line-checkbox [application line-type]
  (initialize CheckBox []
    (.setSelected true)
    (.setOnAction
     (reify
       EventHandler
       (handle [_ event]
         (set-show-lines! application line-type (.isSelected this))
         (notify application ::update-line-type line-type))))))

(defn make-selection-panel [application]
  (let [info-label-text (initialize Text ["Select options for the graph"]
                          (.setFont default-font-bold))
        info-label-flow (TextFlow. (node-arr info-label-text))
        cb-mod (make-line-checkbox application :show-mod-lines)
        cb-arg (make-line-checkbox application :show-arg-lines)
        label-mod (Label. "Modulus Lines")
        label-arg (Label. "Argument Lines")]
    (initialize GridPane []
      (.setVgap 5.0)
      (.setHgap 10.0)
      (.add info-label-flow 0 0 2 1)
      (.addRow 1 (node-arr cb-mod label-mod))
      (.addRow 2 (node-arr cb-arg label-arg)))))

(defn make-application-panel [application]
  (let [fn-panel (make-function-panel application)
        graph-panel (make-graph-panel application)
        selection-panel (make-selection-panel application)]
    (initialize BorderPane []
      (.setCenter graph-panel)
      (.setTop fn-panel)
      (.setRight selection-panel))))

(defn start [stage]
  (let [application (initialize-application)
        pane (make-application-panel application)
        scene (Scene. pane 600 300)]
    (doto stage
      (.setScene scene)
      (.show))))
