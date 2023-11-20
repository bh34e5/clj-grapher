(ns clj-grapher.gui.application-new
  (:require
    [clj-grapher.gui.types-new :as types]
    [clj-grapher.gui.utils :as gutils])
  (:import
    [javafx.scene Scene]
    [javafx.scene.control Button CheckBox TextField]
    [javafx.scene.layout BorderPane HBox VBox]))

(defn initialize-application []
  (types/map->Application {:function nil
                           :flags {:show-mod-lines true
                                   :show-arg-lines true}
                           :config {:scale 250}
                           :event-system {}}))

(def ^{:dynamic true} *application*
  (ref (initialize-application)))

(defn add-event-listener! [target listener]
  (dosync (alter *application* types/add-event-listener! target listener)))

(defn notify [target & args]
  (apply types/notify @*application* target args))

(defmacro ^{:private true} ? [if-obj res]
  `(when-not (nil? ~if-obj) ~res))

(defmacro on-action
  [obj [this event] & body]
  `(.setOnAction ~obj
                 (reify
                   javafx.event.EventHandler
                   (handle [~this ~event] ~@body))))

(defn check-box
  ([options] (check-box nil (assoc options :check-box (CheckBox.))))
  ([label options]
   (let [{cb :check-box
          :keys [on-set-checked reset-checked! init]
          :or {cb (CheckBox. label)}} options]
     (letfn [(upd-checked [checked?]
               (.setSelected cb checked?))]
       (add-event-listener! reset-checked! upd-checked)
       (? on-set-checked
          (on-action cb
            [_ _]
            (notify on-set-checked (.isSelected cb))))
       (? init (.setSelected cb init))
       (? label (.setText cb label))
       cb))))

(defn h-box
  ([options] (h-box () options))
  ([children options]
   (let [{:keys [spacing] :or {spacing 0.0}} options]
     (HBox. spacing (apply gutils/node-arr children)))))

(defn v-box
  ([options] (v-box () options))
  ([children options]
   (let [{:keys [spacing] :or {spacing 0.0}} options]
     (VBox. spacing (apply gutils/node-arr children)))))

(defn border-pane [options]
  (let [{:keys [center top left bottom right]} options
        bp (BorderPane.)]
    (? center (.setCenter bp center))
    (? top (.setTop bp top))
    (? left (.setLeft bp left))
    (? bottom (.setBottom bp bottom))
    (? right (.setRight bp right))
    bp))

(defn text-field
  ([options] (text-field nil (assoc options :text-field (TextField.))))
  ([text options]
   (let [{tf :text-field
          :keys [prompt-text num-columns]
          :or {tf (TextField. text)}}
         options]
     (? prompt-text (.setPromptText tf prompt-text))
     (? num-columns (.setPrefColumnCount tf num-columns))
     tf)))

(defn button [text options]
  (let [{:keys [on-click]} options
        btn (Button. text)]
    (? on-click
       (on-action btn
         [_ _]
         (notify on-click)))
    btn))

(defn top-pane []
  (let [name-field (text-field "Input"
                               {:prompt-text "Put your text here"
                                :num-columns 15})
        top (h-box [name-field
                    (button "Click me!"
                            {:on-click ::test-btn})]
                   {:spacing 5.0})]
    (add-event-listener!
     ::test-btn
     (fn []
       (println "A button clicked!")
       (println "Field's text was..."
                (.getText name-field))))
    top))

(defn right-pane []
  (v-box [(check-box "ChBx1"
                     {:init true
                      :on-set-checked ::cb-one-upd
                      :reset-checked! ::cb-one-get})
          (check-box "ChBx2"
                     {:init false
                      :on-set-checked ::cb-two-upd
                      :reset-checked! ::cb-two-get})]
         {:spacing 10.0}))

(defn center-pane [])

(defn application-pane []
  (let []
    (border-pane {:center (center-pane)
                  :top (top-pane)
                  :right (right-pane)})))

(declare start)
(defonce stage (atom nil))
(defn- set-stage [cur-stage] (reset! stage cur-stage))

(def restart
  #(javafx.application.Platform/runLater
    (fn []
      (load-file "src/clojure/clj_grapher/gui/application_new.clj")
      (dosync (ref-set *application* (initialize-application)))
      (start @stage))))

(comment (restart))

(defn start [stage]
  (set-stage stage)
  (let [pane (application-pane)
        scene (Scene. pane 200 400)]
    (add-event-listener! ::cb-one-upd println)
    (doto stage
      (.setTitle "Complex Function Grapher - In Clojure")
      (.setScene scene)
      (.sizeToScene)
      (.show)))
  nil)
