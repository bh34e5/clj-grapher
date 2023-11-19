(ns clj-grapher.gui.application-new
  (:require
    [clj-grapher.gui.types :as types])
  (:import
    [javafx.scene Scene]
    [javafx.scene.control CheckBox]
    [javafx.scene.layout VBox]))

(defn initialize-application []
  (ref (types/map->Application {:function nil
                                :show-mod-lines true
                                :show-arg-lines true
                                :scale 250
                                :event-system {}})))

(def ^{:dynamic true} *application*
  (initialize-application))

(defmacro ^{:private true} ? [if-obj res]
  `(when-not (nil? ~if-obj) ~res))

(defmacro on-action [obj [this event] & body]
  `(.setOnAction ~obj
                 (reify
                   javafx.event.EventHandler
                   (handle [~this ~event] ~@body))))

(defn check-box [checked?
                 on-upd
                 get-upd
                 & {:keys [label label-pos]
                    :or {}}]
  (let [cb (CheckBox.)]
    (letfn [(upd-checked [checked?]
              (.setSelected cb checked?))]
      (upd-checked checked?)
      (types/add-event-listener! @*application* get-upd upd-checked)
      (on-action cb
        [_ _]
        (types/notify @*application* on-upd (.getSelected cb)))
      (? label (.setText cb label))
      cb)))

(defn v-box [[& children] & {:keys [v-gap] :or {v-gap 5.0}}]
  (let [box (VBox.)]
    (? v-gap (.setSpacing box v-gap))
    (? children
       (let [children-ol (.getChildren box)]
         (.setAll children-ol children)))
    box))

(defn start [stage]
  (let [pane (v-box [(check-box true ::cb-one-set ::cb-one-get :label "ChBx1")
                     (check-box false ::cb-two-set ::cb-two-get :label "ChBx2")]
                    {:v-gap 10.0})
        scene (Scene. pane 100 200)]
    (doto stage
      (.setTitle "Complex Function Grapher - In Clojure")
      (.setScene scene)
      (.show)))
  nil)
