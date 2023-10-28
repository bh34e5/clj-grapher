(ns clj-grapher.gui.inputs
  (:require [clj-utils.core :refer [ensure-seq noisy-let]]
            [clj-grapher.gui.utils
             :refer
             [default-font-bold initialize node-arr]]
            [clj-grapher.gui.types
             :refer
             [notify set-function! set-show-lines!]])
  (:import [javafx.event EventHandler]
           [javafx.scene.control Button CheckBox Label TextField]
           [javafx.scene.layout GridPane HBox]
           [javafx.scene.text Text TextFlow]))

(alias 'gui.app 'clj-grapher.gui.application)

(def ^{:private true} function-prompt-text "(+ 2 z)")

;;; TODO: Need to figure out what I want to do with this. Right now, I don't
;;; think I have the ability to use arithmetic functions because to do so would
;;; require having access to c-add, etc... But in the future, I want to parse
;;; anyway, so when I get to that point, that is when I can conver them into the
;;; correct multiplication/addition, etc...
;;; And right now, because I can access `:real' and `:imaginary', this is less
;;; of a problem than I thought. I just have to do a little bit more work in
;;; testing... no big deal there though.
(defmacro ->complex-math
  [func-text]
  `(fn [~'z]
     ~func-text))

(defn compile-function-text [text]
  (try
    ;;; TODO: compare read-string to load-string
    (noisy-let [inner (read-string text)
                mid-step (cons '->complex-math (ensure-seq inner))
                parsed (eval mid-step)]
      (println parsed)
      parsed)
    (catch Exception e
      (println e))))

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
                      (let [f-text (.getText field)
                            func (compile-function-text f-text)]
                        (set-function! application func))
                      (notify application ::gui.app/update-function)))))]
    (HBox. 5.0 (node-arr label-flow field button))))

(defn make-line-checkbox [application line-type]
  (initialize CheckBox []
    (.setSelected true)
    (.setOnAction
     (reify
       EventHandler
       (handle [_ event]
         (set-show-lines! application line-type (.isSelected this))
         (notify application ::gui.app/update-line-type line-type))))))

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
