(ns clj-grapher.gui.inputs
  (:require [clj-grapher.math :refer [->ComplexNumber]]
            [clj-grapher.gui.types
             :refer [notify set-function! set-show-lines!]]
            [clj-grapher.gui.utils
             :refer [default-font-bold initialize node-arr]]
            [clj-utils.core :refer [ensure-seq noisy-let]])
  (:import [javafx.event EventHandler]
           [javafx.scene.control Button CheckBox Label TextField]
           [javafx.scene.layout GridPane HBox]
           [javafx.scene.text Text TextFlow]))

(alias 'gui.app 'clj-grapher.gui.application)

(def ^{:private true} function-prompt-text "(+ 2 z)")

;;; TODO: Here is a base implementation that allows me to convert +, -, *, and /
;;; into the corresponding "complex" variants. Future optimizations include
;;; shuffling the values so that constants can be grouped and combined, and
;;; other things that I come up with later :)
;;; This is also still just a partial implementation as it still requires the
;;; user to input the function in prefix notation instead of the familiar in-fix
;;; notation.
(def ^{:private true} allowed-fns-to-counterparts
  {'+ 'clj-grapher.math/c-add
   '- 'clj-grapher.math/c-sub
   '* 'clj-grapher.math/c-mult
   '/ 'clj-grapher.math/c-div})

(defn- final-symbol-check
  [input]
  (if-not (= 'z input)
    (throw (ex-info "Unexpected symbol in function" {:symbol input}))
    input))

(defn- numerical-replacement
  [input]
  (if (number? input)
    (->ComplexNumber input 0)
    (if (= 'i input)
      clj-grapher.math/I
      (final-symbol-check input))))

(defn- code-walker
  [input]
  (if (seq? input)
    (map code-walker input)
    (if-let [replacement (get allowed-fns-to-counterparts input)]
      replacement
      (numerical-replacement input))))

(defmacro ->complex-math
  [func-text]
  `(fn [~'z]
     ~(code-walker func-text)))

(defn compile-function-text [text]
  (try
    (noisy-let [inner (read-string text)
                mid-step (cons 'clj-grapher.gui.inputs/->complex-math
                               (list inner))
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
