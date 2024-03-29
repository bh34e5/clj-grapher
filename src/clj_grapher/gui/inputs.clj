(ns clj-grapher.gui.inputs
  (:require
    [clj-grapher.math]
    [clj-grapher.gui.types :as types]
    [clj-grapher.gui.utils :as utils]
    [clj-utils.core :refer [noisy-let]])
  (:import
    [clj_grapher.math ComplexNumber]
    [java.util List]
    [java.util.function Predicate]
    [javafx.beans.value ChangeListener]
    [javafx.event EventHandler]
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
  {'+   'clj-grapher.math/c-add
   '-   'clj-grapher.math/c-sub
   '*   'clj-grapher.math/c-mult
   '/   'clj-grapher.math/c-div
   'exp 'clj-grapher.math/c-exp
   'sin 'clj-grapher.math/c-sin
   'cos 'clj-grapher.math/c-cos
   'tan 'clj-grapher.math/c-tan})

(defn- final-symbol-check
  [input]
  (cond
    (= 'z input) input
    (= 'Re input) #(ComplexNumber. (:real %) 0)
    (= 'Im input) #(ComplexNumber. (:imaginary %) 0)
    :else (throw (ex-info "Unexpected symbol in function" {:symbol input}))))

(defn- numerical-replacement
  [input]
  (if (number? input)
    (ComplexNumber. input 0)
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
                mid-step (cons `->complex-math
                               (list inner))
                parsed (eval mid-step)]
      (println parsed)
      parsed)
    (catch Exception e
      (println e))))

(defn make-function-panel [application]
  (let [field (utils/initialize TextField
                                [(or (get-in @application [:function :text])
                                     "")]
                (.setPromptText function-prompt-text))
        label-text (utils/initialize Text ["f(z) = "]
                     (.setFont utils/default-font-bold))
        label-flow (TextFlow. (utils/node-arr label-text))
        button (utils/initialize Button ["Graph"]
                 (.setOnAction
                  (reify
                    EventHandler
                    (handle [_ event]
                      (let [f-text (.getText field)
                            func (compile-function-text f-text)]
                        (dosync
                          (alter application
                                 types/set-function!
                                 {:text f-text :object func})))
                      (types/notify @application ::gui.app/update-function)))))]
    (HBox. 5.0 (utils/node-arr label-flow field button))))

(defn make-line-checkbox [application line-type]
  (utils/initialize CheckBox []
    (.setSelected (boolean (get @application line-type)))
    (.setOnAction
     (reify
       EventHandler
       (handle [_ event]
         (dosync
           (alter application
                  types/set-show-lines!
                  line-type
                  (.isSelected this)))
         (types/notify @application ::gui.app/update-line-type line-type))))))

(def ^{:private true} regex-pattern-map
  {::continuous [#"\d{1,3}(,(\d{3},)*\d{3}|(\d{3})*\d{3})?(\.\d*)?|\.\d+"
                 #(Double/parseDouble (if (= % "") "0" %))]
   ::discrete [#"\d{1,3}(,(\d{3},)*\d{3}|(\d{3})*\d{3})?"
               #(Long/parseLong (if (= % "") "0" %))]})

;;; TODO: think about the design of this. There seems to be a fundamental need
;;; of this to be stateful, which makes this a little bit harder to reason
;;; about...
(defn make-number-input
  ([initial-value set-value!]
   (make-number-input initial-value set-value! ::continuous))
  ([initial-value set-value! numeric-type]
   (make-number-input initial-value set-value! nil nil numeric-type))
  ([initial-value set-value! min-val max-val numeric-type]
   (let [[pattern parser] (get regex-pattern-map numeric-type)]
     (if pattern
       (utils/initialize TextField [(.toString initial-value)]
         (.. textProperty
             (addListener (reify
                            ChangeListener
                            (changed [_ observable old-value new-value]
                              ;;; TODO: add checking to the value if ranges
                              ;;; provided...
                              (println "Calling number input changed handler")
                              (if-not (re-matcher pattern new-value)
                                (throw (ex-info "Invalid input"
                                                {:value new-value}))
                                (set-value! (parser new-value))))))))
       (throw (ex-info "Invalid numeric type" {:type numeric-type}))))))

(defn make-scale-panel [application]
  (let [cur-scale (.scale @application)
        updated-scale (ref cur-scale)
        number-input (make-number-input
                      cur-scale
                      (fn [new-value]
                        (println "Got new value:" new-value)
                        (dosync (ref-set updated-scale new-value)))
                      ::discrete)
        grid-pane (GridPane.)
        edit-button (Button. "Edit Scale")
        save-button (Button. "Save Scale")
        scale-label (Label. (.toString cur-scale))]
    (.setOnAction edit-button
                  (reify
                    EventHandler
                    (handle [_ event]
                      (.. grid-pane
                          getChildren
                          (removeAll (List/of scale-label edit-button)))
                      (.add grid-pane number-input 0 0)
                      (.add grid-pane save-button 1 0))))
    (.setOnAction save-button
                  (reify
                    EventHandler
                    (handle [_ event]
                      (.. grid-pane
                          getChildren
                          (removeAll (List/of number-input save-button)))
                      (.setText scale-label (.toString @updated-scale))
                      (.add grid-pane scale-label 0 0)
                      (.add grid-pane edit-button 1 0)
                      (dosync
                        (alter application types/set-scale! @updated-scale))
                      (types/notify @application ::gui.app/update-scale))))
    (.addRow grid-pane 0 (utils/node-arr scale-label edit-button))
    grid-pane))

(defn make-selection-panel [application]
  (let [info-label-text (utils/initialize Text ["Select options for the graph"]
                          (.setFont utils/default-font-bold))
        info-label-flow (TextFlow. (utils/node-arr info-label-text))
        cb-mod (make-line-checkbox application :show-mod-lines)
        cb-arg (make-line-checkbox application :show-arg-lines)
        label-mod (Label. "Modulus Lines")
        label-arg (Label. "Argument Lines")
        scale-panel (make-scale-panel application)]
    (utils/initialize GridPane []
      (.setVgap 5.0)
      (.setHgap 10.0)
      (.add info-label-flow 0 0 2 1)
      (.addRow 1 (utils/node-arr cb-mod label-mod))
      (.addRow 2 (utils/node-arr cb-arg label-arg))
      (.add scale-panel 0 3 2 1))))
