(ns clj-grapher.math
  (:require [clojure.math :as math]
            [clojure.core.async :as async :refer [<!! <! >!! >!]]
            [clj-grapher.color
             :as color
             :refer [make-color composite composite*]]
            [clj-utils.core :refer [ecase]]))

(defn frac [n]
  (- n (math/floor n)))

(defrecord ComplexNumber [real imaginary])

(defn c-abs [^ComplexNumber c]
  (math/hypot (:real c) (:imaginary c)))

(defn c-arg [^ComplexNumber c]
  (math/atan2 (:imaginary c) (:real c)))

;; defining a base color for less duplication
(def ^{:private true} base-color (make-color 128 128 128 0))

(defn- get-abs-shade [^ComplexNumber c]
  (let [a (c-abs c)
        frac-a (frac a)]
    (assoc base-color :alpha frac-a)))

(defn- get-arg-shade [^ComplexNumber c]
  (let [a (c-arg c)
        frac-a (/ (+ math/PI a)
                  (* 2 math/PI))]
    (assoc base-color :alpha frac-a)))

;;; TODO: figure out how I want to map from the magnitude of the complex number
;;; to the lightness of the color... I think it might turn into something like
;;; the (/ (math/atan n) math/PI) so that n -> infty :: lightness -> 1
(defn- get-lightness [n]
  (/ n 10))

(defn complex-number->color [^ComplexNumber c]
  (let [argument (c-arg c)
        hue (/ (+ argument math/PI)
               (* 2 math/PI))
        saturation 0.5
        lightness (get-lightness (c-abs c))]
    (make-color hue saturation lightness 1.0 :color/hsla)))

(defn get-color-from-result
  ([^ComplexNumber c] (get-color-from-result c :abs-and-arg))
  ([^ComplexNumber c kwd]
   (let [res-color (complex-number->color c)]
     (ecase kwd
       ::none res-color
       ::abs-only (composite res-color (get-abs-shade c))
       ::arg-only (composite res-color (get-arg-shade c))
       ::abs-and-arg (composite* res-color
                                 (get-abs-shade c)
                                 (get-arg-shade c))))))
