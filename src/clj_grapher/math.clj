(ns clj-grapher.math
  (:require [clojure.math :as math]
            [clojure.core.async :as async :refer [<!! <! >!! >!]]
            [clj-grapher.color :refer [make-color composite composite*]]
            [clj-utils.core :refer [ecase]]))

(defn frac [n]
  (- n (math/floor n)))

(defrecord ComplexNumber [real imaginary])
(def Zero (->ComplexNumber 0 0))
(def One (->ComplexNumber 1 0))
(def I (->ComplexNumber 0 1))

(defn c-abs [^ComplexNumber c]
  (math/hypot (:real c) (:imaginary c)))

(defn c-arg [^ComplexNumber c]
  (math/atan2 (:imaginary c) (:real c)))

;;; FIXME: all of these `apply reduce's are broken :)
(defn c-eql
  ([c] true)
  ([c d] (and (== (:real c) (:real d))
              (== (:imaginary c) (:imaginary d))))
  ([c d & others]
   (apply reduce c-eql c d others)))

(defn c-add
  ([] Zero)
  ([c] c)
  ([c d] (->ComplexNumber (+ (:real c) (:real d))
                          (+ (:imaginary c) (:imaginary d))))
  ([c d & others]
   (apply reduce c-add c d others)))

(defn c-const-mult
  ([] One)
  ([n c] (->ComplexNumber (* n (:real c)) (* n (:imaginary c)))))

(defn c-mult
  ([] One)
  ([c] c)
  ([c d] (->ComplexNumber (- (* (:real c) (:real d))
                             (* (:imaginary c) (:imaginary d)))
                          (+ (* (:real c) (:imaginary d))
                             (* (:imaginary c) (:real d)))))
  ([c d & others]
   (apply reduce c-mult c d others)))

;; defining a base color for less duplication
(def ^{:private true} base-color (make-color 0 0 0 1.0))
(def ^{:private true} max-shade (/ 1 2))

(defn- get-abs-shade [^ComplexNumber c]
  (if (c-eql c Zero)
    base-color
    (let [a (c-abs c)
          frac-a (/ (mod a 50) (/ 50 max-shade))]
      (assoc base-color :alpha frac-a))))

(defn- get-arg-shade [^ComplexNumber c]
  (if (c-eql c Zero)
    base-color
    (let [a (c-arg c)
          circle-split (/ math/PI 6)
          frac-a (/ (mod a circle-split) (/ circle-split max-shade))]
      (assoc base-color :alpha frac-a))))

;;; TODO: figure out how I want to map from the magnitude of the complex number
;;; to the lightness of the color... I think it might turn into something like
;;; the (/ (math/atan n) math/PI) so that n -> infty :: lightness -> 1
;;;
;;; (defn- get-lightness [scale n]
;;;   (let [half-pi (/ math/PI 2)]
;;;     (/ (math/atan (/ n scale)) half-pi)))
(defn- get-lightness [n]
  (/ n 250)) ;;; FIXME: This causes problems if there are values with modulus
             ;;; larger than the denominator

(defn complex-number->color [^ComplexNumber c]
  (let [argument (c-arg c)
        tau (* 2 math/PI)
        hue (* 360
               (/ (mod argument tau) tau))
        saturation 0.5
        lightness (get-lightness (c-abs c))]
    (make-color hue saturation lightness 1.0 :clj-grapher.color/hsla)))

(defn get-color-type
  [show-abs show-arg]
  (if show-abs
    (if show-arg
      ::abs-and-arg
      ::abs-only)
    (if show-arg
      ::arg-only
      ::none)))

(defn get-color-from-result
  ([^ComplexNumber c] (get-color-from-result c ::abs-and-arg))
  ([^ComplexNumber c kwd]
   (let [res-color (complex-number->color c)]
     (ecase kwd
       ::none res-color
       ::abs-only (composite (get-abs-shade c) res-color)
       ::arg-only (composite (get-arg-shade c) res-color)
       ::abs-and-arg (composite* (get-abs-shade c)
                                 (get-arg-shade c)
                                 res-color)))))

;;; TODO: look into using the for comprehensions here?
(defn- calculate-rectangle*
  [app-func init width height step]
  (let [x-num (/ width step)
        y-num (/ height step)
        row-gen (fn [yi]
                  (map #(->ComplexNumber % yi)
                       (take x-num
                             (iterate (partial + step) (:real init)))))
        arr (map row-gen
                 (take y-num
                       (iterate (partial + step) (:imaginary init))))]
    (map #(map app-func %) arr)))

(defn calculate-rectangle
  ([func init width height step]
   (let [app-func (fn [z]
                    (let [v (func z)]
                      (get-color-from-result v)))]
     (calculate-rectangle* app-func init width height step)))
  ([func color-type init width height step]
   (let [app-func (fn [z]
                    (let [v (func z)]
                      (get-color-from-result v color-type)))]
     (calculate-rectangle* app-func init width height step))))
