(ns clj-grapher.math
  (:require [clojure.core.async :as async :refer [<!! <! >!! >!]]
            [clojure.math :as math]
            [clj-grapher.color :refer [make-color composite composite*]]
            [clj-utils.core :refer [ecase noisy-clamp]]))

(defn frac [n]
  (- n (math/floor n)))

;;; TODO: consider moving away from the record, there's no real reason to have
;;; it here, since I'm not defining any protocols with it...
(defrecord ComplexNumber [real imaginary])
(def Zero (->ComplexNumber 0 0))
(def One (->ComplexNumber 1 0))
(def I (->ComplexNumber 0 1))

(defn c-abs [^ComplexNumber c]
  (math/hypot (:real c) (:imaginary c)))

(defn c-arg [^ComplexNumber c]
  (math/atan2 (:imaginary c) (:real c)))

(defn c-eql
  ([c] true)
  ([c d] (and (== (:real c) (:real d))
              (== (:imaginary c) (:imaginary d))))
  ([c d & others]
   (reduce c-eql (list* c d others))))

(defn c-add
  ([] Zero)
  ([c] c)
  ([c d] (->ComplexNumber (+ (:real c) (:real d))
                          (+ (:imaginary c) (:imaginary d))))
  ([c d & others]
   (reduce c-add (list* c d others))))

(defn c-sub
  ([c] (->ComplexNumber (- (:real c)) (- (:imaginary c))))
  ([c d] (c-add c (c-sub d)))
  ([c d & others]
   (c-sub c (apply c-add d others))))

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
   (reduce c-mult (list* c d others))))

(defn c-div
  ([c] (let [r (:real c)
             i (:imaginary c)
             denom (+ (* r r) (* i i))]
         (if (zero? denom)
           (->ComplexNumber 5000 5000) ;;; FIXME: handle infinity
           (->ComplexNumber (/ r denom) (/ (- i) denom)))))
  ([c d] (c-mult c (c-div d)))
  ([c d & others]
   (c-div c (apply c-mult d others))))

(defn c-exp [c]
  (let [mag (math/exp (:real c))
        imag (:imaginary c)
        arg (->ComplexNumber (math/cos imag) (math/sin imag))]
    (c-const-mult mag arg)))

(defn c-sin [c]
  (let [iz (c-mult I c)
        exp-iz (c-exp iz)
        exp-neg-iz (c-exp (c-sub iz))]
    (c-div (c-sub exp-iz exp-neg-iz) (->ComplexNumber 0 2))))

(defn c-cos [c]
  (let [iz (c-mult I c)
        exp-iz (c-exp iz)
        exp-neg-iz (c-exp (c-sub iz))]
    (c-div (c-add exp-iz exp-neg-iz) (->ComplexNumber 2 0))))

(defn c-tan [c]
  (let [csin (c-sin c)
        ccos (c-cos c)]
    (c-div csin ccos)))

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
;;; (defn- get-lightness [n]
;;;   (/ n 250)) ;;; FIXME: This causes problems if there are values with
;;;              ;;; modulus larger than the denominator
(defn- get-lightness [scale n]
  (let [pith (/ math/PI)
        root-n-by-scale (math/sqrt (/ n scale))
        clamped (noisy-clamp root-n-by-scale 0 1)]
    (* 2 pith (math/asin clamped))))

(defn complex-number->color [scale ^ComplexNumber c]
  (let [argument (c-arg c)
        tau (* 2 math/PI)
        hue (* 360
               (/ (mod argument tau) tau))
        saturation 0.5
        lightness (get-lightness scale (c-abs c))]
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
  ([scale ^ComplexNumber c] (get-color-from-result scale c ::abs-and-arg))
  ([scale ^ComplexNumber c kwd]
   (let [res-color (complex-number->color scale c)]
     (ecase kwd
       ::none res-color
       ::abs-only (composite (get-abs-shade c) res-color)
       ::arg-only (composite (get-arg-shade c) res-color)
       ::abs-and-arg (composite* (get-abs-shade c)
                                 (get-arg-shade c)
                                 res-color)))))

(declare ^{:private true} read-calculate-loop)

(def ^{:private true} calculation-manager-chan-req (async/chan 1))
(def ^{:private true} calculation-manager-chan-res (async/chan 1))

(def ^{:private true} calculation-manager-thread
  (Thread. #(read-calculate-loop)))

(defn restart-calculation-manager []
  (when calculation-manager-chan-req
    (async/close! calculation-manager-chan-req))
  (when calculation-manager-chan-res
    (async/close! calculation-manager-chan-res))
  (def ^{:private true} calculation-manager-chan-req (async/chan 1))
  (def ^{:private true} calculation-manager-chan-res (async/chan 1))
  (def ^{:private true} calculation-manager-thread
    (Thread. #(read-calculate-loop))))

(defn- pool-calc
  [app-func input-map]
  (let [res (object-array (count input-map))
        futures (mapv #(let [[ind row-arr] %1]
                         (future (aset res ind (mapv app-func row-arr))))
                      (map-indexed vector input-map))]
    (mapv deref futures)
    (seq res)))

(defn- read-calculate-loop []
  (println (.getName (Thread/currentThread)))
  (let [[app-func input-map] (<!! calculation-manager-chan-req)
        res (pool-calc app-func input-map)]
    (>!! calculation-manager-chan-res res))
  (recur))


;;; TODO: look into using the for comprehensions here?
(defn- calculate-rectangle*
  [app-func init width height step]
  (when (= Thread$State/NEW (.getState calculation-manager-thread))
    (doto calculation-manager-thread
      (.setDaemon true) ;; set this thread to be a daemon so that the
      (.start)))        ;; JVM properly exits on close
  (let [x-num (/ width step)
        y-num (/ height step)
        row-gen (fn [yi]
                  (map #(->ComplexNumber % yi)
                       (take x-num
                             (iterate (partial + step) (:real init)))))
        arr (map row-gen
                 (take y-num
                       (iterate (partial + step) (:imaginary init))))]
    (>!! calculation-manager-chan-req [app-func arr])
    (<!! calculation-manager-chan-res)))

(defn calculate-rectangle
  ([func scale init width height step]
   (let [app-func (fn [z]
                    (let [v (func z)]
                      (get-color-from-result scale v)))]
     (calculate-rectangle* app-func init width height step)))
  ([func color-type scale init width height step]
   (let [app-func (fn [z]
                    (let [v (func z)]
                      (get-color-from-result scale v color-type)))]
     (calculate-rectangle* app-func init width height step))))
