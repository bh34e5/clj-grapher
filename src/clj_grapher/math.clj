(ns clj-grapher.math
  (:require [clojure.core.async :as async :refer [<!! <! >!! >!]]
            [clojure.math :as math]
            [clj-grapher.color :refer [make-color composite composite*]]
            [clj-utils.core :refer [ecase noisy-clamp]])
  (:import [java.math MathContext]))

(defn frac [n]
  (- n (math/floor n)))

;;; TODO: consider moving away from the record, there's no real reason to have
;;; it here, since I'm not defining any protocols with it...
(defrecord ComplexNumber [real imaginary])
(def Zero (->ComplexNumber 0 0))
(def One (->ComplexNumber 1 0))
(def I (->ComplexNumber 0 1))

(defn- ensure-bigdec [n]
  (if (and (number? n) (not (instance? BigDecimal n)))
    (BigDecimal. n)
    n))

;;; TODO: consider making this dynamic so it can be set... but then again,
;;;       I don't intend to expose anything to the application so maybe not
(defn- ensure-complex-bigdec [^ComplexNumber c]
  (->ComplexNumber (ensure-bigdec (:real c))
                   (ensure-bigdec (:imaginary c))))

(def ^{:private true} default-precision 128)
(def ^{:private true} log10-E (ensure-bigdec (math/log10 math/E)))
(def ^{:private true} tau (ensure-bigdec (* 2 math/PI)))

(defn c-abs [^ComplexNumber c]
  (with-precision default-precision
    (let [c-bigdec (ensure-complex-bigdec c)
          r (:real c-bigdec)
          i (:imaginary c-bigdec)
          ssum (+ (* r r) (* i i))]
      (.sqrt ssum MathContext/DECIMAL128))))

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
  ([c d]
   (let [c-big (ensure-complex-bigdec c)
         d-big (ensure-complex-bigdec d)]
     (->ComplexNumber (+ (:real c-big) (:real d-big))
                      (+ (:imaginary c-big) (:imaginary d-big)))))
  ([c d & others]
   (reduce c-add (list* c d others))))

(defn c-sub
  ([c] (let [c-big (ensure-complex-bigdec c)]
         (->ComplexNumber (- (:real c-big)) (- (:imaginary c-big)))))
  ([c d] (c-add c (c-sub d)))
  ([c d & others]
   (c-sub c (apply c-add d others))))

(defn c-const-mult
  ([] One)
  ([n c]
   (let [n-big (ensure-bigdec n)
         c-big (ensure-complex-bigdec c)]
     (->ComplexNumber (* n-big (:real c-big))
                      (* n-big (:imaginary c-big))))))

(defn c-mult
  ([] One)
  ([c] c)
  ([c d]
   (let [c-big (ensure-complex-bigdec c)
         d-big (ensure-complex-bigdec d)]
     (->ComplexNumber (- (* (:real c-big) (:real d-big))
                         (* (:imaginary c-big) (:imaginary d-big)))
                      (+ (* (:real c-big) (:imaginary d-big))
                         (* (:imaginary c-big) (:real d-big))))))
  ([c d & others]
   (reduce c-mult (list* c d others))))

(defn c-div
  ([c] (let [c-big (ensure-complex-bigdec c)
             r (:real c-big)
             i (:imaginary c-big)
             denom (+ (* r r) (* i i))]
         (if (zero? denom)
           (->ComplexNumber ##Inf ##Inf)
           (with-precision default-precision
             (->ComplexNumber (/ r denom) (/ (- i) denom))))))
  ([c d] (c-mult c (c-div d)))
  ([c d & others]
   (c-div c (apply c-mult d others))))

(defn c-exp [c]
  (let [c-big (ensure-complex-bigdec c)
        exponent (* (:real c-big) log10-E)
        [int-val frac-val] (.divideAndRemainder exponent 1M)
        ;; FIXME: this can crash when int-val overflows...
        mag (* (.scaleByPowerOfTen 1M int-val)
               (math/pow 10 frac-val))
        imag-mod (mod (:imaginary c-big) tau)
        arg (->ComplexNumber (math/cos imag-mod) (math/sin imag-mod))]
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
  (try
    (let [futures (map #(future (doall (map app-func %1))) input-map)]
      (pmap deref futures))
    (catch Exception e ::calculation-failure)))

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
    (let [res (<!! calculation-manager-chan-res)]
      (when-not (= res ::calculation-failure)
        res))))

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
