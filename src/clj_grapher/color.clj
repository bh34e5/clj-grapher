(ns clj-grapher.color
  (:require [clojure.math :as math]
            [clj-utils.core :refer [ecase noisy-clamp]]))

(defn- one-minus [n] (- 1 n))

(defn- assert-in-range
  ([low high value]
   (assert-in-range low
                    high
                    value
                    (str "Value " value " not in range [" low ", " high "]")))
  ([low high value message]
   (assert (<= low value high) message)))

(defn- assert-in-range*
  [low high & values]
  (doseq [v values]
    (assert-in-range low high v)))

(def ^{:private true} color-vec [:red :green :blue])

(defn hsl->rgb
  "Takes hue in [0, 360), saturation in [0, 1] and lightness in [0, 1]"
  [[h s l]]
  (let [chroma (* s (one-minus (abs (one-minus (* 2 l)))))
        segment (/ h 60)
        x (* chroma (one-minus (abs (one-minus (mod segment 2)))))
        c-color (nth color-vec (mod (math/floor-div (inc segment) 2) 3))
        x-color (nth color-vec (mod (one-minus (math/floor segment)) 3))
        color-map (assoc {} c-color chroma x-color x)
        m (- l (/ chroma 2))
        mapper (fn [component]
                 (let [n (math/floor (* 255 (+ m component)))]
                   (noisy-clamp n 0 255)))]
    (vec (map mapper
              [(:red color-map 0)
               (:green color-map 0)
               (:blue color-map 0)]))))

(defrecord Color [red green blue alpha])

(defn make-color
  ([[& comps]] (apply make-color comps))
  ([r g b] (make-color r g b 1.0))
  ([c1 c2 c3 a method]
   (ecase method
     ::rgba (make-color c1 c2 c3 a)
     ::hsla (let [[r g b] (hsl->rgb [c1 c2 c3])]
              (make-color r g b a))))
  ([r g b a]
   (assert-in-range* 0 255 r g b)
   (assert-in-range 0.0 1.0 a)
   (->Color r g b a)))

(defn to-hex [^Color color]
  (let [alpha (:alpha color)
        rounded-alpha (math/round (* 255 alpha))]
    (bit-or
      (bit-shift-left (:red color) 24)
      (bit-shift-left (:green color) 16)
      (bit-shift-left (:blue color) 8)
      rounded-alpha)))

(defn composite
  [c1 c2]
  (if (== 0.0 (:alpha c1) (:alpha c2))
    (make-color 0 0 0 0.0)
    (let [apply-alpha (fn [a-co b-co]
                        (+ (* a-co (:alpha c1))
                           (* b-co
                              (:alpha c2)
                              (- 1 (:alpha c1)))))
          alpha-o (apply-alpha 1 1)
          c1-comps (map #(% c1) (list :red :green :blue))
          c2-comps (map #(% c2) (list :red :green :blue))
          new-comps (map #(/ (apply-alpha %1 %2) alpha-o)
                         c1-comps
                         c2-comps)]
      (make-color (conj (vec new-comps) alpha-o)))))

(defn composite*
  [& colors]
  (reduce composite colors))
