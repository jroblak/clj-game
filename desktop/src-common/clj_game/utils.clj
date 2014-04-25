(ns clj-game.utils
  (:require [play-clj.core :refer :all]))

; constants
(def ^:const vertical-tiles 20)
(def ^:const starting-health 5)
(def ^:const pixels-per-tile 16)
(def ^:const duration 0.15)
(def ^:const damping 1.0)
(def ^:const max-velocity 8)
(def ^:const jump-velocity 8)
(def ^:const max-jump-velocity (* jump-velocity 4))
(def ^:const x-deceleration 0.9)
(def ^:const y-deceleration 0.01)
(def ^:const gravity -2.0)

; deceleration for x-velocity
(defn decelerate
  [velocity]
  (let [velocity (* velocity x-deceleration)]
    (if (< (Math/abs velocity) damping)
      0
      velocity)))

; deceleration for y-velocity
(defn y-decelerate
  [velocity]
  (let [velocity (- velocity y-deceleration)]
    (if (< (Math/abs velocity) damping)
      0
      velocity)))

(defn ^:private touched?
  [key]
  (and (game :touched?)
       (case key
         :down (> (game :y) (* (game :height) (/ 2 3)))
         :up (< (game :y) (/ (game :height) 3))
         :left (< (game :x) (/ (game :width) 3))
         :right (> (game :x) (* (game :width) (/ 2 3)))
         false)))

(defn get-x-velocity
  [{:keys [me? x-velocity]}]
  (if me?
    (cond
      (or (key-pressed? :dpad-left) (touched? :left))
      (* -1 max-velocity)
      (or (key-pressed? :dpad-right) (touched? :right))
      max-velocity
      :else
      x-velocity)
    x-velocity))

; get y-velocity from keypresses (jumping)
(defn get-y-velocity
  [{:keys [me? y-velocity can-jump?]}]
  (if me?
    (cond
      (and can-jump? (or (key-pressed? :dpad-up) (touched? :up)))
      max-jump-velocity
      :else
      y-velocity)
    y-velocity))

(defn uuid [] (str (java.util.UUID/randomUUID)))

; flip sprite based on left / right
(defn get-direction
  [{:keys [x-velocity direction]}]
  (cond
    (> x-velocity 0) :right
    (< x-velocity 0) :left
    :else
    direction))

(defn touching-entity?
  [{:keys [x y id] :as e} e2]
  (and (not= id (:id e2))
       (nil? (:draw-time e2))
       (< (Math/abs ^double (- x (:x e2))) 1)
       (< (Math/abs ^double (- y (:y e2))) 1)))

(defn get-touching-entities
  [entities entity]
  (seq (filter #(touching-entity? entity %) entities)))

(defn get-touching-tile
  [screen {:keys [x y width height]} & layer-names]
  (let [layers (map #(tiled-map-layer screen %) layer-names)]
    (->> (for [tile-x (range (int x) (+ x width))
               tile-y (range (int y) (+ y height))]
           (some #(when (tiled-map-cell % tile-x tile-y)
                    [tile-x tile-y])
                 layers))
         (drop-while nil?)
         first)))
