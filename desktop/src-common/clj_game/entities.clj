(ns clj-game.entities
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [clj-game.utils :as u]))

; https://github.com/oakes/play-clj-examples/blob/master/minicraft/desktop/src-common/minicraft/entities.clj
; player entity creation code
(defn create-player
  [stand jump & walk]
  (assoc stand
         :stand-right stand
         :stand-left (texture stand :flip true false)
         :jump-right jump
         :jump-left (texture jump :flip true false)
         :walk-right (animation u/duration
                                walk
                                :set-play-mode (play-mode :loop-pingpong))
         :walk-left (animation u/duration
                               (map #(texture % :flip true false) walk)
                               :set-play-mode (play-mode :loop-pingpong))
         :width 1
         :height (/ 26 18)
         :x-velocity 0
         :y-velocity 0
         :x 20
         :y 10
         :is-me? true
         :can-jump? false
         :can-attack? true
         :direction :right))

(defn move
  [{:keys [delta-time]} {:keys [x y can-jump?] :as entity}]
  (let [x-velocity (u/get-x-velocity entity)
        y-velocity (+ (u/get-y-velocity entity) u/gravity)
        x-change (* x-velocity delta-time)
        y-change (* y-velocity delta-time)]
    (if (or (not= 0 x-change) (not= 0 y-change))
      (assoc entity
             :x-velocity (u/decelerate x-velocity)
             :y-velocity (u/y-decelerate y-velocity)
             :x-change x-change
             :y-change y-change
             :x (+ x x-change)
             :y (+ y y-change)
             :can-jump? (if (> y-velocity 0) false can-jump?))
      entity)))

(defn create-attack
  [screen entities entity]
  (let [sheet (texture "sprites.png")
        tiles (texture! sheet :split 32 32)
        player-images (for [col [0 1 2 3 4]]
                        (texture (aget tiles 0 col)))]
      (conj entities (apply create-player player-images))))

(defn attack
  [screen entities {:keys [x y can-attack? is-me?] :as entity}]
  (if (and is-me? (is-pressed? :control-left) can-attack?)
      (do
          (add-timer! screen :player-attack-cooldown 10)
          (create-attack screen entities entity)
          (assoc entity :can-attack false))
      entity))

; animation code
(defn animate
  [screen {:keys [x-velocity y-velocity
                  stand-right stand-left
                  jump-right jump-left
                  walk-right walk-left] :as entity}]
  (let [direction (u/get-direction entity)]
    (merge entity
           (cond
             (not= y-velocity 0)
             (if (= direction :right) jump-right jump-left)
             (not= x-velocity 0)
             (if (= direction :right)
               (animation->texture screen walk-right)
               (animation->texture screen walk-left))
             :else
             (if (= direction :right) stand-right stand-left))
           {:direction direction})))

; collision code
(defn prevent-move
  [screen {:keys [x y x-change y-change] :as entity}]
  (let [old-x (- x x-change)
        old-y (- y y-change)
        entity-x (assoc entity :y old-y)
        entity-y (assoc entity :x old-x)
        up? (> y-change 0)]
    (merge entity
           (when (u/get-touching-tile screen entity-x "walls")
             {:x-velocity 0 :x-change 0 :x old-x})
           (when-let [tile (u/get-touching-tile screen entity-y "walls")]
             {:y-velocity 0 :y-change 0 :y old-y
              :can-jump? (not up?)})
           (when-let [tile (u/get-touching-tile screen entity-y "breakable")]
             {:y-velocity 0 :y-change 0 :y old-y
              :can-jump? (not up?) :to-destroy (when up? tile)}))))
