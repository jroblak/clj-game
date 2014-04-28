(ns clj-game.entities
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [clj-game.utils :as u]))

(defn ^:private create
  ([{:keys [x y direction] :as entity}] ; projectile
   (let [sheet (texture "attack.png")
         tiles (texture! sheet :split 15 15)
         base-image (texture (aget tiles 0 0))
         attack-images (for [col [0 1 2 3 4 5 6 7 8 9]]
                        (texture (aget tiles 0 col)))
         dir (condp = direction
               :right 1
               :left -1
               0)]
   (assoc base-image
     :width 1
     :height 1
     :walk-right (animation u/duration
                            attack-images
                            :set-play-mode (play-mode :loop))
     :walk-left (animation u/duration
                           (map #(texture % :flip true false) attack-images)
                           :set-play-mode (play-mode :loop))
     :direction direction
     :y-velocity 0
     :x-velocity (* 10 dir)
     :x (+ x (* 1 dir))
     :y y)))
  ([stand jump walk] ; player and enemies
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
     :height 1
     :x-velocity 0
     :y-velocity 0
     :can-jump? false
     :can-damage? true
     :can-attack? true
     :direction :right)))

(defn create-health
  [heart-image]
  (assoc heart-image
    :x 10
    :y 10
    :width 1
    :height 1))

(defn player-collision
  [player screen touching-entities]
  (let [current-health (:health player)]
    (if (and (some #(:enemy? %) touching-entities) (:can-damage? player))
      (do
        (add-timer! screen :player-damage-cooldown 3)
        {:health (- current-health 1) :can-damage? false :x (- (:x player) 0.5) :y (- (:y player) 0.5)})
      {})))

(defn baddy-collision
  [baddy screen touching-entities]
  (let [current-health (:health baddy)]
    (merge {}
           (if (some #(:attack? %) touching-entities)
             {:health (- current-health 1)}
             {}))))

(defn create-player
  [stand jump & walk]
  (assoc (create stand jump walk)
         :me? true
         :id 0
         :health u/starting-health
         :collision-callback player-collision
         :x 20
         :y 10))

(defn create-baddy
  [object stand jump & walk]
  (assoc (create stand jump walk)
    :enemy? true
    :health 1
    :collision-callback baddy-collision
    :id (u/uuid)
    :x (/ (map-object! object :x) u/pixels-per-tile)
    :y (/ (map-object! object :y) u/pixels-per-tile)))

(defn create-attack
  [screen entities entity]
    (assoc (create entity)
      :id (u/uuid)
      :collision-callback (fn [self screen touching-entities] {:remove? true})
      :attack? true))

(defn move
  [{:keys [delta-time]} {:keys [x y can-jump? attack?] :as entity}]
  (let [x-velocity (u/get-x-velocity entity)
        y-velocity (+ (u/get-y-velocity entity) u/gravity)
        x-change (* x-velocity delta-time)
        y-change (* y-velocity delta-time)]
    (cond
     (= attack? true) ; attacks should always move
       (assoc entity
        :x-velocity x-velocity
        :y-velocity (u/get-y-velocity entity)
        :x-change (* x-velocity delta-time)
        :y-change (* y-velocity delta-time)
        :x (+ x x-change)
        :y (+ y y-change))
     (or (not= 0 x-change) (not= 0 y-change)) ; if there was movement
      (assoc entity
        :x-velocity (u/decelerate x-velocity)
        :y-velocity (u/y-decelerate y-velocity)
        :x-change x-change
        :y-change y-change
        :x (+ x x-change)
        :y (+ y y-change)
        :can-jump? (if (> y-velocity 0) false can-jump?))
      :else
        entity)))

(defn handle-attacks
  [screen entities]
  (flatten
    (for [{:keys [x y x-change y-change can-attack? me? attack?] :as entity} entities]
      (if (and me? (key-pressed? :control-left) can-attack?)
        (do
          (add-timer! screen :player-attack-cooldown 1)
          [(create-attack screen entities entity)
           (assoc entity :can-attack? false)])
        (if (and attack? (= x-change 0))
          []
          entity)))))

; ai code
(defn handle-ai
  [entities]
  entities)

; remove any entities with <= 0 health
(defn handle-damage
  [screen entities]
  (remove #(cond
          (:remove? %) true
          (nil? (:health %)) false
          (> (:health %) 0) false
          :default true) entities))

; animation code
(defn animate
  [screen {:keys [x-velocity y-velocity
                  stand-right stand-left
                  jump-right jump-left
                  walk-right walk-left
                  attack?] :as entity}]
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


(defn collide
  [screen entities {:keys [x y x-change y-change collision-callback] :as entity}]
  (let [old-x (- x x-change)
        old-y (- y y-change)
        entity-x (assoc entity :y old-y)
        entity-y (assoc entity :x old-x)
        up? (> y-change 0)]
    (merge entity
           (when-let [touching-entities (u/get-touching-entities entities entity)]
             (collision-callback entity screen touching-entities))
           (when-let [tile (u/get-touching-tile screen entity-x "walls")]
             {:x-velocity 0 :x-change 0 :x old-x})
           (when-let [tile (u/get-touching-tile screen entity-y "walls")]
             {:y-velocity 0 :y-change 0 :y old-y
              :can-jump? (not up?)})
           (when-let [tile (u/get-touching-tile screen entity-y "breakable")]
             {:y-velocity 0 :y-change 0 :y old-y
              :can-jump? (not up?) :to-destroy (when up? tile)}))))
