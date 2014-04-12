(ns clj-game.level
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.ui :refer :all]
            [clj-game.entities :as e]
            [clj-game.utils :as u]))

; custom update code for the game
; handles removing and resetting entities
(defn update-screen!
  [clj-game title-screen screen entities]
  (doseq [{:keys [x y height me? attack? to-destroy] :as entity} entities]
    (when me?
      (x! screen x)
      (when (< y (- height)) ; when the players y is less than the maps height, reset
        (set-screen! clj-game title-screen)))
    (when-let [[tile-x tile-y] to-destroy] ; iterate through breakable tiles that have to-destroy set
      (tiled-map-layer! (tiled-map-layer screen "breakable")
                        :set-cell tile-x tile-y nil)))
  (map #(dissoc % :to-destroy) entities))

(defn on-show
  [level screen entities]
  (let [screen (->> (/ 1 u/pixels-per-tile)
                    (orthogonal-tiled-map (clojure.string/join "" [level ".tmx"]))
                    (update! screen :camera (orthographic) :renderer))
        ; TODO - better way to handle this with changing between level
        sheet (texture "player.png")
        tiles (texture! sheet :split 32 32)
        player-images (for [col [0 1 2 3 4]]
                        (texture (aget tiles 0 col)))
        enemy-images (for [col [0 1 2 3 4]]
                       (texture (aget tiles 1 col)))]
    (flatten (pvalues
              (apply e/create-player player-images)
              (for [object (map-layer! (map-layer screen "entities") :get-objects)]
                (apply e/create-baddy (conj enemy-images (map-object! object :get-rectangle))))))))

(defn on-render
  [clj-game title-screen screen entities]
  (clear! 0.5 0.5 1 1) ; RGBA background color
  (->> entities
       (e/handle-ai)
       (map #(->> %
                  (e/move screen)
                  (e/collide screen)
                  (e/animate screen)))
       (e/handle-attacks screen)
       (render! screen)
       (update-screen! clj-game title-screen screen)))

(defn on-resize
  [{:keys [width height] :as screen} entities]
  (orthographic! screen
                 :set-to-ortho
                 false
                 (* u/vertical-tiles (/ width height))
                 u/vertical-tiles))

(defn on-timer
  [screen entities]
  (case (:id screen)
    :player-attack-cooldown (for [entity entities]
                              (if (get entity :me?)
                                (assoc entity :can-attack? true)
                                entity))
    nil))
