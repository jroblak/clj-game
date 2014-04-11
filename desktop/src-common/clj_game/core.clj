; TODO:
; 1. health
; 2. transitioning to new level(s)
;    - factor out level 1 code to separate functions that are called
;      by the level functions ?

(ns clj-game.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.ui :refer :all]
            [clj-game.entities :as e]
            [clj-game.utils :as u]))

; declare our 'screens' / 'levels'
(declare clj-game main-screen text-screen title-screen)

; custom update code for the game
; handles removing and resetting entities
(defn update-screen!
  [screen entities]
  (doseq [{:keys [x y height me? attack? to-destroy] :as entity} entities]
    (when me?
      (x! screen x)
      (when (< y (- height)) ; when the players y is less than the maps height, reset
        (set-screen! clj-game title-screen)))
    (when-let [[tile-x tile-y] to-destroy] ; iterate through breakable tiles that have to-destroy set
      (tiled-map-layer! (tiled-map-layer screen "breakable")
                        :set-cell tile-x tile-y nil)))
  (map #(dissoc % :to-destroy) entities))

; TITLE SCREEN
(defscreen title-screen
  :on-show
  (fn [screen entities]
    (update! screen :camera (orthographic) :renderer (stage))
    (let [ui-skin (skin "uiskin.json")]
      (table [[(image "logo.png")
               :width 200
               :height 200
               :space-top 20
               :space-bottom 20]
              :row
              (text-button "Start Game!" ui-skin)]
             :align (align :center)
             :set-fill-parent true)))
  :on-render
  (fn [screen entities]
    (clear!)
    (render! screen entities))
  :on-resize
  (fn [screen entities]
    (height! screen (:height screen)))
  :on-ui-changed
  (fn [screen entities]
    (if (instance? com.badlogic.gdx.scenes.scene2d.ui.TextButton (:actor screen))
      (set-screen! clj-game main-screen text-screen))))

; FIRST LEVEL SCREEN
(defscreen main-screen
  :on-show
  (fn [screen entities]
    (let [screen (->> (/ 1 u/pixels-per-tile)
                      (orthogonal-tiled-map "level1.tmx")
                      (update! screen :camera (orthographic) :renderer))
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
  :on-render
  (fn [screen entities]
    (clear! 0.5 0.5 1 1) ; RGBA background color
    (->> entities
         (e/handle-ai)
         (map #(->> %
                    (e/move screen)
                    (e/collide screen)
                    (e/prevent-move screen)
                    (e/animate screen)))
         (e/handle-attacks screen)
         (render! screen)
         (update-screen! screen)))
  :on-resize
  (fn [{:keys [width height] :as screen} entities]
    (orthographic! screen
                   :set-to-ortho
                   false
                   (* u/vertical-tiles (/ width height))
                   u/vertical-tiles))
  :on-timer
  (fn [screen entities]
    (case (:id screen)
      :player-attack-cooldown (for [entity entities]
                                (if (get entity :me?)
                                  (assoc entity :can-attack? true)
                                  entity))
      nil)))

; FPS RENDERER
(defscreen text-screen
  :on-show
  (fn [screen entities]
    (update! screen :camera (orthographic) :renderer (stage))
    (assoc (label "0" (color :white))
           :id :fps
           :x 5))
  :on-render
  (fn [screen entities]
    (->> (for [entity entities]
           (case (:id entity)
             :fps (doto entity (label! :set-text (str (game :fps))))
             entity))
         (render! screen)))
  :on-resize
  (fn [screen entities]
    (height! screen 300)))

; define and start the game
(defgame clj-game
  :on-create
  (fn [this]
    (set-screen! this title-screen)))
