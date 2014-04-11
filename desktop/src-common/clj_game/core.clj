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
            [clj-game.utils :as u]
            [clj-game.level :as l]))

; declare forward bindings for our 'screens' / 'levels'
(declare clj-game main-screen text-screen title-screen)

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
    (l/on-show "level1" screen entities))
  :on-render
  (fn [screen entities]
    (l/on-render screen entities))
  :on-resize
  (fn [{:keys [width height] :as screen} entities]
    (l/on-resize screen entities))
  :on-timer
  (fn [screen entities]
    (l/on-timer screen entities)))

; SECOND LEVEL SCREEN
(defscreen second-screen
  :on-show
  (fn [screen entities]
    (l/on-show "level2" screen entities))
  :on-render
  (fn [screen entities]
    (l/on-render screen entities))
  :on-resize
  (fn [{:keys [width height] :as screen} entities]
    (l/on-resize screen entities))
  :on-timer
  (fn [screen entities]
    (l/on-timer screen entities)))

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
