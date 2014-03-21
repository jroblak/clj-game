(ns clj-game.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]))

; https://github.com/oakes/play-clj-examples

(defn move
  [entity direction]
  (case direction
    :down (assoc entity :y (dec (:y entity)))
    :up (assoc entity :y (inc (:y entity)))
    :right (assoc entity :x (inc (:x entity)))
    :left (assoc entity :x (dec (:x entity)))
    nil))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage) :camera (orthographic))
    (conj entities (assoc (texture "clojure.png")
                     :x 50 :y 50 :width 100 :height 100)))
  :on-render
  (fn [screen entities]
    (clear!)
    (render! screen entities))

  :on-resize
  (fn [screen entities]
    (height! screen 600))

  :on-key-down
  (fn [screen entities]
    (cond
      (= (:keycode screen) (key-code :dpad-down))
      (move (first entities) :down)
      (= (:keycode screen) (key-code :dpad-up))
      (move (first entities) :up)
      (= (:keycode screen) (key-code :dpad-right))
      (move (first entities) :right)
      (= (:keycode screen) (key-code :dpad-left))
      (move (first entities) :left)))

  :on-touch-down
  (fn [screen entities]
    (cond
      (> (:y screen) (* (game :height) (/ 2 3)))
      (move (first entities) :down)
      (< (:y screen) (/ (game :height) 3))
      (move (first entities) :up)
      (> (:x screen) (* (game :width) (/ 2 3)))
      (move (first entities) :right)
      (< (:x screen) (/ (game :width) 3))
      (move (first entities) :left))))

(defgame clj-game
  :on-create
  (fn [this]
    (set-screen! this main-screen)))
