(set! *warn-on-reflection* true)

(ns dungeon-crawler.core
  (:require [dungeon-crawler.entities :as e]
            [dungeon-crawler.rooms :as r]
            [dungeon-crawler.utils :as u]
            [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.ui :refer :all]))

(defn update-screen!
  [screen entities]
  (doseq [{:keys [x y player?]} entities]
    (when player?
      (position! screen x y)))
  entities)

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (let [screen (->> (/ 1 u/pixels-per-tile)
                      (isometric-tiled-map "level1.tmx")
                      (update! screen :camera (orthographic) :renderer))
          start-room {:x (rand-int r/rows)
                      :y (rand-int r/cols)}
          start-player-x (+ (* (:x start-room) r/size)
                            (/ r/size 2))
          start-player-y (+ (* (:y start-room) r/size)
                            (/ r/size 2))
          rooms (for [row (range r/rows)
                      col (range r/cols)]
                  {:x row :y col})]
      (r/connect-rooms! screen rooms start-room)
      (->> [(->> (assoc (e/create-player)
                        :x start-player-x
                        :y start-player-y)
                 (isometric->screen screen))
            (take 20 (repeat (e/create-elemental)))
            (take 20 (repeat (e/create-ogre)))]
           flatten
           (reduce
             (fn [entities entity]
               (conj entities
                     (-> (if (:npc? entity)
                           (e/randomize-location screen entities entity)
                           entity)
                         (assoc :id (count entities)))))
             []))))
  :on-render
  (fn [screen entities]
    (clear!)
    (->> entities
         (map (fn [entity]
                (->> entity
                     (e/move screen entities)
                     (e/animate screen)
                     (e/prevent-move screen entities)
                     (e/adjust screen))))
         (render-sorted! screen ["walls"])
         (update-screen! screen)))
  :on-resize
  (fn [screen entities]
    (height! screen u/vertical-tiles)))

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

(defgame dungeon-crawler
  :on-create
  (fn [this]
    (set-screen! this main-screen text-screen)))
