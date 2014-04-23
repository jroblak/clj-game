(ns clj-game.core.desktop-launcher
  (:require [clj-game.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. clj-game "clj-game" 800 600)
  (Keyboard/enableRepeatEvents true))
