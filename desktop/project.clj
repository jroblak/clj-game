(defproject clj-game "0.0.1-SNAPSHOT"
  :description "FIXME: write description"
  
  :dependencies [[com.badlogicgames.gdx/gdx "0.9.9"]
                 [com.badlogicgames.gdx/gdx-backend-lwjgl "0.9.9"]
                 [com.badlogicgames.gdx/gdx-platform "0.9.9"
                  :classifier "natives-desktop"]
                 [org.clojure/clojure "1.5.1"]
                 [play-clj "0.2.2"]]
  :repositories [["sonatype"
                  "https://oss.sonatype.org/content/repositories/snapshots/"]]
  
  :source-paths ["src" "src-common"]
  :javac-options ["-target" "1.6" "-source" "1.6" "-Xlint:-options"]
  :aot [clj-game.core.desktop-launcher]
  :main clj-game.core.desktop-launcher)
