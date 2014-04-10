(defproject clj-game "0.1.0-SNAPSHOT"
  :description "a game"

  :dependencies [[com.badlogicgames.gdx/gdx "1.0-SNAPSHOT"]
                 [com.badlogicgames.gdx/gdx-backend-lwjgl "1.0-SNAPSHOT"]
                 [com.badlogicgames.gdx/gdx-platform "1.0-SNAPSHOT"
                  :classifier "natives-desktop"]
                 [org.clojure/clojure "1.5.1"]
                 [play-clj "0.3.0-SNAPSHOT"]]
  :repositories [["sonatype"
                  "https://oss.sonatype.org/content/repositories/snapshots/"]]

  :source-paths ["src" "src-common"]
  :javac-options ["-target" "1.6" "-source" "1.6" "-Xlint:-options"]
  :aot [clj-game.core.desktop-launcher]
  :main clj-game.core.desktop-launcher)
