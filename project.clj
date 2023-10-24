(defproject clj-grapher "0.1.0-SNAPSHOT"
  :description "A Grapher implementation"
  :url "https://github.com/bh34e5/clj-grapher"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.async "1.6.673"]
                 [org.openjfx/javafx-controls "17.0.9"]
                 [clj-utils "0.1.0-SNAPSHOT"]]
  :main clj-grapher.entry
  :aot [clj-grapher.entry clj-grapher.gui.application]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
