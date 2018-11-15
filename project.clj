(defproject ctomo/apextools "0.1.0-SNAPSHOT"
  :description "Parse Apex sources into clojure data structures."
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths ["src/clj"]
  :java-source-paths ["src/java" "target/antlr-gen-src"]
  :resource-paths ["src/resources"]
  :prep-tasks [["antlr"]
               "javac" "compile"]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.antlr/antlr4 "4.5"]
                 [org.clojure/tools.cli "0.3.5"]
                 [ubergraph "0.4.0"]
                 [cheshire "5.8.0"]
                 [byte-streams "0.2.3"]
                 [com.taoensso/timbre "4.10.0"]
                 [com.taoensso/nippy "2.14.0"]
                 [com.rpl/specter "1.1.0"]
                 [org.glassfish.jaxb/jaxb-xjc "2.3.1"]
                 [org.glassfish.jaxb/jaxb-runtime "2.3.1"]
                 [javax.activation/javax.activation-api "1.2.0"]]
  :plugins [[org.clojars.ctomo/lein-xjc "0.2.3"]
            [lein-antlr "0.3.0"]]
  :xjc-plugin {:xjc-calls [{:xsd-file "src/xsd/metadata.xsd" :options ["-quiet"]}]}
  :antlr-src-dir "src/antlr"
  :antlr-dest-dir "target/antlr-gen-src/apex/parser"
  :antlr-options {:Werror true
                  :package "apex.parser"}
  :hooks [leiningen.antlr]
  :profiles {:uberjar {:aot :all}}
  :main apextools.main)
