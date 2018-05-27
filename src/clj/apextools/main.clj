(ns apextools.main
  (:require [apextools.helper.api :as api]
            [apextools.helper.schema :as schema]
            [clojure.tools.cli :refer [parse-opts]]
            [taoensso.timbre :as timbre]
            [clojure.string :as string]
            [apextools.package :as pkg])
  (:gen-class))



(defn usage [options-summary]
  (->> [""
        "Actions:"
        ""
        ""
        "Options:"
        options-summary]
       (string/join \newline)))

(def cli-options
  [["-c" "--class CLASSNAME" "Specify the name of an Apex class."
    :assoc-fn (fn [m k v] (update-in m [k]
                                     (fn [pre]
                                       (if pre
                                         (if (set? pre)
                                           (conj pre v)
                                           #{v pre})
                                         v))))]
   ["-p" "--path PATH" "Set the path." :default (System/getProperty "user.dir")]
   ["-h" "--help" "Show this help page."]])

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn exit
  ([status]
   (System/exit status))
  ([status msg]
   (println msg)
   (exit status)))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (timbre/merge-config!
     {:level :error})

    ;; Handle help and error conditions
    (cond
      (:help options) (exit 0 (usage summary))
      (not= (count arguments) 1) (exit 1 (usage summary))
      errors (exit 1 (error-msg errors)))
    
    ;; Execute program with options
    (case (first arguments)
      "options" (println "options:" options)
      "parse" (do (pkg/load-packages (:path options))
                  (exit 0 "done."))
      "prepare-api" (do (api/save-default-apex-api-file options)
                        (exit 0 "done."))
      "prepare-schema" (do (schema/save-default-schema-file options)
                           (exit 0 "done."))
      (exit 1 (usage summary)))))