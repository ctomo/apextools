(ns apextools.paths
  (:require [clojure.string :as string]
            [clojure.java.io :as io])
  (:import [java.nio.file FileSystems SimpleFileVisitor FileVisitResult Files]
           [java.nio.file Paths]))

(defn get-path
  "Converts a String to a java Path object."
  [& parts]
  (->> (map #(if (string? %) % (.toString %)) parts)
       (into-array String)
       (Paths/get "")))

(defn exists
  "Check if the file specified by path-str exists and is not a directory."
  [path-str]
  (let [f (io/file path-str)]
    (and (.exists f) (not (.isDirectory f)))))

(defn find-all
  "Return a vector with UnixPath instances which match search criteria."
  [glob-pattern starting-dir]
  (let [starting-path-dir (get-path starting-dir)]
    (with-local-vars [results []]
      (let [matcher (.getPathMatcher (FileSystems/getDefault) (str "glob:" glob-pattern))
            finder (proxy [SimpleFileVisitor] []
                     (visitFile [path attrs]
                       (let [name (.getFileName path)]
                         (when (and name (.matches matcher name))
                           (var-set results (conj @results path))))
                       FileVisitResult/CONTINUE)
                     (visitFileFailed [path io-exception]
                       FileVisitResult/CONTINUE))]
        (Files/walkFileTree starting-path-dir finder)
        @results))))

(defn find-members
  [path pattern]
  (remove empty?
          (map #(-> % .getFileName .toString (string/split #"\.") first)
               (find-all pattern path))))
