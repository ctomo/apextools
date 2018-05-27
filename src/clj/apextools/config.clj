(ns apextools.config
  ^{:author chris}
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))


(def config-name "build.properties")

(def property-names
  "Mapping of properties"
  {:username "sf.username",
   :password "sf.password",
   :security-token "sf.securitytoken",
   :login-url "sf.serverurl"
   :prefix "sf.namespaceprefix"})


(defn- get-dir
  "Return the directory of path, or nil if it doesn't exists."
  [path-or-directory]
  (let [file (.toFile path-or-directory)]
    (if (.exists file)
      (if (.isDirectory file)
        (.getCanonicalPath file)
        (.getParent file)))))

(defn- find-first-matching-file
  "Return the named file of the project for the related path."
  [start-path name]
  (if-let [start-dir (get-dir start-path)]
    (let [possible-paths (for [x ["/" "/../" "/../../"]] ;; possible related paths
                           (str start-dir x name))]
      (some #(let [f (io/file %)] ;; return the first valid matching file name
               (when (.exists f) f))
            possible-paths))))

(defn get-file-path
  "Return the file with name of the project for the related path."
  [path-or-directory name]
  (when-let [f (find-first-matching-file path-or-directory name)]
    (.getCanonicalPath f)))

(defn get-parent-path
  "Return the parent directory of name for the project with related path."
  [path-or-directory name]
  (when-let [f (find-first-matching-file path-or-directory name)]
    (-> f .getParentFile .getCanonicalPath)))

(defn- load-properties
  [file-name]
  (with-open [reader (io/reader file-name)]
    (doto (java.util.Properties.)
      (.load reader))))

(defn load-config
  "Load the config file wich belongs to related-path and return a map."
  [related-path]
  (if-let [path (get-file-path related-path config-name)]
    (let [properties (load-properties path)
          prop-pairs (for [x property-names]
                       [(key x) (get properties (val x))])]
      (into {} prop-pairs))))
