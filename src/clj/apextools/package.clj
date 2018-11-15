(ns apextools.package
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [com.rpl.specter :as s]
            [taoensso.timbre :as timbre]
            [taoensso.nippy :as nippy]
            [apextools.jaxb :as jaxb]
            [apextools.ast :as ast]
            [apextools.paths :as paths] 
            [apextools.config :as config]
            [apextools.helper.schema :as schema]))


(defn load-metadata
  [path metadata-type]
  (when (-> path .toFile .exists)
    (.getValue (jaxb/unmarshal path metadata-type))))

(defn load-meta-data-file
  [path name ext type]
  (load-metadata (->> (str (.toString path) "/" name "." ext)
                      (io/file)
                      (.toPath))
                 type))

;;; caching

(def ast-version 2)


(def internal-path (str (System/getProperty "user.home") "/.apextools/"))
(def cache-path (str internal-path "cache/"))

(defn- setup-folders []
  (io/make-parents cache-path "*"))

(defn- cache-file [f]
  (->> (str (string/replace (.toString f) #"/" "!") ".bin")
       (str cache-path)
       (io/as-file)))

(defn- save-cached
  [f data]
  (setup-folders)
  (let [c (cache-file f)]
    (nippy/freeze-to-file c data))
  data)

(defn- load-cached
  [f load-fn]
  (let [c (cache-file f)
        data (when (.exists c)
               (nippy/thaw-from-file c))
        src (slurp f)]
    (if-not (and data
              (= ast-version (:version data))
              (= src (:source data)))
      (save-cached f (apply load-fn [src]))
      data)))

;;; -------

(defn- load-ast [src]
  (assoc (ast/gen-ast "compilationUnit" src)
    :source src
    :version ast-version))

;;; -------

(defn- load-trigger
  [triggers-path name]
  (let [trg-meta-path (paths/get-path triggers-path (str name ".trigger-meta.xml"))
        trg-file (io/file (.toString (paths/get-path triggers-path (str name ".trigger"))))
        parse-result (ast/gen-ast trg-file)
        meta-data (load-metadata trg-meta-path com.sforce.soap._2006._04.metadata.ApexTrigger)]
    (assoc parse-result :meta meta-data)))

;; -- loading classes

(defn- debug-print-parse-errors
  [parse-result]
  (doall (map #(timbre/debug (str "class " name ": ")
                             (str "Line " (:line %) " Column " (:column %) ": " (:message %)))
              (:parse-errors parse-result))))

(defn load-class
  [classes-path name]
  (timbre/debug (str "loading class " name))
  (let [cls-file (io/file (.toString (paths/get-path classes-path (str name ".cls"))))
        parse-result (load-cached cls-file load-ast)
        meta-data (load-meta-data-file classes-path name "cls-meta.xml" com.sforce.soap._2006._04.metadata.ApexClass)]
    (debug-print-parse-errors parse-result)
    (assoc parse-result
           :file cls-file
           :meta meta-data
           :inner-classes (when-let [ast (:ast parse-result)] (ast/extract-inner-classes ast)))))

(defn- load-classes
  [root]
  (let [p (paths/get-path root "classes/")
        names (paths/find-members p "*.cls")]
    (zipmap
     (map string/lower-case names)
     (pmap #(assoc (load-class p %) :name %) names))))

(defn load-objects
  [root]
  (let [p (paths/get-path root "objects/")
        names (paths/find-members p "*.object")
        load-fn #(load-meta-data-file p % "object" com.sforce.soap._2006._04.metadata.CustomObject)]
    (->> (map load-fn names)
         (zipmap names)
         (schema/custom-meta-data-map))))

;; -- loading packages

(defn pkg-metadata
  [pkg-path] ;; input is the path to the package.xml
  (let [pkg (load-metadata pkg-path com.sforce.soap._2006._04.metadata.Package)
        prefix (if-let [config (config/load-config pkg-path)]
                 (:prefix config)
                 (.getNamespacePrefix pkg))]
    {:prefix prefix
     :package pkg
     :root-path (.getParent pkg-path)}))

(defn load-package
  [pkg-meta]
  (timbre/debug (str "loading package " (:prefix pkg-meta) " from " (:root-path pkg-meta)))
  (assoc pkg-meta
         :classes (load-classes (:root-path pkg-meta))
         :sobjects (load-objects (:root-path pkg-meta))))

(defn prefix
  [pkg-meta]
  (when-let [pre (:prefix pkg-meta)] (string/lower-case pre)))

(defn load-packages
  [starting-dir]
  (let [paths (paths/find-all "package.xml" (paths/get-path starting-dir))]
    (into {} (map
              (fn [path]
                (let [pkg (load-package (pkg-metadata path))
                      ns (prefix pkg)]
                  [ns pkg]))
              paths))))

;; -- querying

(defn get-ast
  "Return the ast for [namespace class-name inner-class-name] in pkgs."
  [pkgs [n1 n2 n3]]
  (let [cls (get-in pkgs [n1 :classes n2])]
    (if-let [index (get-in cls [:inner-classes n3 :member-index])]
      (get-in cls [:ast :body index])
      (:ast cls))))

(defn exists?
  [pkgs [n1 n2 n3 :as qname]]
  (if (= 1 (count qname))
    (contains? pkgs n1)
    (get-ast pkgs qname)))

(defn classes
  "Returns a list of qualified names of all classes and inner classes in a package. Each element has the format [namespace class-name inner-class-name]."
  ([pkgs namespace]
   (s/select [(s/putval namespace) (s/keypath namespace)
              :classes s/ALL (s/pred #(empty (:parse-errors (% 1)))) ;; all classes without parse errors
              (s/multi-path s/FIRST                                                         ;; class names
                [(s/collect-one s/FIRST) s/LAST :inner-classes s/ALL s/FIRST])] ;; inner class names
     pkgs))
  ([pkgs]
   (->> (keys pkgs)
     (mapcat #(classes pkgs %)))))

(defn inner-classes
  [pkgs [namespace name inner :as qname]]
  (if (nil? inner)
    (s/select [(s/putval namespace) (s/keypath namespace)
               :classes
               (if name
                 [(s/keypath name) (s/putval name)
                  (s/pred #(empty (:parse-errors %)))
                  :inner-classes s/ALL s/FIRST]
                 [(s/pred #(empty (:parse-errors %))) s/MAP-KEYS])] ;; inner class names
              pkgs)
    []))

(defn outer-classes
  [pkgs namespace]
  (s/select [(s/putval namespace) (s/keypath namespace)
             :classes s/ALL (s/pred #(empty (:parse-errors (% 1)))) ;; all classes without parse errors
             (s/multi-path s/FIRST)]
            pkgs))



