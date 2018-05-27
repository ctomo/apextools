(ns apextools.helper.api
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.data :as data]
            [apextools.ast :as ast]
            [com.rpl.specter :as s]
            [taoensso.nippy :as nippy]
            [byte-streams :as streams]
            [cheshire.core :as json]))

(def default (with-open [in (io/input-stream (io/resource "api40.bin"))]
               (nippy/thaw (streams/to-byte-array in))))

(defn- gen-identifier-node [s]
  (ast/make-node :identifier {:value s} nil))

(defn- gen-modifier-node [k]
  (ast/make-node :modifier {:value k} nil))

(defn- gen-qualified-name-node [v]
  (ast/make-node :qualified-name {:identifiers (mapv gen-identifier-node v)} nil))

(defn- gen-super-data-type-node [v]
  (ast/make-node :data-type {:name (gen-qualified-name-node v)
                             :parameters nil} nil))

(defn- fix-identifier [id]
  (case (string/upper-case id)
    "ANY" "Object"
    "APEX_OBJECT" "Object"
    id))

(defn- gen-data-type-node [str]
  (let [node (:ast (ast/gen-ast "type" str))]
    (s/transform (ast/ast-walker #(= :identifier (:node-category %)))
                 (fn [m] (if (map? m)
                           (update m :value fix-identifier)
                           m))
                 node)))

(defn- gen-method-parameter-node [p]
  (ast/make-node :method-parameter {:final false
                                    :data-type (gen-data-type-node (p "type"))
                                    :identifier (gen-identifier-node (p "name"))} nil))

(defn gen-member-modifiers [m]
  (->> (when (or (m "static") (m "isStatic")) [(gen-modifier-node :static)])
       (concat [(gen-modifier-node :global)])
       (into [])))

(defn- gen-method-node [is-interface m]
  (->> (ast/make-node :method-declaration {:interface is-interface
                                           :stubbed true
                                           :modifiers (gen-member-modifiers m)
                                           :identifier (gen-identifier-node (m "name"))
                                           :parameters (mapv gen-method-parameter-node (m "parameters"))} nil)
       (into (if-let [type (m "returnType")]
               {:data-type (gen-data-type-node type)}
               {:ctor true}))))

(defn- gen-property-node [m]
  (ast/make-node :property {:modifiers (gen-member-modifiers m)
                            :data-type (gen-data-type-node (m "type"))
                            :identifier (gen-identifier-node (m "name"))
                            :getter (ast/make-node :property-getter {:modifier nil
                                                                     :statement-block nil} nil)
                            :setter (ast/make-node :property-setter {:modifier nil
                                                                     :statement-block nil} nil)}))

(def enum-signature
  {"methods"
   {"hashcode"
    [{"returnType" "Integer",
      "argTypes" [],
      "name" "hashCode",
      "isStatic" false}],
    "ordinal"
    [{"returnType" "Integer",
      "argTypes" [],
      "name" "ordinal",
      "isStatic" false}],
    "values"
    [{"argTypes" [],
      "name" "values",
      "isStatic" true}],
    "equals"
    [{"returnType" "Boolean",
      "argTypes" ["Object"],
      "name" "equals",
      "isStatic" false}]},
   "isInterface" false})

(defn- is-enum [m]
  (nil? (nth (data/diff m enum-signature) 1)))

(defn- class-type [m]
  (cond
    (m "isInterface") :interface
    (is-enum m) :enum
    :else :class))

(defn- gen-unit-node
  "Generate an AST map of class description m and inner class descriptions coll."
  [m coll]
  (let [type (class-type m)
        is-interface (m "isInterface")]
    (->> (ast/make-node :type-declaration {:type type
                                           :identifier (gen-identifier-node (m "name"))
                                           :modifiers [(gen-modifier-node :global)]} nil)
         (merge (if (= :enum type)
                  {:constants (s/select [(s/keypath "properties")
                                         s/ALL
                                         (s/keypath "name")
                                         (s/view gen-identifier-node)] m)}
                  {:type-parameters nil
                   :super-class (when-let [s (m "super")] (gen-super-data-type-node s))
                   :interfaces nil
                   :stubbed true
                   :body (reduce into [(mapv #(gen-unit-node % []) coll)
                                       (s/select [(s/keypath "properties") s/ALL (s/pred #(% "type"))
                                                  (s/view gen-property-node)] m)
                                       (s/select [(s/multi-path [(s/keypath "methods") s/MAP-VALS]
                                                                (s/keypath "constructors"))
                                                  s/ALL (s/view #(gen-method-node is-interface %))] m)])})))))

;; structure data

(defn- outer-class?
  [[k v]]
  (not (string/includes? k ".")))

(defn- inner-class-of?
  [outer-name]
  (fn [[k v :as cls]]
    (let [[n1 n2] (string/split k #"\.")]
      (and n2 (.equalsIgnoreCase outer-name n1)))))

(defn- classes-path
  [ns pred]
  (s/path [(s/keypath ns "classes") s/ALL (s/pred pred)]))

(defn- gen-api-class
  [cls inners]
  (let [ast (gen-unit-node cls inners)]
    {:inner-classes (ast/extract-inner-classes ast)
     :ast ast
     :parse-errors []}))

(defn- inner-classes
  "Get a list of inner class definitions."
  [apex-api-json ns outer-name]
  (s/select [(classes-path ns (inner-class-of? outer-name)) s/LAST] apex-api-json))

(defn- gen-api-package
  [ns apex-api-json]
  (let [class-defs (fn [[n v]]
                     [n (gen-api-class v (inner-classes apex-api-json ns n))])]
    {:prefix ""
     :package ns
     :classes (into {} (s/select [(classes-path ns outer-class?)
                                  (s/view class-defs)] apex-api-json))}))

(defn- gen-api-packages
  [apex-api-json]
  (into {} (pmap
            (fn [ns]
              [ns (gen-api-package ns apex-api-json)])
            (keys apex-api-json))))

(defn load-default-apex-api [json-api-path]
  (->> (json/parse-string (slurp json-api-path))
       (gen-api-packages)))

(defn save-default-apex-api-file
  [options]
  (->> (load-default-apex-api (:path options))
       (nippy/freeze-to-file "api.bin")))

