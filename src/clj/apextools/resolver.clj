(ns apextools.resolver
  (:require [clojure.pprint :as pp]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [com.rpl.specter :as s]
            [taoensso.timbre :as timbre]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]
            [apextools.ast :as ast]
            [apextools.package :as pkg]
            [apextools.helper.api :as api]
            [apextools.helper.schema :as schema]))


(defn qualified-name
  "Extract the name of the data type from the ast node as a vector of strings. (no parameters)"
  [n]
  (case (:node-category n)
    :array-type ["list"]
    :data-type (mapv (fn [id] (string/lower-case (:value id)))
                     (->> n :name :identifiers))
    nil))

(defn custom-object-qname
  "Return the fully qualified name for name referenced from namespace."
  [namespace name]
    (if (= 2 (count (string/split name #"__")))
      ["schema" (if (string/blank? namespace)
                  name
                  (str namespace "__" name))]
      ["schema" name]))


;; Inheritance of classes


(defn- super-class-type-node
  "Return the data-type AST node for the super class."
  [pkgs qname]
  (:super-class (pkg/get-ast pkgs qname)))

(defn- derived-class?
  "Checks if the class has a super class."
  [pkgs qname]
  (not (nil? (super-class-type-node pkgs qname))))

(defn- super-class-qname
  "Returns the qualified name of the super-class datatype node."
  [pkgs qname]
  (qualified-name (super-class-type-node pkgs qname)))


(defn- match-name
  [g [n1 n2 n3 :as raw-qname]]
  (let [l (count raw-qname)]
    (fn [[x y z :as w]]
      (when (or (and (= 1 l) (= w [x y n1]))
                (and (= 2 l) (= w [x n1 n2]))
                (and (= 3 l) (= w raw-qname)))
        (if (uber/has-node? g w)
          w
          :unresolvable)))))

(defn match-context
  [pkgs g [m1 m2 m3 :as qname] raw-qname]
  (let [context-types (if m3 (pkg/inner-classes pkgs [m1 m2])
                          (pkg/inner-classes pkgs qname))]
    (some (match-name g raw-qname) (cons qname context-types))))

(defn resolve-class-name
  "Return the fully qualified name for qname relative to qname-context."
  [pkgs g [namespace name inner :as context-qname] [n1 n2 n3 :as raw-qname]]
  (when (pkg/exists? pkgs context-qname)
    (let [match (fn [len qname]
                  (when (and (= (count raw-qname) len)
                             (uber/has-node? g qname))
                    qname))
          match-traverse (fn [start-context]
                           (some (fn [qname] (match-context pkgs g qname raw-qname))
                                 (alg/pre-traverse g start-context)))
          resolved (match-traverse context-qname)
          resolved-outer-supers (match-traverse [namespace name])]
      (or
       ;; 1. match types in current context + super classes 
       (when (not= :unresolvable resolved) resolved)
       ;; 2. when context is an inner class look at the outer class supers
       (when (not= :unresolvable resolved-outer-supers) resolved-outer-supers)
       ;; 3. other
       (when (and (nil? resolved) (nil? resolved-outer-supers))
         (or (match 1 [namespace n1])
             (match 1 [namespace name n1])
             (match 1 ["system" n1])
             (match 1 (custom-object-qname namespace n1))
             (match 2 [namespace n1 n2])
             (match (count raw-qname) raw-qname)))))))
  

(defn unresolved-classes
  "Return all names of unresolved classes in pkgs."
  [pkgs g]
  (->> (keys pkgs)
       (pmap #(pkg/classes pkgs %))
       (reduce into)
       (remove #(uber/has-node? g %))))

(defn- update-inheritance-add-custom-sobject-classes
  "Add custom sobject classes to inheritance graph."
  [pkgs g]
  (->> (s/select [s/ALL (s/collect-one s/FIRST) s/LAST :sobjects s/MAP-KEYS] pkgs)
       (map (fn [[namespace sobj]] [(custom-object-qname namespace sobj) ["system" "sobject"]]))
       (into #{})
       (uber/add-edges* g)))

(defn- update-inheritance-add-default-sobject-classes
  "Add default sobject classes to inheritance graph."
  [g]
  (->> (keys schema/default)
       (map (fn [name] [["schema" name] ["system" "sobject"]]))
       (into #{})
       (uber/add-edges* g)))

(defn- update-inheritance-add-top-classes
  "Add apex loner classes to graph."
  [pkgs g]
  (->> (unresolved-classes pkgs g)
       (remove #(derived-class? pkgs %))
       (uber/add-nodes* g)))

(defn- resolve-derived-type
  [pkgs g [n1 n2 n3 :as qname]]
  (when-let [s (resolve-class-name pkgs g [n1 n2] (super-class-qname pkgs qname))]
    [qname s]))

(defn- update-inheritance-add-derived-classes
  "Add inheritance edges."
  [pkgs g]
  (->> (unresolved-classes pkgs g)
       (filter #(derived-class? pkgs %)) ;; super resolve-able
       (map #(resolve-derived-type pkgs g %))
       (filter vector?)
       (uber/add-edges* g)))

(defn calculate-inheritance-graph
  "Calculate a graph where vertices are the fully qualified class names and the edges describe the inheritance."
  ([pkgs g]
   (let [g2 (update-inheritance-add-derived-classes pkgs g)]
     (if (= g g2)
       g
       (recur pkgs g2))))
  ([pkgs]
   (->> (uber/digraph ["void"] ["object"])
        (update-inheritance-add-top-classes pkgs)
        (update-inheritance-add-default-sobject-classes)
        (update-inheritance-add-custom-sobject-classes pkgs)
        (calculate-inheritance-graph pkgs))))

(defn resolve-fn
  [pkgs]
  (let [g (calculate-inheritance-graph pkgs)]
    (fn [& coll]
      (let [context (butlast coll)
            node (last coll)
            raw-qname (qualified-name node)
            resolved-qname (resolve-class-name pkgs g context raw-qname)]
        (when-not resolved-qname
          (timbre/debug
           (str "unable to resolve " raw-qname " in context " context)))
        (assoc node :fully-qualified-name resolved-qname)))))

(defn resolve-types
  "Resolve the fully qualified names of all data type nodes. (all packages)"
  [pkgs]
  (s/transform [s/ALL (s/collect-one s/FIRST) s/LAST ;; namespace
                :classes
                s/MAP-VALS
                :ast
                (ast/ast-class-context-walker ast/data-type-node?)]
    (resolve-fn pkgs)
    pkgs))


;; ===== symbol tables ================================================================================================

(s/defnav
  ^{:doc "Vector: Navigates to a vector of `[index value]`."}
  ALL-INDEXED
  []
  (select* [this structure next-fn]
    (mapcat next-fn (map-indexed vector structure)))
  (transform* [this structure next-fn]
    (->> (map-indexed vector structure)
      (mapv next-fn)
      (mapv last)
      doall)))

(def key-accumulator-walker
  (s/recursive-path [f] p
    (s/if-path coll?
      (s/if-path f
        s/STAY ;; goal
        [(s/if-path vector? ALL-INDEXED s/ALL)
         (s/collect-one s/FIRST) s/LAST p]))))


(defn type-signature
  "Return the type signature of a data-type ast node as a map {:fully-qualified-name ..., :parameters ...} (fully qualified names needed)."
  [n]
  (let [sgn {:fully-qualified-name (:fully-qualified-name n)}]
    (if-let [params (seq (:parameters n))]
      (assoc sgn :parameters (mapv type-signature params))
      sgn)))


(defn property-table
  "Returns for a class-body ast the symbol table: property-id-key --> path/name/type."
  [class-body]
  (->> class-body
    (keep-indexed #(when (ast/property-node? %2)
                     [(ast/get-id-key %2) {:path [%1]
                                           :name (ast/get-id %2)
                                           :type (type-signature (:data-type %2))}]))
    (into {})))

(defn variable-table
  "Returns for a class body ast the symbol table: variable-id-key --> path/name/type."
  [block]
  (->> block
    (s/select [ALL-INDEXED (s/pred (fn [[i n]] (ast/variable-declaration-node? n))) (s/collect-one s/FIRST) s/LAST
               (s/collect-one :data-type)
               :initializations ALL-INDEXED (s/collect-one s/FIRST) s/LAST])
    (map (fn [[i data-type j var]]
           [(ast/get-id-key var) {:path [i :initializations j]
                                  :name (ast/get-id var)
                                  :type (type-signature data-type)}]))
    (into {})))

(defn- variable-inits
  [v]
  (let [node (peek v) ;; the first part of the vector is the keypath to the node inside the AST
        path (pop v)] ;; the last element is the declaration node
    (->> (s/select [(s/collect-one :data-type)
                    :initializations ALL-INDEXED
                    (s/collect-one s/FIRST) s/LAST] node)
      (mapv (fn [[data-type idx init]]
              [(ast/get-id-key init) {:path (conj path :initializations idx)
                                      :name (ast/get-id init)
                                      :type (type-signature data-type)}])))))

(defn add-block-var [m [k v]]
  (update-in m [k]
    #(if (nil? %1)
       [%2]
       (conj %1 %2))
    v))

(defn variable-table-stmt-block
  "Returns for a statement block ast the symbol table: variable-id-key --> path/name/type."
  [block]
  (->> block
    (s/select [(key-accumulator-walker ast/variable-declaration-node?)])
    (map variable-inits)
    (reduce into [])
    (reduce add-block-var {})))

(defn stmt-block-variable-tables
  "Returns a map: path --> variable symbol table."
  [body]
  (->> body
    (s/select [(key-accumulator-walker ast/block-statement?) :statements])
    (filter #(not= :body (second %)))               ;; skip inner class bodies
    (map (fn [v]
           [(pop v)                                 ;; = path
            (variable-table-stmt-block (peek v))])) ;; = variable table
    (into {})))

(defn- add-method [m [i s]]
  (let [sig (mapv #(type-signature (:data-type %)) (:parameters s))]
    (assoc-in m [(ast/get-id-key s) sig] {:path [i]
                                          :name (ast/get-id s)
                                          :type (type-signature (:data-type s))})))

(defn method-table
  [class-body]
  "Returns for a class-body ast the method symbol table: method-id-key --> arg-types --> path/name/type."
  (->> (map-indexed vector class-body)
    (filter #(ast/method-node? (second %)))
    (reduce add-method {})))

(defn symbol-table
  [pkgs qname]
  (let [body (:body (pkg/get-ast pkgs qname))]
    {:methods (method-table body)
     :properties (property-table body)
     :class-variables (variable-table body)
     :block-variables (stmt-block-variable-tables body)}))

(defn symbol-tables
  [pkgs]
  (let [qnames (pkg/classes pkgs)]
    (zipmap
     qnames
     (map #(symbol-table pkgs %) qnames))))


;; ====================================================================================================================

(defn resolve-all
  "Resolve identifiers from ASTs."
  [pkgs]
  (let [prt (resolve-types (into pkgs api/default))]
    {:packages prt
     :inheritance-graph (calculate-inheritance-graph prt)
     :symbol-tables (symbol-tables prt)}))


