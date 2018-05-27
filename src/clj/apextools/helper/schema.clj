(ns apextools.helper.schema
  (:require [clojure.string :as string]
            [com.rpl.specter :as s]
            [taoensso.nippy :as nippy]
            [clojure.java.io :as io]
            [byte-streams :as streams]
            [cheshire.core :as json]))

(def default (with-open [in (io/input-stream (io/resource "schema40.bin"))]
               (nippy/thaw (streams/to-byte-array in))))

(defn deep-merge [a b]
  (cond
    (or (every? sequential? [a b]) (every? set? [a b])) (into a b)
    (every? map? [a b]) (merge-with deep-merge a b)
    :else b))

(defmacro func [fname]
  `(s/parser #(~fname %) #(identity %)))

(defmulti to-map type)

(defmethod to-map com.sforce.soap._2006._04.metadata.CustomField [fld]
  (let [m {"name" (.getFullName fld)
           "relationshipName" (when (.getReferenceTo fld) (.replace (.getFullName fld) "__c" "__r"))
           "referenceTo" (.getReferenceTo fld)}
        type (-> fld .getType .value)]
    (if (not= "Summary" type)
      (assoc m "type" type)
      (assoc m "summaryOperation" (-> fld .getSummaryOperation .value)
             "summarizedField" (-> fld .getSummarizedField)))))

(defmethod to-map com.sforce.soap._2006._04.metadata.CustomObject [obj]
  {"fields" (into {} (map
                      (fn [fld] [(->> (.getFullName fld) (string/lower-case)) (to-map fld)])
                      (.getFields obj)))})

(defn- child-relationships [objects]
  (->> (s/select [s/ALL (s/collect-one s/FIRST) s/LAST (func .getFields) s/ALL (s/pred #(not (nil? (.getReferenceTo %))))] objects)
       (mapv (fn [[name fld]] {(.getReferenceTo fld) [{"relationshipName" (.getRelationshipName fld)
                                                       "childSObject" name}]}))
       (reduce #(merge-with into %1 %2) {})))


(defn custom-meta-data-map [objects]
  (let [cr (child-relationships objects)]
    (s/transform [s/ALL] (fn [[name obj]]
                           [(string/lower-case name)
                            (-> (to-map obj)
                                (assoc "name" name
                                       "childRelationships" (cr name)))])
                 objects)))

(defn- translate-field-type
  [type]
  (when type
    (case (string/lower-case type)
      "reference" "Id"
      "string" "String"
      "picklist" "String"
      "phone" "String"
      "url" "String"
      "email" "String"
      "textarea" "String"
      "combobox" "String"
      "date" "Date"
      "datetime" "Datetime"
      "boolean" "Boolean"
      "address" "Address"
      "double" "Double"
      "int" "Integer"
      "currency" "Decimal"
      "percent" "Decimal"

      "lookup" "Id"
      "masterdetail" "Id"
      "externallookup" "String"
      "indirectlookup" "String"
      "checkbox" "Boolean"
      "number" "Decimal"
      "autonumber" "String"
      "text" "String"
      "longtextarea" "String"
      "html" "String"
      "encryptedtext" "String"
      nil)))

(defn- field-meta
  [objects object-name field-name]
  (get-in objects [(string/lower-case object-name)
                   "fields"
                   (string/lower-case field-name)]))

(declare apex-type)

(defn- summarized-field-type
  [objects summarized-field]
  (when summarized-field
    (let [[object-name field-name] (string/split summarized-field #"\.")]
      (apex-type objects object-name field-name))))

;; FIXME: consider default fields for custom objects: Name, Id
(defn apex-type
  "Return the Apex type of field-name from object-name."
  [objects object-name field-name]
  (let [meta (field-meta objects object-name field-name)
        type (get-in meta ["type"])
        summarized-field (get-in meta ["summarizedField"])
        summary-op (get-in meta ["summaryOperation"])]
    (if-let [atype (translate-field-type type)]
      atype
      (when summary-op
        (if (not= "count" summary-op)
          (summarized-field-type objects summarized-field)
          "Decimal")))))

(defn save-default-schema-file
  "Create the default schema.bin file."
  [options]
  (->> (json/parse-string (slurp (:path options)))
       (nippy/freeze-to-file "schema.bin")))
