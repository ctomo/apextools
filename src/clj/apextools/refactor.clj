(ns apextools.refactor
  (:require [clojure.string :as string]
            [apextools.paths :as paths]
            [apextools.package :as pkg]
            [apextools.ast :as ast]))

(defn- chunk-between [src first second]
  (cond
    (and first second) (.substring src (:end first) (:start second))
    (and first (not second)) (.substring src (:end first) (count src))
    (and (not first) second) (.substring src 0 (:start second))
    :else src))

(defn replace-with [src all-replacements]
  (string/join
   (loop [replacements (sort #(< (:start %1) (:start %2)) all-replacements)
          result []
          frst nil]
     (if (seq replacements)
       (recur (rest replacements)
              (conj result (chunk-between src frst (first replacements)) (:replacement (first replacements)))
              (first replacements))
       (conj result (chunk-between src (last all-replacements) nil))))))

