(ns goeburst.parse
  (:require [clojure.string :as str]))

(defn- parse-row [line]
  (str/split line #"\t|,"))

(defn parse-distance-matrix
  "Parse a TSV or CSV distance matrix into {:ids [...] :matrix [[...]]}.
   First row and first column are treated as ST identifiers.
   Cell values are integer allelic distances."
  [text]
  (let [lines   (->> (str/split-lines text)
                     (remove str/blank?))
        rows    (map parse-row lines)
        ids     (->> (first rows) rest (mapv str/trim))
        matrix  (->> (rest rows)
                     (mapv (fn [row]
                             (->> (rest row)
                                  (mapv (fn [v] (js/parseInt (str/trim v) 10)))))))]
    {:ids ids :matrix matrix}))
