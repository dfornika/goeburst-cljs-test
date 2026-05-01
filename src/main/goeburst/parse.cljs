(ns goeburst.parse
  (:require [clojure.string :as str]))

(defn- parse-row [line]
  (str/split line #"\t|," -1))

(defn- invalid-distance-matrix [message data]
  (throw (ex-info message data)))

(defn- parse-distance-cell [value row-index col-index]
  (let [trimmed (str/trim value)]
    (when-not (re-matches #"^-?\d+$" trimmed)
      (invalid-distance-matrix
       (str "Invalid distance value at row " row-index ", column " col-index ": " (pr-str value))
       {:row row-index :column col-index :value value}))
    (js/parseInt trimmed 10)))

(defn parse-distance-matrix
  "Parse a TSV or CSV distance matrix into {:ids [...] :matrix [[...]]}.
   First row and first column are treated as ST identifiers.
   Cell values are integer allelic distances."
  [text]
  (let [lines (->> (str/split-lines text)
                   (remove str/blank?))
        _     (when (empty? lines)
                (invalid-distance-matrix
                 "Distance matrix is empty."
                 {:text text}))
        rows  (mapv parse-row lines)
        ids   (->> (first rows) rest (mapv str/trim))
        _     (when (empty? ids)
                (invalid-distance-matrix
                 "Distance matrix header must contain at least one identifier."
                 {:header (first rows)}))
        matrix (->> (rest rows)
                    (mapv (fn [row-index row]
                            (when-not (= (count row) (inc (count ids)))
                              (invalid-distance-matrix
                               (str "Row " row-index " has " (dec (count row))
                                    " distance value(s); expected " (count ids) ".")
                               {:row row-index
                                :expected-columns (inc (count ids))
                                :actual-columns (count row)
                                :row-data row}))
                            (->> (rest row)
                                 (mapv (fn [col-index v]
                                         (parse-distance-cell v row-index col-index))
                                       (range 1 (inc (count ids)))))))
                    (range 1 (inc (count (rest rows)))))]
    (when-not (= (count matrix) (count ids))
      (invalid-distance-matrix
       (str "Distance matrix is not square: header defines " (count ids)
            " identifier(s) but found " (count matrix) " data row(s).")
       {:ids-count (count ids)
        :row-count (count matrix)}))
    {:ids ids :matrix matrix}))
