(ns span.parse
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

(defn parse-header [first-row]
  (let [ids (mapv str/trim (rest first-row))]
    (when (empty? ids)
      (invalid-distance-matrix
       "Distance matrix header must contain at least one identifier."
       {:header first-row}))
    ids))

(defn parse-data-row [row row-index expected-count]
  (when-not (= (count row) (inc expected-count))
    (invalid-distance-matrix
     (str "Row " row-index " has " (dec (count row))
          " distance value(s); expected " expected-count ".")
     {:row row-index
      :expected-columns (inc expected-count)
      :actual-columns (count row)
      :row-data row}))
  (->> (rest row)
       (map-indexed (fn [i v]
                      (parse-distance-cell v row-index (inc i))))
       vec))

(defn parse-distance-matrix
  "Parse a TSV or CSV distance matrix into {:ids [...] :matrix [[...]]}.
   First row and first column are treated as ST identifiers.
   Cell values are integer allelic distances."
  [text]
  (let [lines (->> (str/split-lines text)
                   (remove str/blank?))]
    (when (empty? lines)
      (invalid-distance-matrix
       "Distance matrix is empty."
       {:text text}))
    (let [rows   (mapv parse-row lines)
          ids    (parse-header (first rows))
          matrix (->> (rest rows)
                      (map-indexed (fn [i row]
                                     (parse-data-row row (inc i) (count ids))))
                      vec)]
      (when-not (= (count matrix) (count ids))
        (invalid-distance-matrix
         (str "Distance matrix is not square: header defines " (count ids)
              " identifier(s) but found " (count matrix) " data row(s).")
         {:ids-count (count ids)
          :row-count (count matrix)}))
      {:ids ids :matrix matrix})))
