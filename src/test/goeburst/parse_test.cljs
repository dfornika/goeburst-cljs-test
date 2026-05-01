(ns goeburst.parse-test
  (:require [cljs.test :refer [deftest is testing]]
            [goeburst.parse :as parse]))

(deftest parse-header-test
  (testing "returns trimmed IDs from header row"
    (is (= ["A" "B" "C"] (parse/parse-header ["" "A" "B" "C"]))))
  (testing "trims whitespace from each ID"
    (is (= ["A" "B"] (parse/parse-header ["" " A " " B "]))))
  (testing "throws on empty header"
    (is (thrown? js/Error (parse/parse-header [""])))))

(deftest parse-data-row-test
  (testing "parses a valid row of integers"
    (is (= [0 1 2] (parse/parse-data-row ["A" "0" "1" "2"] 1 3))))
  (testing "parses a row with whitespace-padded cells"
    (is (= [0 1] (parse/parse-data-row ["A" " 0 " " 1 "] 1 2))))
  (testing "throws when row has too few columns"
    (is (thrown? js/Error (parse/parse-data-row ["A" "0"] 1 3))))
  (testing "throws when row has too many columns"
    (is (thrown? js/Error (parse/parse-data-row ["A" "0" "1" "2" "3"] 1 3))))
  (testing "throws on non-integer cell value"
    (is (thrown? js/Error (parse/parse-data-row ["A" "x" "1" "2"] 1 3)))))

(deftest parse-distance-matrix-test
  (testing "parses a valid TSV 2x2 matrix"
    (let [result (parse/parse-distance-matrix "\tA\tB\nA\t0\t1\nB\t1\t0")]
      (is (= ["A" "B"] (:ids result)))
      (is (= [[0 1] [1 0]] (:matrix result)))))
  (testing "parses a valid CSV 2x2 matrix"
    (let [result (parse/parse-distance-matrix ",A,B\nA,0,1\nB,1,0")]
      (is (= ["A" "B"] (:ids result)))
      (is (= [[0 1] [1 0]] (:matrix result)))))
  (testing "parses a 1x1 matrix"
    (let [result (parse/parse-distance-matrix "\tA\nA\t0")]
      (is (= ["A"] (:ids result)))
      (is (= [[0]] (:matrix result)))))
  (testing "throws on empty input"
    (is (thrown? js/Error (parse/parse-distance-matrix ""))))
  (testing "throws on blank-only input"
    (is (thrown? js/Error (parse/parse-distance-matrix "\n\n"))))
  (testing "throws when matrix is not square (missing rows)"
    (is (thrown? js/Error (parse/parse-distance-matrix "\tA\tB\nA\t0\t1"))))
  (testing "throws on malformed cell value"
    (is (thrown? js/Error (parse/parse-distance-matrix "\tA\tB\nA\t0\tx\nB\t1\t0")))))
