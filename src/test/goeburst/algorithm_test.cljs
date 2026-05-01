(ns goeburst.algorithm-test
  (:require [cljs.test :refer [deftest is testing]]
            [goeburst.algorithm :as algo]))

;; ---------------------------------------------------------------------------
;; Test fixtures
;; ---------------------------------------------------------------------------

(def m-2x2-slv
  "Two STs that are SLVs of each other."
  [[0 1]
   [1 0]])

(def m-2x2-dlv
  "Two STs that are DLVs – no SLV link, so each ST is its own CC."
  [[0 2]
   [2 0]])

(def m-two-ccs
  "STs 0 and 1 are SLVs (one CC); ST 2 is isolated (its own CC).
   Globally, ST 2 is a DLV of both ST 0 and ST 1."
  [[0 1 2]
   [1 0 2]
   [2 2 0]])

(def m-3x3-chain
  "0–1 are SLVs; 1–2 are SLVs; so 0, 1, 2 are all in the same CC.
   0–2 are DLVs within the CC."
  [[0 1 2]
   [1 0 1]
   [2 1 0]])

;; ---------------------------------------------------------------------------
;; slv-components
;; ---------------------------------------------------------------------------

(deftest slv-components-test
  (testing "two SLV-linked STs share a component"
    (let [comp (algo/slv-components m-2x2-slv)]
      (is (= (comp 0) (comp 1)))))
  (testing "two DLV-only STs are in separate components"
    (let [comp (algo/slv-components m-2x2-dlv)]
      (is (not= (comp 0) (comp 1)))))
  (testing "correctly identifies two CCs in a 3-ST matrix"
    (let [comp (algo/slv-components m-two-ccs)]
      (is (= (comp 0) (comp 1)) "STs 0 and 1 must be in the same CC")
      (is (not= (comp 0) (comp 2)) "ST 2 must be in a different CC")))
  (testing "chain of SLVs produces one component"
    (let [comp (algo/slv-components m-3x3-chain)]
      (is (= (comp 0) (comp 1) (comp 2))))))

;; ---------------------------------------------------------------------------
;; cc-relative-counts
;; ---------------------------------------------------------------------------

(deftest cc-relative-counts-test
  (testing "SLV within the same CC is counted"
    (let [counts (algo/cc-relative-counts m-2x2-slv)]
      (is (= {:slv 1 :dlv 0 :tlv 0} (counts 0)))
      (is (= {:slv 1 :dlv 0 :tlv 0} (counts 1)))))

  (testing "DLV crossing a CC boundary is NOT counted"
    ;; In m-two-ccs, ST 2 is a DLV of ST 0 globally, but they are in different CCs.
    (let [counts (algo/cc-relative-counts m-two-ccs)]
      (is (= {:slv 1 :dlv 0 :tlv 0} (counts 0)) "ST 0: 1 SLV (ST 1); ST 2 excluded")
      (is (= {:slv 1 :dlv 0 :tlv 0} (counts 1)) "ST 1: 1 SLV (ST 0); ST 2 excluded")
      (is (= {:slv 0 :dlv 0 :tlv 0} (counts 2)) "ST 2: isolated")))

  (testing "DLV within the same CC is counted"
    ;; In m-3x3-chain: 0–1 SLVs, 1–2 SLVs, 0–2 DLVs. All in same CC.
    (let [counts (algo/cc-relative-counts m-3x3-chain)]
      (is (= 1 (:slv (counts 0))) "ST 0: SLV of ST 1")
      (is (= 1 (:dlv (counts 0))) "ST 0: DLV of ST 2, same CC")
      (is (= 2 (:slv (counts 1))) "ST 1: SLV of both ST 0 and ST 2")
      (is (= 0 (:dlv (counts 1)))))))

;; ---------------------------------------------------------------------------
;; kruskal / run – integration-level checks
;; ---------------------------------------------------------------------------

(deftest run-test
  (testing "two SLV-linked STs produce exactly one edge"
    (let [{:keys [edges]} (algo/run {:ids ["A" "B"] :matrix m-2x2-slv})]
      (is (= 1 (count edges)))
      (is (= 1 (:d (first edges))))))

  (testing "two DLV-linked STs produce one DLV edge"
    ;; The current implementation connects at up to TLV distance (multi-level mode).
    (let [{:keys [edges]} (algo/run {:ids ["A" "B"] :matrix m-2x2-dlv})]
      (is (= 1 (count edges)))
      (is (= 2 (:d (first edges))))))

  (testing "node SLV/DLV counts are CC-relative, not global"
    (let [{:keys [nodes]} (algo/run {:ids ["A" "B" "C"] :matrix m-two-ccs})
          by-id (into {} (map (juxt :id identity) nodes))]
      (is (= 1 (:slv (by-id "A"))) "A has 1 SLV (B)")
      (is (= 0 (:dlv (by-id "A"))) "C is a global DLV of A but in a different CC")
      (is (= 0 (:slv (by-id "C"))) "C is isolated")))

  (testing "chain of 3 STs produces 2 edges forming a spanning tree"
    (let [{:keys [edges]} (algo/run {:ids ["A" "B" "C"] :matrix m-3x3-chain})]
      (is (= 2 (count edges))))))
