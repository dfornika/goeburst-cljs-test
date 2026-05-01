(ns goeburst.algorithm)

;; ---------------------------------------------------------------------------
;; Neighbor counts
;; ---------------------------------------------------------------------------

(defn neighbor-counts
  "For each ST index, count how many other STs are at distance 1 (SLV),
   2 (DLV), and 3 (TLV) according to the distance matrix."
  [matrix]
  (let [n (count matrix)]
    (mapv (fn [i]
            (reduce (fn [acc j]
                      (if (= i j)
                        acc
                        (let [d (get-in matrix [i j])]
                          (cond
                            (= d 1) (update acc :slv inc)
                            (= d 2) (update acc :dlv inc)
                            (= d 3) (update acc :tlv inc)
                            :else acc))))
                    {:slv 0 :dlv 0 :tlv 0}
                    (range n)))
          (range n))))

;; ---------------------------------------------------------------------------
;; Edge priority (total order for Kruskal's)
;; ---------------------------------------------------------------------------

(defn- edge-priority
  "Return a vector that, when compared lexicographically, implements the
   goeBURST total order.  Lower is better (we sort ascending and pick first).

   Rules (in order):
   1. Allelic distance d  – lower wins  (SLV=1 < DLV=2 < TLV=3)
   2. max SLV count of the two endpoints – higher wins  (negate)
   3. max DLV count – higher wins
   4. max TLV count – higher wins
   5. max occurrence frequency – higher wins  (placeholder: always 1)
   6. min ST index – lower wins"
  [counts [i j d]]
  (let [ci (counts i)
        cj (counts j)]
    [d
     (- (max (:slv ci) (:slv cj)))
     (- (max (:dlv ci) (:dlv cj)))
     (- (max (:tlv ci) (:tlv cj)))
     (- (max 1 1))                     ; frequency placeholder
     (min i j)]))

;; ---------------------------------------------------------------------------
;; Union-Find (path-compressed)
;; ---------------------------------------------------------------------------

(defn- make-uf [n]
  (vec (range n)))

(defn- uf-find [uf x]
  (if (= (uf x) x)
    [uf x]
    (let [[uf root] (uf-find uf (uf x))]
      [(assoc uf x root) root])))

(defn- uf-union [uf x y]
  (let [[uf rx] (uf-find uf x)
        [uf ry] (uf-find uf y)]
    (if (= rx ry)
      [uf false]
      [(assoc uf rx ry) true])))

;; ---------------------------------------------------------------------------
;; Kruskal's MST
;; ---------------------------------------------------------------------------

(defn kruskal
  "Run Kruskal's algorithm with goeBURST edge priority.
   Returns a sequence of edges {:i :j :d :level} that form the MST forest,
   where :level is the tiebreak level reached (1=SLV, 2=DLV, 3=TLV)."
  [n-sts matrix counts]
  (let [edges (->> (for [i (range n-sts)
                         j (range (inc i) n-sts)
                         :let [d (get-in matrix [i j])]
                         :when (and (int? d) (pos? d) (<= d 3))]
                     [i j d])
                   (sort-by #(edge-priority counts %)))]
    (loop [remaining edges
           uf        (make-uf n-sts)
           mst       []]
      (if (empty? remaining)
        mst
        (let [[i j d :as edge] (first remaining)
              [uf2 joined?]    (uf-union uf i j)]
          (if joined?
            (recur (rest remaining) uf2
                   (conj mst {:i i :j j :d d :level d}))
            (recur (rest remaining) uf mst)))))))

;; ---------------------------------------------------------------------------
;; Main entry point
;; ---------------------------------------------------------------------------

(defn run
  "Given parsed input {:ids [...] :matrix [[...]]} run goeBURST and return
   {:ids :nodes :edges} where nodes carry neighbor-count metadata."
  [{:keys [ids matrix]}]
  (let [n      (count ids)
        counts (neighbor-counts matrix)
        mst    (kruskal n matrix counts)
        nodes  (mapv (fn [i]
                       (merge {:id (ids i) :idx i}
                              (counts i)))
                     (range n))]
    {:ids ids :nodes nodes :edges mst}))
