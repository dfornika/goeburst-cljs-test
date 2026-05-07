(ns span.algorithm)

;; ---------------------------------------------------------------------------
;; Union-Find (path-compressed, union-by-rank)
;; ---------------------------------------------------------------------------

(defn- make-uf 
  ""
  [n]
  {:parent (vec (range n))
   :rank   (vec (repeat n 0))})

(defn- uf-find 
  ""
  [uf x]
  (let [parent (:parent uf)
        [root path] (loop [node x, path []]
                      (let [p (parent node)]
                        (if (= p node)
                          [node path]
                          (recur p (conj path node)))))
        parent' (reduce (fn [p node] (assoc p node root)) parent path)]
    [(assoc uf :parent parent') root]))

(defn- uf-union 
  ""
  [uf x y]
  (let [[uf rx] (uf-find uf x)
        [uf ry] (uf-find uf y)]
    (if (= rx ry)
      [uf false]
      (let [rank   (:rank uf)
            rank-x (rank rx)
            rank-y (rank ry)]
        (cond
          (< rank-x rank-y) [(assoc-in uf [:parent rx] ry) true]
          (> rank-x rank-y) [(assoc-in uf [:parent ry] rx) true]
          :else             [(-> uf
                                 (assoc-in [:parent ry] rx)
                                 (update :rank assoc rx (inc rank-x)))
                             true])))))

;; ---------------------------------------------------------------------------
;; SLV connected components
;; ---------------------------------------------------------------------------

(defn slv-components
  "Returns a vector of component-root IDs, one per ST index.
   Two STs share a component iff they are connected via SLV links (λ=1)."
  [matrix]
  (let [n  (count matrix)
        uf (reduce (fn [uf [i j]]
                     (first (uf-union uf i j)))
                   (make-uf n)
                   (for [i (range n)
                         j (range (inc i) n)
                         :when (= 1 (get-in matrix [i j]))]
                     [i j]))]
    (loop [i 0, uf uf, roots []]
      (if (= i n)
        roots
        (let [[uf root] (uf-find uf i)]
          (recur (inc i) uf (conj roots root)))))))

;; ---------------------------------------------------------------------------
;; CC-relative neighbor counts  (μ from Francisco et al. 2009, eq. 2)
;; ---------------------------------------------------------------------------

(defn cc-relative-counts
  "For each ST at index i, count SLVs, DLVs, TLVs within i's SLV-connected
   component (clonal complex).  Returns a vector of {:slv n :dlv n :tlv n}."
  [matrix]
  (let [n    (count matrix)
        comp (slv-components matrix)]
    (mapv (fn [i]
            (reduce (fn [acc j]
                      (if (or (= i j) (not= (comp i) (comp j)))
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
;; Edge priority  (total order for Kruskal's)
;; ---------------------------------------------------------------------------

(defn- compare-st-ids
  "Compares ST identifier strings with length-first ordering (shorter < longer),
   falling back to lexicographic for equal lengths.  This matches the Java
   implementation and produces numeric ordering for typical integer ST IDs."
  [a b]
  (let [len-diff (- (count a) (count b))]
    (if (zero? len-diff) (compare a b) len-diff)))

(defn- edge-priority
  "Returns a sort key implementing the goeBURST total order
   (Francisco et al. 2009, §Algorithm, total order ≤ on E).
   Sorting ascending; a lower key means a higher-quality edge.

   At each tiebreak level i (SLV → DLV → TLV → occurrence frequency):
     1. max of both endpoint μ_i counts  (higher wins → negate for ascending sort)
     2. min of both endpoint μ_i counts  (higher wins → negate)
   Final tiebreak: lower ST ID wins, compared with length-first string ordering."
  [ids counts [i j d]]
  (let [ci     (counts i)
        cj     (counts j)
        id-i   (ids i)
        id-j   (ids j)
        [lo-id hi-id] (if (neg? (compare-st-ids id-i id-j))
                        [id-i id-j]
                        [id-j id-i])]
    [d
     (- (max (:slv ci) (:slv cj)))
     (- (min (:slv ci) (:slv cj)))
     (- (max (:dlv ci) (:dlv cj)))
     (- (min (:dlv ci) (:dlv cj)))
     (- (max (:tlv ci) (:tlv cj)))
     (- (min (:tlv ci) (:tlv cj)))
     (- (max 1 1))         ; occurrence frequency – placeholder (always 1)
     (- (min 1 1))
     (count lo-id) lo-id   ; ST ID tiebreak: length-first, then lexicographic
     (count hi-id) hi-id]))

(defn- edge-confidence-level
  "Returns the 1-indexed tiebreak level at which sort keys ka and kb first
   diverge after position 0 (the allelic distance).

   Returns:
     0    – the distance components differ (no count-level tiebreak was needed)
     1–2  – SLV count comparison was decisive (≈ blue in the paper)
     3–4  – DLV count comparison was decisive (≈ green)
     5–6  – TLV count comparison was decisive (≈ red)
     7–8  – occurrence frequency was decisive
     9+   – ST ID tiebreak was reached
     nil  – keys are identical (or ka is the last edge with no successor)"
  [ka kb]
  (if (not= (first ka) (first kb))
    0
    (loop [level 1, a (next ka), b (next kb)]
      (cond
        (or (nil? a) (nil? b)) nil
        (not= (first a) (first b)) level
        :else (recur (inc level) (next a) (next b))))))

;; ---------------------------------------------------------------------------
;; Kruskal's MST  (Francisco et al. 2009, §goeBURST algorithm)
;; ---------------------------------------------------------------------------

(defn kruskal
  "Run Kruskal's algorithm with the goeBURST edge priority order.
   Returns a vector of edges {:i :j :d :level} forming the MST forest.

   :level encodes link confidence: at which tiebreak step the chosen edge
   was separated from its closest competitor in the sorted edge list.
   nil → unique best (no competitor)
   0   → separated by allelic distance alone (no count-level tiebreak was needed)
   1–2 → SLV; 3–4 → DLV; 5–6 → TLV; 7+ → other."
  ([n-sts ids matrix counts] (kruskal n-sts ids matrix counts 3))
  ([n-sts ids matrix counts max-level]
   (let [priority   (fn [e] (edge-priority ids counts e))
         raw-edges  (for [i (range n-sts)
                          j (range (inc i) n-sts)
                          :let [d (get-in matrix [i j])]
                          :when (and (int? d) (pos? d) (<= d max-level))]
                      [i j d])
         ;; Pre-compute sort keys to avoid redundant calls during sort and level lookup.
         keyed      (mapv (fn [e] [e (priority e)]) raw-edges)
         sorted     (vec (sort-by second keyed))
         n          (count sorted)
         level-at   (fn [k]
                      (when (< k (dec n))
                        (edge-confidence-level (second (nth sorted k))
                                               (second (nth sorted (inc k))))))]
     (loop [k   0
            rem sorted
            uf  (make-uf n-sts)
            mst []]
       (if (empty? rem)
         mst
         (let [[[i j d] _]  (first rem)
               [uf2 joined?] (uf-union uf i j)]
           (if joined?
             (recur (inc k) (rest rem) uf2 (conj mst {:i i :j j :d d :level (level-at k)}))
             (recur (inc k) (rest rem) uf mst))))))))

;; ---------------------------------------------------------------------------
;; Single-linkage MST  (standard Kruskal's, distance only)
;; ---------------------------------------------------------------------------

(defn- single-linkage-priority
  "Sort key for single-linkage MST: distance alone, with ST ID as the only tiebreak.
   Equivalent to Kruskal's standard MST with no neighborhood-count comparisons."
  [ids [i j d]]
  (let [id-i (ids i)
        id-j (ids j)
        [lo-id hi-id] (if (neg? (compare-st-ids id-i id-j))
                        [id-i id-j]
                        [id-j id-i])]
    [d (count lo-id) lo-id (count hi-id) hi-id]))

(defn run-single-linkage
  "Build a minimum spanning forest using single-linkage (distance-only) priority.
   Equivalent to running Kruskal's MST with no goeBURST neighbourhood-count tiebreaks.
   Returns {:ids :nodes :edges} in the same format as run; edges carry :i :j :d
   but no :level (the link-confidence concept is specific to goeBURST).
   max-level controls the maximum allelic distance for edge inclusion (default 3)."
  ([parsed] (run-single-linkage parsed 3))
  ([{:keys [ids matrix]} max-level]
   (let [n         (count ids)
         priority  (fn [e] (single-linkage-priority ids e))
         raw-edges (for [i (range n)
                         j (range (inc i) n)
                         :let [d (get-in matrix [i j])]
                         :when (and (int? d) (pos? d) (<= d max-level))]
                     [i j d])
         sorted    (sort-by priority raw-edges)
         mst       (loop [rem sorted
                          uf  (make-uf n)
                          acc []]
                     (if (empty? rem)
                       acc
                       (let [[i j d]      (first rem)
                             [uf2 joined?] (uf-union uf i j)]
                         (if joined?
                           (recur (rest rem) uf2 (conj acc {:i i :j j :d d}))
                           (recur (rest rem) uf acc)))))
         nodes     (mapv (fn [i] {:id (ids i) :idx i}) (range n))]
     {:ids ids :nodes nodes :edges mst})))

;; ---------------------------------------------------------------------------
;; Main entry point
;; ---------------------------------------------------------------------------

(defn run
  "Given parsed input {:ids [...] :matrix [[...]]} run goeBURST and return
   {:ids :nodes :edges} where nodes carry CC-relative neighbor-count metadata
   and edges carry a :level confidence field.
   max-level controls the maximum allelic distance for edge inclusion (default 3)."
  ([parsed] (run parsed 3))
  ([{:keys [ids matrix]} max-level]
   (let [n      (count ids)
         counts (cc-relative-counts matrix)
         mst    (kruskal n ids matrix counts max-level)
         nodes  (mapv (fn [i]
                        (merge {:id (ids i) :idx i}
                               (counts i)))
                      (range n))]
     {:ids ids :nodes nodes :edges mst})))
