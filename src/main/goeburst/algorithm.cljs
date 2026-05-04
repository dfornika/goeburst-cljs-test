(ns goeburst.algorithm)

;; ---------------------------------------------------------------------------
;; Union-Find (path-compressed)
;; ---------------------------------------------------------------------------

(defn- make-uf [n]
  {:parent (vec (range n))
   :rank   (vec (repeat n 0))})

(defn- uf-find [uf x]
  (let [parent (:parent uf)
        [root path] (loop [node x, path []]
                      (let [p (parent node)]
                        (if (= p node)
                          [node path]
                          (recur p (conj path node)))))
        parent' (reduce (fn [p node] (assoc p node root)) parent path)]
    [(assoc uf :parent parent') root]))

(defn- uf-union [uf x y]
  (let [[uf rx] (uf-find uf x)
        [uf ry] (uf-find uf y)]
    (if (= rx ry)
      [uf false]
      (let [rank   (:rank uf)
            rank-x (rank rx)
            rank-y (rank ry)]
        (cond
          (< rank-x rank-y)
          [(assoc-in uf [:parent rx] ry) true]

          (> rank-x rank-y)
          [(assoc-in uf [:parent ry] rx) true]

          :else
          [(-> uf
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

(defn- edge-priority
  "Return a sort key implementing the goeBURST total order.
   Sorting ascending; a lower key means a higher-quality edge (selected first).

   Per Francisco et al. 2009, at each level i (SLV → DLV → TLV → freq):
     1. max of both endpoint counts at level i  (higher wins → negate)
     2. min of both endpoint counts at level i  (higher wins → negate)
   Final tiebreak: lower ST index wins → (max i j) then (min i j)."
  [counts [i j d]]
  (let [ci (counts i)
        cj (counts j)]
    [d
     (- (max (:slv ci) (:slv cj)))
     (- (min (:slv ci) (:slv cj)))
     (- (max (:dlv ci) (:dlv cj)))
     (- (min (:dlv ci) (:dlv cj)))
     (- (max (:tlv ci) (:tlv cj)))
     (- (min (:tlv ci) (:tlv cj)))
     (- (max 1 1))   ; occurrence frequency – placeholder (always 1)
     (- (min 1 1))
     (max i j)
     (min i j)]))

;; ---------------------------------------------------------------------------
;; Kruskal's MST
;; ---------------------------------------------------------------------------

(defn kruskal
  "Run Kruskal's algorithm with goeBURST edge priority.
   max-level controls the maximum allelic distance for edge inclusion."
  ([n-sts matrix counts] (kruskal n-sts matrix counts 3))
  ([n-sts matrix counts max-level]
   (let [edges (->> (for [i (range n-sts)
                          j (range (inc i) n-sts)
                          :let [d (get-in matrix [i j])]
                          :when (and (int? d) (pos? d) (<= d max-level))]
                      [i j d])
                    (sort-by #(edge-priority counts %)))]
     (loop [remaining edges
            uf        (make-uf n-sts)
            mst       []]
       (if (empty? remaining)
         mst
         (let [[i j d] (first remaining)
               [uf2 joined?] (uf-union uf i j)]
           (if joined?
             (recur (rest remaining) uf2 (conj mst {:i i :j j :d d}))
             (recur (rest remaining) uf mst))))))))

;; ---------------------------------------------------------------------------
;; Main entry point
;; ---------------------------------------------------------------------------

(defn run
  "Given parsed input {:ids [...] :matrix [[...]]} run goeBURST and return
   {:ids :nodes :edges} where nodes carry CC-relative neighbor-count metadata.
   max-level controls the maximum allelic distance for edge inclusion (default 3)."
  ([parsed] (run parsed 3))
  ([{:keys [ids matrix]} max-level]
   (let [n      (count ids)
         counts (cc-relative-counts matrix)
         mst    (kruskal n matrix counts max-level)
         nodes  (mapv (fn [i]
                        (merge {:id (ids i) :idx i}
                               (counts i)))
                      (range n))]
     {:ids ids :nodes nodes :edges mst})))
