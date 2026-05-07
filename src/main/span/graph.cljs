(ns span.graph
  (:require ["d3" :as d3]
            [uix.core :as uix :refer [defui $]]))

;; ---------------------------------------------------------------------------
;; Visual attribute maps
;; Applied via set-attrs so visual constants live as data, not buried in chains
;; ---------------------------------------------------------------------------

(def ^:private node-circle-attrs
  {:r 8
   :fill "#aaaaaa"
   :stroke "#ffffff"
   :stroke-width 1.5})

(def ^:private node-label-attrs
  {:x 11
   :y 4
   :font-size "11px"
   :font-family "sans-serif"})

;; stroke (dynamic, per-edge) and visibility (toggled separately) are NOT here
(def ^:private edge-line-attrs
  {:stroke-width 1.5
   :stroke-opacity 0.8})

;; class, text content, and visibility are set separately
(def ^:private edge-label-base-attrs
  {:text-anchor       "middle"
   :dominant-baseline "central"
   :font-size         "10px"
   :font-family       "sans-serif"
   :fill              "#333"
   :stroke            "white"
   :stroke-width      3
   :paint-order       "stroke"
   :pointer-events    "none"})

;; ---------------------------------------------------------------------------
;; Utilities
;; ---------------------------------------------------------------------------

(defn- set-attrs
  "Apply a map of SVG attributes to a D3 selection.
   Keyword keys are converted to strings via `name` (e.g. :stroke-width → \"stroke-width\")."
  [selection attr-map]
  (reduce (fn [s [k v]] (.attr s (name k) v)) selection attr-map))

(defn- link-color [d min-d max-d]
  (let [normalized (if (= min-d max-d) 0 (/ (- d min-d) (- max-d min-d)))
        t          (- 0.9 (* 0.7 normalized))]  ; 0.9 (dark) → 0.2 (light)
    (.interpolateBlues d3 t)))

;; ---------------------------------------------------------------------------
;; Data-transformation helpers  (public → independently testable)
;; ---------------------------------------------------------------------------

(defn edge-distance-range
  "Returns [min-d max-d] of the allelic distances in `edges`, or [1 1] when empty."
  [edges]
  (if (seq edges)
    [(apply min (map :d edges))
     (apply max (map :d edges))]
    [1 1]))

(defn build-nodes
  "Converts ClojureScript node maps to a mutable JS array for D3.
   Restores x/y from `prev-positions` (a map of id→[x y]) when available,
   so the force simulation resumes near the previous layout."
  [nodes prev-positions]
  (clj->js
   (mapv (fn [{:keys [id slv]}]
           (let [[px py] (get prev-positions id)]
             (cond-> {:id id :slv slv}
               (and px py) (assoc :x px :y py))))
         nodes)))

(defn build-links
  "Converts ClojureScript edge maps to a mutable JS array for D3 forceLink.
   Uses ST identifiers (not indices) as source/target so forceLink can resolve them."
  [ids edges]
  (clj->js
   (mapv (fn [{:keys [i j d]}]
           {:source (nth ids i) :target (nth ids j) :d d})
         edges)))

;; ---------------------------------------------------------------------------
;; D3 behavior factories  (private — produce opaque D3 objects)
;; ---------------------------------------------------------------------------

(defn- saved-positions
  "Extracts a {id → [x y]} map from a D3-mutated JS node array.
   D3 writes x/y directly onto the node objects during simulation."
  [js-nodes]
  (when js-nodes
    (reduce (fn [m n]
              (if (and (.-x n) (.-y n))
                (assoc m (.-id n) [(.-x n) (.-y n)])
                m))
            {}
            (array-seq js-nodes))))

(defn- make-simulation
  "Creates a D3 forceSimulation with goeBURST-appropriate forces.
   Starts at alpha 0.3 (gentle re-settle) when previous positions exist,
   or alpha 1.0 (full layout) for a fresh dataset."
  [js-nodes js-links w h prev-positions {:keys [link-distance repulsion gravity]}]
  (-> (.forceSimulation d3 js-nodes)
      (cond-> (seq prev-positions) (.alpha 0.3))
      (.force "link"
              (-> (.forceLink d3 js-links)
                  (.id #(.-id %))
                  (.distance link-distance)))
      (.force "charge" (.strength (.forceManyBody d3) (- repulsion)))
      (.force "collide" (.radius (.forceCollide d3) 15))
      (.force "x" (.strength (.forceX d3 (/ w 2)) gravity))
      (.force "y" (.strength (.forceY d3 (/ h 2)) gravity))))

(defn- make-drag
  "Creates a D3 drag behavior that pins a node while dragging
   and releases it (fx/fy → nil) on drop so the simulation can reclaim it."
  [sim]
  (-> (.drag d3)
      (.on "start" (fn [ev]
                     (when (zero? (.-active ev))
                       (-> sim (.alphaTarget 0.3) .restart))
                     (set! (.-fx (.-subject ev)) (.-x (.-subject ev)))
                     (set! (.-fy (.-subject ev)) (.-y (.-subject ev)))))
      (.on "drag" (fn [ev]
                    (set! (.-fx (.-subject ev)) (.-x ev))
                    (set! (.-fy (.-subject ev)) (.-y ev))))
      (.on "end" (fn [ev]
                   (when (zero? (.-active ev))
                     (.alphaTarget sim 0))
                   (set! (.-fx (.-subject ev)) nil)
                   (set! (.-fy (.-subject ev)) nil)))))

;; ---------------------------------------------------------------------------
;; DOM update helpers
;; ---------------------------------------------------------------------------

(defn- update-label-visibility! [svg-el show?]
  (-> (.select d3 svg-el)
      (.selectAll "text.edge-label")
      (.attr "visibility" (if show? "visible" "hidden"))))

(defn- update-forces! [sim {:keys [link-distance repulsion gravity]}]
  (.distance (.force sim "link") link-distance)
  (.strength (.force sim "charge") (- repulsion))
  (.strength (.force sim "x") gravity)
  (.strength (.force sim "y") gravity)
  (-> sim (.alpha 0.3) .restart))

;; ---------------------------------------------------------------------------
;; Main simulation setup
;; ---------------------------------------------------------------------------

(defn- setup-simulation! [svg-el {:keys [ids nodes edges]} show-distances force-params vis-state]
  (when-let [prev-sim (:sim @vis-state)]
    (.stop prev-sim))
  (let [prev-transform (.zoomTransform d3 svg-el)
        prev-positions (saved-positions (:js-nodes @vis-state))]
    (-> (.select d3 svg-el) (.selectAll "*") .remove)
    (let [w            (max 400 (.-clientWidth svg-el))
          h            (max 300 (.-clientHeight svg-el))
          [min-d max-d] (edge-distance-range edges)
          js-nodes     (build-nodes nodes prev-positions)
          js-links     (build-links ids edges)
          svg          (.select d3 svg-el)
          g            (-> (.append svg "g") (.attr "transform" (str prev-transform)))
          link-g       (.append g "g")
          label-g      (.append g "g")
          node-g       (.append g "g")
          sim          (make-simulation js-nodes js-links w h prev-positions force-params)
          drag         (make-drag sim)
          links        (-> link-g
                           (.selectAll "line")
                           (.data js-links)
                           (.join "line")
                           (set-attrs edge-line-attrs)
                           (.attr "stroke" #(link-color (.-d %) min-d max-d)))
          labels       (-> label-g
                           (.selectAll "text.edge-label")
                           (.data js-links)
                           (.join "text")
                           (.attr "class" "edge-label")
                           (.text #(str (.-d %)))
                           (set-attrs edge-label-base-attrs)
                           (.attr "visibility" (if show-distances "visible" "hidden")))
          node-gs      (-> node-g
                           (.selectAll "g")
                           (.data js-nodes)
                           (.join "g")
                           (.call drag)
                           (.attr "cursor" "grab"))]
      (swap! vis-state assoc :sim sim :js-nodes js-nodes)
      (-> node-gs (.append "circle") (set-attrs node-circle-attrs))
      (-> node-gs (.append "text") (.text #(.-id %)) (set-attrs node-label-attrs))
      (.on sim "tick"
           (fn []
             (.attr links "x1" #(.. % -source -x))
             (.attr links "y1" #(.. % -source -y))
             (.attr links "x2" #(.. % -target -x))
             (.attr links "y2" #(.. % -target -y))
             (.attr labels "x" #(/ (+ (.. % -source -x) (.. % -target -x)) 2))
             (.attr labels "y" #(/ (+ (.. % -source -y) (.. % -target -y)) 2))
             (.attr node-gs "transform" #(str "translate(" (.-x %) "," (.-y %) ")"))))
      (.call svg
             (-> (.zoom d3)
                 (.scaleExtent (clj->js [0.05 8]))
                 (.on "zoom" (fn [event]
                               (.attr g "transform" (.. event -transform toString)))))))))

;; ---------------------------------------------------------------------------
;; UIx component
;; ---------------------------------------------------------------------------

(defui force-graph [{:keys [result show-distances force-params]}]
  (let [svg-ref   (uix/use-ref nil)
        ;; vis-state holds {:sim ... :js-nodes ...}; UIx refs are atom-like so
        ;; setup-simulation! can @vis-state and swap! vis-state unchanged.
        vis-state (uix/use-ref {})
        ;; prev-ref tracks the last set of props seen by the effect so we can
        ;; distinguish an initial mount (::init) from the three update paths.
        prev-ref  (uix/use-ref ::init)]

    ;; Runs after every render where result, show-distances, or force-params changed.
    ;; Routes to setup-simulation! (full rebuild), update-label-visibility!, or
    ;; update-forces! based on which prop actually changed.
    (uix/use-effect
     (fn []
       (when-let [el @svg-ref]
         (let [prev @prev-ref]
           (cond
             (= ::init prev)
             (setup-simulation! el result show-distances force-params vis-state)

             (not= result (:result prev))
             (setup-simulation! el result show-distances force-params vis-state)

             (not= show-distances (:show-distances prev))
             (update-label-visibility! el show-distances)

             (not= force-params (:force-params prev))
             (when-let [sim (:sim @vis-state)]
               (update-forces! sim force-params))))
         (reset! prev-ref {:result         result
                           :show-distances show-distances
                           :force-params   force-params}))
       nil)
     [result show-distances force-params])

    ;; Cleanup: stop the simulation when this component unmounts.
    (uix/use-effect
     (fn []
       (fn []
         (when-let [sim (:sim @vis-state)]
           (.stop sim))))
     [])

    ($ :svg {:ref   svg-ref
             :style {:width "100%" :height "100%" :display "block"}})))

;; ---------------------------------------------------------------------------
;; Legend
;; ---------------------------------------------------------------------------

(defui legend [_]
  (let [dark  (.interpolateBlues d3 0.9)
        light (.interpolateBlues d3 0.2)]
    ($ :div {:style {:display "flex" :align-items "center" :gap "0.5rem"
                     :margin "0.5rem 0" :font-size "0.82rem" :font-family "sans-serif"}}
       ($ :span "Shorter")
       ($ :div {:style {:flex 1 :height "6px" :border-radius "3px"
                        :background (str "linear-gradient(to right, " dark ", " light ")")}})
       ($ :span "Longer"))))
