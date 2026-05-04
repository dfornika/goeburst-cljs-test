(ns goeburst.graph
  (:require ["d3" :as d3]
            [reagent.core :as r]))

(defn- link-color [d min-d max-d]
  (let [normalized (if (= min-d max-d) 0 (/ (- d min-d) (- max-d min-d)))
        t          (- 0.9 (* 0.7 normalized))]  ; 0.9 (dark) → 0.2 (light)
    (.interpolateBlues d3 t)))

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

(defn- setup-simulation! [svg-el {:keys [ids nodes edges]} show-distances
                          {:keys [link-distance repulsion gravity]} vis-state]
  (when-let [prev-sim (:sim @vis-state)]
    (.stop prev-sim))
  (let [prev-transform (.zoomTransform d3 svg-el)
        prev-js-nodes  (:js-nodes @vis-state)
        prev-positions (when prev-js-nodes
                         (reduce (fn [m n]
                                   (if (and (.-x n) (.-y n))
                                     (assoc m (.-id n) [(.-x n) (.-y n)])
                                     m))
                                 {}
                                 (array-seq prev-js-nodes)))]
    (-> (.select d3 svg-el) (.selectAll "*") .remove)
    (let [w        (max 400 (.-clientWidth svg-el))
          h        (max 300 (.-clientHeight svg-el))
          ds       (mapv :d edges)
          min-d    (if (seq ds) (apply min ds) 1)
          max-d    (if (seq ds) (apply max ds) 1)
          js-nodes (clj->js
                    (mapv (fn [{:keys [id slv]}]
                            (let [[px py] (get prev-positions id)]
                              (cond-> {:id id :slv slv}
                                (and px py) (assoc :x px :y py))))
                          nodes))
          js-links (clj->js (mapv (fn [{:keys [i j d]}]
                                    {:source (nth ids i) :target (nth ids j) :d d})
                                  edges))
          svg      (.select d3 svg-el)
          g        (-> (.append svg "g")
                       (.attr "transform" (str prev-transform)))
          link-g   (.append g "g")
          label-g  (.append g "g")
          node-g   (.append g "g")
          sim      (-> (.forceSimulation d3 js-nodes)
                       (cond-> prev-positions (.alpha 0.3))
                       (.force "link"
                               (-> (.forceLink d3 js-links)
                                   (.id #(.-id %))
                                   (.distance link-distance)))
                       (.force "charge" (.strength (.forceManyBody d3) (- repulsion)))
                       (.force "collide" (.radius (.forceCollide d3) 15))
                       (.force "x" (.strength (.forceX d3 (/ w 2)) gravity))
                       (.force "y" (.strength (.forceY d3 (/ h 2)) gravity)))
          drag     (-> (.drag d3)
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
                                    (set! (.-fy (.-subject ev)) nil))))
          links    (-> link-g
                       (.selectAll "line")
                       (.data js-links)
                       (.join "line")
                       (.attr "stroke" #(link-color (.-d %) min-d max-d))
                       (.attr "stroke-width" 1.5)
                       (.attr "stroke-opacity" 0.8))
          labels   (-> label-g
                       (.selectAll "text.edge-label")
                       (.data js-links)
                       (.join "text")
                       (.attr "class" "edge-label")
                       (.text #(str (.-d %)))
                       (.attr "text-anchor" "middle")
                       (.attr "dominant-baseline" "central")
                       (.attr "font-size" "10px")
                       (.attr "font-family" "sans-serif")
                       (.attr "fill" "#333")
                       (.attr "stroke" "white")
                       (.attr "stroke-width" 3)
                       (.attr "paint-order" "stroke")
                       (.attr "pointer-events" "none")
                       (.attr "visibility" (if show-distances "visible" "hidden")))
          node-gs  (-> node-g
                       (.selectAll "g")
                       (.data js-nodes)
                       (.join "g")
                       (.call drag)
                       (.attr "cursor" "grab"))]
      (swap! vis-state assoc :sim sim :js-nodes js-nodes)
      (-> node-gs
          (.append "circle")
          (.attr "r" 8)
          (.attr "fill" "#aaaaaa")
          (.attr "stroke" "#ffffff")
          (.attr "stroke-width" 1.5))
      (-> node-gs
          (.append "text")
          (.text #(.-id %))
          (.attr "x" 11)
          (.attr "y" 4)
          (.attr "font-size" "11px")
          (.attr "font-family" "sans-serif"))
      (.on sim "tick"
           (fn []
             (.attr links "x1" #(.. % -source -x))
             (.attr links "y1" #(.. % -source -y))
             (.attr links "x2" #(.. % -target -x))
             (.attr links "y2" #(.. % -target -y))
             (.attr labels "x" #(/ (+ (.. % -source -x) (.. % -target -x)) 2))
             (.attr labels "y" #(/ (+ (.. % -source -y) (.. % -target -y)) 2))
             (.attr node-gs "transform"
                    #(str "translate(" (.-x %) "," (.-y %) ")"))))
      (.call svg
             (-> (.zoom d3)
                 (.scaleExtent (clj->js [0.05 8]))
                 (.on "zoom" (fn [event]
                               (.attr g "transform" (.. event -transform toString)))))))))

(defn force-graph [_ _ _]
  (let [svg-ref   (atom nil)
        vis-state (atom {})]
    (r/create-class
     {:display-name "force-graph"
      :component-did-mount
      (fn [this]
        (let [[_ r show-distances force-params] (r/argv this)]
          (when @svg-ref
            (setup-simulation! @svg-ref r show-distances force-params vis-state))))
      :component-did-update
      (fn [this prev-argv]
        (let [[_ result show-distances force-params] (r/argv this)
              [_ prev-result prev-show prev-force-params] prev-argv]
          (when @svg-ref
            (if (not= result prev-result)
              (setup-simulation! @svg-ref result show-distances force-params vis-state)
              (do
                (when (not= show-distances prev-show)
                  (update-label-visibility! @svg-ref show-distances))
                (when (not= force-params prev-force-params)
                  (when-let [sim (:sim @vis-state)]
                    (update-forces! sim force-params))))))))
      :component-will-unmount
      (fn [_]
        (when-let [sim (:sim @vis-state)]
          (.stop sim)))
      :reagent-render
      (fn [_ _ _]
        [:svg {:ref   #(reset! svg-ref %)
               :style {:width "100%" :height "100%" :display "block"}}])})))

(defn legend []
  (let [dark  (.interpolateBlues d3 0.9)
        light (.interpolateBlues d3 0.2)]
    [:div {:style {:display "flex" :align-items "center" :gap "0.5rem"
                   :margin "0.5rem 0" :font-size "0.82rem" :font-family "sans-serif"}}
     [:span "Shorter"]
     [:div {:style {:flex 1 :height "6px" :border-radius "3px"
                    :background (str "linear-gradient(to right, " dark ", " light ")")}}]
     [:span "Longer"]]))
