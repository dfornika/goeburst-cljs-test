(ns goeburst.graph
  (:require ["d3" :as d3]
            [reagent.core :as r]))

(defn- link-color [d max-d]
  (cond
    (= d 1) "#4477aa"
    (= d 2) "#228833"
    (= d 3) "#cc3311"
    :else (let [t (/ (- d 4) (max 1 (- max-d 4)))]
            (.interpolateRdYlBu d3 (- 1 (* 0.6 t))))))

(defn- setup-simulation! [svg-el {:keys [ids nodes edges]}]
  (-> (.select d3 svg-el) (.selectAll "*") .remove)
  (let [w        (max 400 (.-clientWidth svg-el))
        h        (max 300 (.-clientHeight svg-el))
        max-d    (reduce (fn [m {:keys [d]}] (max m d)) 1 edges)
        js-nodes (clj->js (mapv (fn [{:keys [id slv]}] {:id id :slv slv}) nodes))
        js-links (clj->js (mapv (fn [{:keys [i j d]}]
                                  {:source (nth ids i) :target (nth ids j) :d d})
                                edges))
        svg      (.select d3 svg-el)
        g        (.append svg "g")
        link-g   (.append g "g")
        node-g   (.append g "g")
        sim      (-> (.forceSimulation d3 js-nodes)
                     (.force "link"
                             (-> (.forceLink d3 js-links)
                                 (.id #(.-id %))
                                 (.distance 60)))
                     (.force "charge" (.strength (.forceManyBody d3) -200))
                     (.force "collide" (.radius (.forceCollide d3) 15))
                     (.force "x" (.strength (.forceX d3 (/ w 2)) 0.05))
                     (.force "y" (.strength (.forceY d3 (/ h 2)) 0.05)))
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
                     (.attr "stroke" #(link-color (.-d %) max-d))
                     (.attr "stroke-width" 1.5)
                     (.attr "stroke-opacity" 0.8))
        node-gs  (-> node-g
                     (.selectAll "g")
                     (.data js-nodes)
                     (.join "g")
                     (.call drag)
                     (.attr "cursor" "grab"))]
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
           (.attr node-gs "transform"
                  #(str "translate(" (.-x %) "," (.-y %) ")"))))
    (.call svg
           (-> (.zoom d3)
               (.scaleExtent (clj->js [0.05 8]))
               (.on "zoom" (fn [event]
                             (.attr g "transform" (.. event -transform toString))))))))

(defn force-graph [_]
  (let [svg-ref (atom nil)]
    (r/create-class
     {:display-name "force-graph"
      :component-did-mount
      (fn [this]
        (let [[_ r] (r/argv this)]
          (when @svg-ref (setup-simulation! @svg-ref r))))
      :component-did-update
      (fn [this _]
        (let [[_ r] (r/argv this)]
          (when @svg-ref (setup-simulation! @svg-ref r))))
      :reagent-render
      (fn [_]
        [:svg {:ref   #(reset! svg-ref %)
               :style {:width "100%" :height "100%" :display "block"}}])})))

(defn legend []
  [:div {:style {:display "flex" :gap "1.5rem" :margin "0.5rem 0"
                 :font-size "0.85rem" :font-family "sans-serif"}}
   (for [[label color] [["SLV" "#4477aa"] ["DLV" "#228833"] ["TLV" "#cc3311"]]]
     ^{:key label}
     [:span {:style {:display "flex" :align-items "center" :gap "0.3rem"}}
      [:span {:style {:display "inline-block" :width "24px" :height "3px"
                      :background color}}]
      label])])
