(ns goeburst.graph
  (:require ["d3" :as d3]
            [reagent.core :as r]))

(def ^:private w 900)
(def ^:private h 600)

(defn- link-color [d]
  (case d 1 "#4477aa" 2 "#228833" 3 "#cc3311" "#999"))

(defn- setup-simulation! [svg-el {:keys [ids nodes edges]}]
  (-> (.select d3 svg-el) (.selectAll "*") .remove)
  (let [js-nodes (clj->js (mapv (fn [{:keys [id slv]}] {:id id :slv slv}) nodes))
        js-links (clj->js (mapv (fn [{:keys [i j d]}]
                                  {:source (nth ids i) :target (nth ids j) :d d})
                                edges))
        svg      (-> (.select d3 svg-el)
                     (.attr "viewBox" (str "0 0 " w " " h)))
        link-g   (.append svg "g")
        node-g   (.append svg "g")
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
                     (.attr "stroke" #(link-color (.-d %)))
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
                  #(str "translate(" (.-x %) "," (.-y %) ")"))))))

(defn force-graph [result]
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
               :style {:width "100%" :height (str h "px") :display "block"
                       :border "1px solid #e0e0e0" :border-radius "4px"}}])})))

(defn legend []
  [:div {:style {:display "flex" :gap "1.5rem" :margin "0.5rem 0"
                 :font-size "0.85rem" :font-family "sans-serif"}}
   (for [[label color] [["SLV" "#4477aa"] ["DLV" "#228833"] ["TLV" "#cc3311"]]]
     ^{:key label}
     [:span {:style {:display "flex" :align-items "center" :gap "0.3rem"}}
      [:span {:style {:display "inline-block" :width "24px" :height "3px"
                      :background color}}]
      label])])
