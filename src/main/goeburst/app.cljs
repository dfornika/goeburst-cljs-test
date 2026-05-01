(ns goeburst.app
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [goeburst.parse :as parse]
            [goeburst.algorithm :as algo]))

;; ---------------------------------------------------------------------------
;; State
;; ---------------------------------------------------------------------------

(defonce state (r/atom {:parsed nil :result nil :error nil}))

;; ---------------------------------------------------------------------------
;; File upload
;; ---------------------------------------------------------------------------

(defn- on-file-change [e]
  (let [file (-> e .-target .-files (aget 0))]
    (when file
      (let [reader (js/FileReader.)]
        (set! (.-onload reader)
              (fn [ev]
                (try
                  (let [text   (-> ev .-target .-result)
                        parsed (parse/parse-distance-matrix text)
                        result (algo/run parsed)]
                    (swap! state assoc :parsed parsed :result result :error nil))
                  (catch js/Error err
                    (swap! state assoc :error (.-message err))))))
        (.readAsText reader file)))))

;; ---------------------------------------------------------------------------
;; MST table view (placeholder until D3 graph is wired up)
;; ---------------------------------------------------------------------------

(defn- edge-row [{:keys [i j d]} ids]
  [:tr {:key (str i "-" j)}
   [:td (ids i)]
   [:td (ids j)]
   [:td d]
   [:td (case d 1 "SLV" 2 "DLV" 3 "TLV" "—")]])

(defn- mst-table [result]
  (let [{:keys [ids edges]} result]
    [:div
     [:h2 "MST edges (" (count edges) ")"]
     [:table
      [:thead [:tr [:th "ST A"] [:th "ST B"] [:th "Distance"] [:th "Link type"]]]
      [:tbody
       (for [e edges]
         ^{:key (str (:i e) "-" (:j e))}
         [edge-row e ids])]]]))

(defn- node-table [result]
  (let [{:keys [nodes]} result]
    [:div
     [:h2 "Nodes"]
     [:table
      [:thead [:tr [:th "ST"] [:th "SLVs"] [:th "DLVs"] [:th "TLVs"]]]
      [:tbody
       (for [{:keys [id slv dlv tlv]} nodes]
         ^{:key id}
         [:tr [:td id] [:td slv] [:td dlv] [:td tlv]])]]]))

;; ---------------------------------------------------------------------------
;; Root component
;; ---------------------------------------------------------------------------

(defn app []
  (let [{:keys [result error]} @state]
    [:div {:style {:font-family "sans-serif" :max-width "900px" :margin "2rem auto"}}
     [:h1 "goeBURST"]
     [:p "Upload a pairwise allelic-distance matrix (CSV or TSV, with ST identifiers in the first row and column)."]
     [:input {:type "file" :accept ".csv,.tsv,.txt"
              :on-change on-file-change}]
     (when error
       [:p {:style {:color "red"}} "Error: " error])
     (when result
       [:div
        [node-table result]
        [mst-table result]])]))

;; ---------------------------------------------------------------------------
;; Mount
;; ---------------------------------------------------------------------------

(defn init []
  (rdom/render [app] (.getElementById js/document "app")))
