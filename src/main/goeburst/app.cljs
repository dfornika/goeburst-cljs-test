(ns goeburst.app
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [goeburst.parse :as parse]
            [goeburst.algorithm :as algo]
            [goeburst.graph :as graph]))

;; ---------------------------------------------------------------------------
;; State
;; ---------------------------------------------------------------------------

(defonce state (r/atom {:parsed nil :result nil :error nil :max-level 3}))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- run-algo! [parsed max-level]
  (try
    (swap! state assoc :result (algo/run parsed max-level) :error nil)
    (catch :default err
      (swap! state assoc :error (or (ex-message err) (str err))))))

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
                        parsed (parse/parse-distance-matrix text)]
                    (swap! state assoc :parsed parsed)
                    (run-algo! parsed (:max-level @state)))
                  (catch :default err
                    (swap! state assoc :error (or (ex-message err) (str err)))))))
        (.readAsText reader file)))))

;; ---------------------------------------------------------------------------
;; Sidebar
;; ---------------------------------------------------------------------------

(def ^:private sidebar-open-width "280px")
(def ^:private sidebar-closed-width "40px")

(defn- sidebar [open?]
  (let [{:keys [result error max-level parsed]} @state]
    [:div {:style {:width           (if @open? sidebar-open-width sidebar-closed-width)
                   :min-width       (if @open? sidebar-open-width sidebar-closed-width)
                   :flex-shrink     0
                   :background      "#f8f8f8"
                   :border-right    "1px solid #e0e0e0"
                   :overflow        "hidden"
                   :transition      "width 0.2s ease, min-width 0.2s ease"
                   :display         "flex"
                   :flex-direction  "column"}}
     [:div {:style {:padding "0.4rem" :text-align "right" :flex-shrink 0}}
      [:button {:on-click #(swap! open? not)
                :title    (if @open? "Collapse panel" "Expand panel")
                :style    {:background "none" :border "none" :cursor "pointer"
                           :font-size "1rem" :padding "4px 8px" :border-radius "4px"}}
       (if @open? "◀" "▶")]]
     [:div {:style {:width sidebar-open-width :padding "0 1rem 1rem"
                    :overflow-y "auto" :flex 1}}
      [:h1 {:style {:margin-top 0 :font-size "1.3rem"}} "goeBURST"]
      [:p {:style {:font-size "0.82rem" :color "#555" :margin-top 0}}
       "Upload a pairwise allelic-distance matrix (CSV or TSV, ST identifiers in first row and column)."]
      [:div {:style {:margin "0.75rem 0"}}
       [:input {:type      "file"
                :accept    ".csv,.tsv,.txt"
                :style     {:font-size "0.82rem" :max-width "100%"}
                :on-change on-file-change}]]
      [:div {:style {:margin "0.75rem 0" :display "flex" :align-items "center" :gap "0.5rem"}}
       [:label {:style {:font-size "0.82rem"}} "Max distance:"]
       [:input {:type      "number"
                :min       1 :max 10000
                :value     max-level
                :style     {:width "5rem" :font-size "0.82rem"}
                :on-change (fn [e]
                             (let [v (js/parseInt (.. e -target -value) 10)]
                               (when (pos? v)
                                 (swap! state assoc :max-level v)
                                 (when parsed (run-algo! parsed v)))))}]]
      (when error
        [:p {:style {:color "#c0392b" :font-size "0.82rem" :margin "0.5rem 0"}}
         "Error: " error])
      (when result
        [:div {:style {:margin-top "1rem"}}
         [graph/legend]])]]))

;; ---------------------------------------------------------------------------
;; Root component
;; ---------------------------------------------------------------------------

(defn app []
  (let [open? (r/atom true)]
    (fn []
      (let [{:keys [result]} @state]
        [:div {:style {:display "flex" :height "100vh" :font-family "sans-serif"}}
         [sidebar open?]
         [:div {:style {:flex 1 :overflow "hidden" :position "relative"}}
          (when result
            [graph/force-graph result])]]))))

;; ---------------------------------------------------------------------------
;; Mount
;; ---------------------------------------------------------------------------

(defn init []
  (rdom/render [app] (.getElementById js/document "app")))
