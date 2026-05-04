(ns goeburst.app
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [goeburst.parse :as parse]
            [goeburst.algorithm :as algo]
            [goeburst.graph :as graph]))

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
                  (catch :default err
                    (swap! state assoc :error (or (ex-message err)
                                                  (when (and err (exists? (.-message err)))
                                                    (.-message err))
                                                  (str err)))))))
        (.readAsText reader file)))))

;; ---------------------------------------------------------------------------
;; Root component
;; ---------------------------------------------------------------------------

(defn app []
  (let [{:keys [result error]} @state]
    [:div {:style {:font-family "sans-serif" :max-width "960px" :margin "2rem auto"
                   :padding "0 1rem"}}
     [:h1 "goeBURST"]
     [:p "Upload a pairwise allelic-distance matrix (CSV or TSV, with ST identifiers in the first row and column)."]
     [:input {:type "file" :accept ".csv,.tsv,.txt"
              :on-change on-file-change}]
     (when error
       [:p {:style {:color "red"}} "Error: " error])
     (when result
       [:div
        [graph/legend]
        [graph/force-graph result]])]))

;; ---------------------------------------------------------------------------
;; Mount
;; ---------------------------------------------------------------------------

(defn init []
  (rdom/render [app] (.getElementById js/document "app")))
