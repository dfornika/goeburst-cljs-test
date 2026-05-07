(ns span.app
  (:require [uix.core :as uix :refer [defui $]]
            [uix.dom :as dom]
            [span.parse :as parse]
            [span.algorithm :as algo]
            [span.graph :as graph]
            [span.state :refer [state]]))

(def error-boundary
  (uix/create-error-boundary
   {:derive-error-state (fn [error] {:error error})
    :did-catch          (fn [error _info] (js/console.error error))}
   (fn [[{:keys [error]} _] {:keys [children]}]
     (if error
       ($ :div {:style {:color "#c0392b" :padding "1rem"}}
          "Error: " (.-message error))
       children))))


;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- run-algo! [parsed max-level]
  (try
    (let [result (if (= :single-linkage (:method @state))
                   (algo/run-single-linkage parsed max-level)
                   (algo/run parsed max-level))]
      (swap! state assoc :result result :error nil))
    (catch :default err
      (swap! state assoc :result nil :error (or (ex-message err) (str err))))))

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
                    (swap! state assoc :parsed nil :result nil
                           :error (or (ex-message err) (str err)))))))
        (.readAsText reader file)))))

;; ---------------------------------------------------------------------------
;; Sidebar components
;; ---------------------------------------------------------------------------

(def ^:private sidebar-open-width "280px")
(def ^:private sidebar-closed-width "40px")

(defui force-slider [{:keys [label value min-val max-val step on-change]}]
  ($ :div {:style {:margin "0.6rem 0"}}
     ($ :div {:style {:display "flex" :justify-content "space-between"
                      :font-size "0.82rem" :margin-bottom "2px"}}
        ($ :span label)
        ($ :span value))
     ($ :input {:type      "range"
                :min       min-val :max max-val :step step
                :value     value
                :style     {:width "100%"}
                :on-change on-change})))

(defui sidebar [{:keys [open? set-open!]}]
  (let [{:keys [result error max-level parsed show-edge-distances force-params method]} (uix/use-atom state)
        {:keys [link-distance repulsion gravity]} force-params]
    ($ :div {:style {:width           (if open? sidebar-open-width sidebar-closed-width)
                     :min-width       (if open? sidebar-open-width sidebar-closed-width)
                     :flex-shrink     0
                     :background      "#f8f8f8"
                     :border-right    "1px solid #e0e0e0"
                     :overflow        "hidden"
                     :transition      "width 0.2s ease, min-width 0.2s ease"
                     :display         "flex"
                     :flex-direction  "column"}}
       ($ :div {:style {:padding "0.4rem" :text-align "right" :flex-shrink 0}}
          ($ :button {:on-click #(set-open! not)
                      :title    (if open? "Collapse panel" "Expand panel")
                      :style    {:background "none" :border "none" :cursor "pointer"
                                 :font-size "1rem" :padding "4px 8px" :border-radius "4px"}}
             (if open? "◀" "▶")))
       ($ :div {:style {:width sidebar-open-width :padding "0 1rem 1rem"
                        :overflow-y "auto" :flex 1}}
          ($ :h1 {:style {:margin-top 0 :font-size "1.3rem"}} "Span")
          ($ :p {:style {:font-size "0.82rem" :color "#555" :margin-top 0}}
             "Upload a pairwise allelic-distance matrix (CSV or TSV, ST identifiers in first row and column).")
          ($ :div {:style {:margin "0.75rem 0"}}
             ($ :input {:type      "file"
                        :accept    ".csv,.tsv,.txt"
                        :style     {:font-size "0.82rem" :max-width "100%"}
                        :on-change on-file-change}))
          ($ :div {:style {:margin "0.75rem 0" :display "flex" :align-items "center" :gap "0.5rem"}}
             ($ :label {:style {:font-size "0.82rem"}} "Method:")
             ($ :select {:value     (name method)
                         :style     {:font-size "0.82rem"}
                         :on-change (fn [e]
                                      (let [m (keyword (.. e -target -value))]
                                        (swap! state assoc :method m)
                                        (when parsed (run-algo! parsed max-level))))}
                ($ :option {:value "single-linkage"} "Single-linkage")
                ($ :option {:value "goeburst"} "goeBURST")))
          ($ :div {:style {:margin "0.75rem 0" :display "flex" :align-items "center" :gap "0.5rem"}}
             ($ :label {:style {:font-size "0.82rem"}} "Max distance:")
             ($ :input {:type      "number"
                        :min       1 :max 10000
                        :value     max-level
                        :style     {:width "5rem" :font-size "0.82rem"}
                        :on-change (fn [e]
                                     (let [v (js/parseInt (.. e -target -value) 10)]
                                       (when (pos? v)
                                         (swap! state assoc :max-level v)
                                         (when parsed (run-algo! parsed v)))))}))
          ($ :label {:style {:display "flex" :align-items "center" :gap "0.5rem"
                             :font-size "0.82rem" :cursor "pointer" :margin "0.75rem 0"}}
             ($ :input {:type      "checkbox"
                        :checked   show-edge-distances
                        :on-change #(swap! state update :show-edge-distances not)})
             "Show edge distances")
          ($ :div {:style {:margin "0.75rem 0"}}
             ($ :p {:style {:font-size "0.82rem" :font-weight "bold" :margin "0 0 0.25rem"}}
                "Layout forces")
             ($ force-slider
                {:label     "Link distance"
                 :value     link-distance
                 :min-val   10 :max-val 300 :step 5
                 :on-change #(swap! state assoc-in [:force-params :link-distance]
                                    (js/parseInt (.. % -target -value) 10))})
             ($ force-slider
                {:label     "Repulsion"
                 :value     repulsion
                 :min-val   20 :max-val 800 :step 10
                 :on-change #(swap! state assoc-in [:force-params :repulsion]
                                    (js/parseInt (.. % -target -value) 10))})
             ($ force-slider
                {:label     "Gravity"
                 :value     gravity
                 :min-val   0.00 :max-val 0.30 :step 0.01
                 :on-change #(swap! state assoc-in [:force-params :gravity]
                                    (js/parseFloat (.. % -target -value)))}))
          (when error
            ($ :p {:style {:color "#c0392b" :font-size "0.82rem" :margin "0.5rem 0"}}
               "Error: " error))
          (when result
            ($ :div {:style {:margin-top "1rem"}}
               ($ graph/legend)))))))

;; ---------------------------------------------------------------------------
;; Root component
;; ---------------------------------------------------------------------------

(defui app [_]
  (let [[open? set-open!]                       (uix/use-state true)
        {:keys [result show-edge-distances
                force-params]}                  (uix/use-atom state)]
    ($ :div {:style {:display "flex" :height "100vh" :font-family "sans-serif"}}
       ($ sidebar {:open? open? :set-open! set-open!})
       ($ :div {:style {:flex 1 :overflow "hidden" :position "relative"}}
          (when result
            ($ graph/force-graph {:result         result
                                  :show-distances show-edge-distances
                                  :force-params   force-params}))))))

;; ---------------------------------------------------------------------------
;; Mount
;; ---------------------------------------------------------------------------

(defonce root
  (when (exists? js/document)
    (when-let [el (js/document.getElementById "app")]
      (dom/create-root el))))

(defn render
  "Renders the root [[app]] component into the DOM."
  []
  (when root
    (dom/render-root ($ error-boundary {} ($ app)) root)))

(defn ^:export init
  "Exported entry point called by shadow-cljs on page load."
  []
  (when ^boolean goog.DEBUG
    (js/console.debug "goog.DEBUG = true\nDebug mode enabled"))
  (render))

(defn ^:dev/after-load re-render
  "Hot-reload hook called by shadow-cljs after code changes.
  Re-renders from root so that new component definitions take effect.
  State is preserved because it lives in a `defonce` atom."
  []
  (render))

