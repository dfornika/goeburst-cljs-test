(ns span.state)

(defonce state (atom {:parsed nil :result nil :error nil
                      :method :single-linkage
                      :max-level 3
                      :show-edge-distances false
                      :force-params {:link-distance 60
                                     :repulsion     200
                                     :gravity       0.05}}))