(ns docnudb.frontend.main
  (:require [replicant.dom :as r]
            [nexus.core :as nexus]
            [nexus.registry :as nxr]))

(defonce state (atom {:mode :edit
                  :documents
                  [{:document/number "FJT-120-001"
                    :document/title "Test Document"
                    :focus true}
                   {:document/number "FJT-120-003"
                    :document/title "SAT Document"}]}))

(defn mouse-into [n]
  (swap! state assoc-in [:documents n :focus] true))

(defn mouse-out [n]
  (swap! state update-in [:documents n] #(dissoc % :focus)))

(defn start-edit [n]
  (swap! state assoc :mode :edit)
  (swap! state assoc-in [:documents n :editing] true))

(defn stop-edit [n]
  (swap! state assoc :mode :view)
  (swap! state update-in [:documents n] #(dissoc % :editing)))

(def nexus 
  {:nexus/effects
   {:document/mouse-into (fn [_ _ n] (mouse-into n))
    :document/mouse-out (fn [_ _ n] (mouse-out n))
    :document/start-edit (fn [_ _ n] (start-edit n))
    :document/stop-edit (fn [_ _ n] (stop-edit n))}})

(r/set-dispatch! #(nexus/dispatch nexus state %1 %2))

(update-in {:mode :view
            :documents
            [{:document/number "FJT-120-001"
              :document/title "Test Document"
              :editing true
              :focus 3}
             {:document/number "FJT-120-003"
              :document/title "SAT Document"}]}
           [:documents 0] #(dissoc % :focus))

(comment (mouse-into 0)
         (mouse-out 1)
         (start-edit 0)
         (stop-edit 1)
         )



(def dom-effects (atom {}))

#_(r/set-dispatch!
 (fn [event-data handler-data]
   (doseq [effect (@dom-effects handler-data)]
     (effect))))

(defn add-dom-effect! [event effect]
  (swap! dom-effects update event #(conj % effect)))

(defn with-effects
  [node & events-effects]
  (assoc-in node [1 :replicant/on-mount]
            (fn [data]
              (doseq [[event effect] (partition 2 events-effects)]
                (add-dom-effect! event (partial effect (data :replicant/node)))))))

(defn render [state]
  [:div
   [:table {:style {:border-collapse :collapse}}
    [:tr [:th "Number"] [:th "Title"]]
    (map #(vector :tr
                  {:on {:mouseenter (fn [_] (prn "enter"))}}
                  (cond (and (= (state :mode) :edit) (% :editing))
                        (list [:td [:input {:type :input :value (% :document/number)}]]
                              [:td [:input {:value (% :document/title)}]]
                              [:td [:button "delete"] [:button "apply"] [:button {:on {:click [[:document/stop-edit 1]]}} "cancel"]])
                        (= (state :mode) :edit)
                        (list [:td {:style {:color "grey"}} (% :document/number)]
                              [:td {:style {:color "grey"}} (% :document/title)])
                        (and (= (state :mode) :view) (% :focus))
                        (list [:td {:style {:background-color "lightyellow"}} (% :document/number)]
                              [:td {:style {:background-color "lightyellow"}} (% :document/title)]
                              [:td [:button {:on {:click [[:document/start-edit 1]]}}"edit"]])
                        (= (state :mode) :view)
                        (list [:td (% :document/number)]
                              [:td (% :document/title)]))
                  [:td])
         (state :documents))]])

(defonce el (js/document.getElementById "app"))

(defn ^:dev/after-load main []
  (add-watch state ::render
             (fn [_ _ _ state]
               (r/render el (render state))))

  ;; Trigger the initial render
  (swap! state assoc :app/started-at (js/Date.)))


(r/render el (render @state))
(add-watch state ::render
           (fn [_ _ _ state]
             (r/render el (render state))))
