(ns docnudb.frontend.main
  (:require [replicant.dom :as r]
            [nexus.core :as nexus]
            [nexus.registry :as nxr]))

(def state (atom {:mode :document-overview #_:document-selected
                  :documents
                  [{:number "FJT-120-001" :title "Test Document"}
                   {:number "FJT-120-003" :title "SAT Document"}
                   {:number "FJT-120-013" :title "SIT Document"}
                   {:number "FJT-120-023" :title "UT Document"}]
                  :document-edit
                  {:number nil :title nil}
                  :issues-of-selected
                  [{:issue "4"} {:issue "A" :focus true} {:issue "B Draft 1"}]}))

(defn focus-document [n state]
  (assoc-in state [:documents n :focus] true))

(defn unfocus-document [n state]
  (update-in state [:documents n] #(dissoc % :focus)))

(defn start-document-edit [n state]
  (-> state
      (assoc :mode :edit)
      (assoc-in [:documents n :editing] true)
      (assoc-in [:document-edit :number] (get-in state [:documents n :number]))
      (assoc-in [:document-edit :title] (get-in state [:documents n :title]))))

(defn stop-document-edit [n state]
  (-> state
      (assoc :mode :document-overview)
      (update-in [:documents n] #(dissoc % :editing))))

(defn change-document-number [s state]
  (assoc-in state [:document-edit :number] s))

(defn change-document-title [s state]
  (assoc-in state [:document-edit :title] s))

(defn accept-document-edit [n state]
  (-> state
      (assoc-in [:documents n :number] (get-in state [:document-edit :number]))
      (assoc-in [:documents n :title] (get-in state [:document-edit :title]))))

(defn remove-document [n state]
  (assoc state :documents
         (into (subvec (state :documents) 0 n) (subvec (state :documents) (inc n)))))

(defn new-document [state]
  (assoc state :documents
         (conj (state :documents) {:number "<new number>"
                                   :title "<new title>"})))

(defn select-document [n state]
  (-> state
      (assoc :mode :document-selected)
      (assoc-in [:documents n :selected] true)))

(defn unselect-document [n state]
  (-> state
      (assoc :mode :document-overview)
      (update-in [:documents n] #(dissoc % :selected))))

(defn focus-issue [n state]
  (assoc-in state [:issues-of-selected n :focus] true))

(defn unfocus-issue [n state]
  (update-in state [:issues-of-selected n] #(dissoc % :focus)))

((fn [a b] (+ a b)) 1 4)

(def nexus
  {:nexus/effects
   {:focus-document (fn [_ _ n] (swap! state (partial focus-document n)))
    :unfocus-document (fn [_ _ n] (swap! state (partial unfocus-document n)))
    :start-document-edit (fn [_ _ n] (swap! state (partial start-document-edit n)))
    :stop-document-edit (fn [_ _ n] (swap! state (partial stop-document-edit n)))
    :accept-document-edit (fn [_ _ n] (swap! state (partial accept-document-edit n)))
    :change-document-number (fn [_ _ s] (swap! state (partial change-document-number s)))
    :change-document-title (fn [_ _ s] (swap! state (partial change-document-title s)))
    :remove-document (fn [_ _ s] (swap! state (partial remove-document s)))
    :add-new-document (fn [_ _] (swap! state new-document))
    :select-document (fn [_ _ n] (swap! state (partial select-document n)))
    :unselect-document (fn [_ _ n] (swap! state (partial unselect-document n)))}
   :nexus/placeholders
   {:number-input
    (fn [dispatch-data]
      (some-> dispatch-data :replicant/dom-event .-target .-value))}})

(r/set-dispatch! #(nexus/dispatch nexus state %1 %2))

(defn render-issues [state]
  (map-indexed
   (fn [i {issue :issue focus :focus}]
     [:tr
      [:td {:align :right} (if (= i 0) (list "Issues:"))]
      [:td (if focus {:style {:background-color "lightyellow"}}) issue]])
   (state :issues-of-selected)))

(defn render [state]
  [:div
   [:table {:style {:border-collapse :collapse}}
    [:tr [:th {:align :left} "Number"] [:th {:align :left} "Title"]]
    (map-indexed
     (fn [index document]
       (cond (and (= (state :mode) :edit) (document :editing))
             [:tr
              [:td [:input {:value (document :number)
                            :on {:input [[:change-document-number [:number-input]]]}}]]
              [:td
               [:input {:value (document :title)
                        :on {:input [[:change-document-title [:number-input]]]}}]]
              [:td
               [:button {:on {:click [[:stop-document-edit index]
                                      [:remove-document index]]}} "delete"]
               [:button {:on {:click [[:accept-document-edit index]
                                      [:stop-document-edit index]]}} "apply"]
               [:button {:on {:click [[:stop-document-edit index]]}} "cancel"]]]
             (and (= (state :mode) :document-overview) (document :focus))
             [:tr {:on {:mouseleave [[:unfocus-document index]]}}
              [:td {:style {:background-color "lightyellow"}
                    :on {:click [[:select-document index]]}} (document :number)]
              [:td {:style {:background-color "lightyellow"}
                    :on {:click [[:select-document index]]}} (document :title)]
              [:td [:button {:on {:click [[:unfocus-document index]
                                          [:start-document-edit index]]}} "edit"]]]
             (and (= (state :mode) :document-selected) (document :selected))
             (list [:tr
                    [:td  (document :number)]
                    [:td (document :title)]
                    [:td [:button {:on {:click [[:unselect-document index]]}} "x"]]] (render-issues state))
             (or (= (state :mode) :edit)
                 (= (state :mode) :document-selected))
             [:tr
              [:td {:style {:color "grey"}} (document :number)]
              [:td {:style {:color "grey"}} (document :title)]]
             (= (state :mode) :document-overview)
             [:tr {:on {:mouseenter [[:focus-document index]]}}
              [:td (document :number)]
              [:td (document :title)]]))
     (state :documents))
    (if (= (state :mode) :document-overview)
      [:button {:on {:click [[:add-new-document]
                             [:start-document-edit (count (state :documents))]]}} "new"])]])

(defonce el (js/document.getElementById "app"))

(defn ^:dev/after-load main []
  (add-watch state ::render
             (fn [_ _ _ state]
               (r/render el (render state)))))


(r/render el (render @state))
(add-watch state ::render
           (fn [_ _ _ state]
             (r/render el (render state))))
