(ns docnudb.frontend.main
  (:require [replicant.dom :as r]))

(def store (atom {}))

(def dom-effects (atom {}))

(r/set-dispatch!
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
   (with-effects
     [:h3 {} "Hello world"]
     :name-hl #(set! (.-style.color %) "red")
     :reset #(set! (.-style.color %) "blue"))
   [:p "Started at " (:app/started-at state)]
   [:button {:on {:click :name-hl}}
    "update"]
   [:button {:on {:click :reset}}
    "reset"]])

(defonce el (js/document.getElementById "app"))

(defn ^:dev/after-load main []
  (add-watch store ::render
             (fn [_ _ _ state]
               (r/render el (render state))))

  ;; Trigger the initial render
  (swap! store assoc :app/started-at (js/Date.)))

(main)
