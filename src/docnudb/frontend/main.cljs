(ns docnudb.frontend.main
  (:require [replicant.dom :as r]))

(defonce store (atom {}))

(defn render [state]
  [:div
   [:h1 "Hello world"]
   [:p "Started at " (:app/started-at state)]
   [:button {:on {:click 
                  (fn [_] (swap! store assoc :app/started-at (js/Date.)))}}
    "update"]])

(defonce el (js/document.getElementById "app"))
 
(defn ^:dev/after-load main []
  (add-watch store ::render
   (fn [_ _ _ state]
     (r/render el (render state))))

  ;; Trigger the initial render
  (swap! store assoc :app/started-at (js/Date.)))

(main)
