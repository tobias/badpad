(ns ^:figwheel-hooks compad.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom cursor]]))

(defonce app-state (atom {:active-creature 0
                          :round 1
                          :creatures [{:name "Honk"
                                       :init 10
                                       :init-mod 8}]}))


(defn get-app-element []
  (gdom/getElement "app"))

(defn with-sign [v]
  (if (>= v 0)
    (str "+" v)
    (str v)))

(defn parse-entry
  [e]
  (let [[_ name init-mod] (re-find #"([^|]+)\|?(.*)" e)]
    [name (if (empty? init-mod) 0 (js/parseInt init-mod))]))

(defn new-creature
  [name init-mod]
  {:name name
   :init 10
   :init-mod init-mod})

(defn add-creature []
  (let [val (atom nil)]
    (fn []
      [:div.add-creature
       [:input
        {:type "text"
         :placeholder "name|init-mod"
         :on-change #(reset! val (-> % .-target .-value))
         :on-key-down (fn [e]
                        (when
                            (and (= 13 (.-which e))
                                 (not (empty? @val)))
                          (swap! app-state update
                                 :creatures conj (apply new-creature (parse-entry @val)))
                          (set! (.-value (.-target e)) "")))}]])))

(defn counter []
  (let [count-state (atom 0)]
    (fn [parent-state {:keys [value visible? position close]}]
      (let [[x y] position
            close' (fn []
                     (reset! count-state 0)
                     (close))]
        [:div {:class ["counter" (when (not visible?) "hidden")]
               :style {:top (str y "px") :left (str x "px")}}
         [:button {:on-click close'} "x"]
         [:span.bold (str (+ value @count-state) " (" (with-sign @count-state) ")")]
         [:button {:on-click #(swap! count-state + 10)} (with-sign 10)]
         [:button {:on-click #(swap! count-state + 5)} (with-sign 5)]
         [:button {:on-click #(swap! count-state + 1)} (with-sign 1)]
         [:button {:on-click #(swap! count-state - 1)} (with-sign -1)]
         [:button {:on-click (fn []
                               (swap! parent-state + @count-state)
                               (close'))} "✓"]]))))

(defn creature []
  (let [creature-state (atom {})
        toggle-counter #(swap! creature-state update % not)]
    (fn [{:keys [name init init-mod] :as creature} idx active?]
      [:div {:class ["creature" (when active? "active")]}
       [:span.bold.name name]
       [:span.init-mod (with-sign init-mod)]
       [:span "Init: " init
        [:button {:on-click (fn [e]
                              (let [pos (.getBoundingClientRect (.-target e))]
                                (toggle-counter :init-visible?)
                                (swap! creature-state assoc :counter-position [(.-x pos) (- (.-y pos) 2)])))}
         "+"]
        [counter (cursor app-state [:creatures idx :init])
         {:value init
          :visible? (:init-visible? @creature-state)
          :position (:counter-position @creature-state)
          :close (partial toggle-counter :init-visible?)}]]])))

(defn next-creature []
  (let [{:keys [active-creature creatures round]} @app-state
        next (inc active-creature)
        updates (if (= next (count creatures))
                  {:round (inc round)
                   :active-creature 0}
                  {:active-creature next})]
    (swap! app-state merge updates)))

(defn encounter []
  (let [{:keys [active-creature creatures round]} @app-state]
    [:div
     [:div (str @app-state)]
     [add-creature]
     [:div.controls
      [:span.round "Round: " round]
      [:button.next {:on-click next-creature} ">>"]]
     [:div
      (map-indexed
        (fn [idx c]
          ^{:key idx} [creature c idx (= idx active-creature)])
        creatures)]]))

(defn mount [el]
  (reagent/render-component [encounter] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

;; conditionally start your application based on the presence of an "app" element
;; this is particularly helpful for testing this ns without launching the app
(mount-app-element)

;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  (mount-app-element)
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
