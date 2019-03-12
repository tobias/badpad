(ns ^:figwheel-hooks compad.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom cursor]]))

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
   :hp 10
   :damage {:lethal 0 :non-lethal 0}
   :init-mod init-mod})

(defonce app-state (atom {:active-creature 0
                          :round 1
                          :creatures [(new-creature "Honk" 8)]}))

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

(defn counter-pane []
  (let [internal-state (atom 0)
        button (fn [amount]
                 [:button {:on-click #(swap! internal-state + amount)} (with-sign amount)])]
    (fn [parent-state {:keys [visible? position close]}]
      (let [[x y] position
            close' (fn []
                     (reset! internal-state 0)
                     (close))]
        [:div.counter-pane {:class [(when (not visible?) "hidden")]
                            :style {:top (str y "px") :left (str x "px")}}
         [:button {:on-click close'} "x"]
         [:span.bold (str (+ @parent-state @internal-state) " (" (with-sign @internal-state) ")")]
         (button 10)
         (button 5)
         (button 1)
         (button -1)
         [:button {:on-click (fn []
                               (swap! parent-state + @internal-state)
                               (close'))} "✓"]]))))

(defn counter-button
  []
  (let [internal-state (atom {})]
    (fn [count-state]
      [:div.flex
       [:button {:on-click (fn [e]
                             (let [pos (.getBoundingClientRect (.-target e))]
                               (swap! internal-state assoc
                                      count-state {:visible? true
                                                   :position [(.-x pos) (- (.-y pos) 2)]})))}
        "+"]
       [counter-pane count-state
        {:visible? (get-in @internal-state [count-state :visible?])
         :position (get-in @internal-state [count-state :position])
         :close #(swap! internal-state update-in [count-state :visible?] not)}]])))

(defn creature
  [{:keys [name damage hp init init-mod] :as creature} idx active?]
   [:div.creature.flex {:class [(when active? "active")]}
    [:div.name
     [:span.bold name]
     [:span.init-mod (with-sign init-mod)]]
    [:div.flex
     [:span.init "Init: " init]
     [counter-button (cursor app-state [:creatures idx :init])]]
    [:div.flex
     [:span.hp "HP: " hp]
     [counter-button (cursor app-state [:creatures idx :hp])]]
    [:div.flex
     [:span "Dmg - L: " (:lethal damage)]
     [counter-button (cursor app-state [:creatures idx :damage :lethal])]
     [:span "NL: " (:non-lethal damage)]
     [counter-button (cursor app-state [:creatures idx :damage :non-lethal])]]])

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
