(ns ^:figwheel-hooks compad.core
  (:require
   [clojure.pprint :as pprint]
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom cursor]]))

(defonce app-state (atom {:active-creature 0
                          :round 0
                          :creatures []}))

(defonce undo-stack (atom '()))

(def in-undo (atom false))

(defn store-undo-state
  [_ _ old-state new-state]
  (when-not @in-undo
    (swap! undo-stack #(cons old-state %))))

(defn undo
  []
  (when-let [state (first @undo-stack)]
    (swap! undo-stack rest)
    (reset! in-undo true)
    (reset! app-state state)
    (reset! in-undo false)))

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
                          (reset! val nil)
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

(defn swap-creatures
  [a-idx b-idx]
  (let [creatures (:creatures @app-state)]
    (if (<= 0 b-idx (dec (count creatures)))
      (let [creature-a (nth creatures a-idx)
            creature-b (nth creatures b-idx)]
        (swap! app-state update :creatures assoc
               a-idx creature-b
               b-idx creature-a)))))

(defn remove-creature
  [idx]
  (swap! app-state update :creatures #(vec
                                        (concat (take idx %)
                                                (drop (inc idx) %)))))

(defn creature
  [{:keys [name damage hp init init-mod] :as creature} idx active?]
  (let [{:keys [lethal non-lethal]} damage]
    [:div.creature.flex {:class [(when active? "active")]}
     [:div.name
      [:span.bold name]
      [:span.init-mod (with-sign init-mod)]]
     [:div.flex
      [:span.init "Init: " init]
      [counter-button (cursor app-state [:creatures idx :init])]]
     [:div.hp.flex {:class (when (>= (+ lethal non-lethal) hp) "ouch")}
      [:span "HP: " hp]
      [counter-button (cursor app-state [:creatures idx :hp])]
      [:span "Dmg - L: " lethal]
      [counter-button (cursor app-state [:creatures idx :damage :lethal])]
      [:span "NL: " non-lethal]
      [counter-button (cursor app-state [:creatures idx :damage :non-lethal])]]

     [:div
      [:button {:on-click #(swap-creatures idx (dec idx))} "↑"]
      [:button {:on-click #(swap-creatures idx (inc idx))} "↓"]]

     [:div
      [:button {:on-click #(remove-creature idx)} "x"]]]))

(defn init-comparator
  [a b]
  (let [init-a (:init a)
        init-b (:init b)]
    (if (= init-a init-b)
      (- (:init-mod b) (:init-mod a))
      (- init-b init-a))))

(defn sort-by-init
  [creatures]
  (vec (sort init-comparator creatures)))

(defn next-creature []
  (let [{:keys [active-creature creatures round]} @app-state
        next-creature (inc active-creature)
        next-round {:round (inc round)
                    :active-creature 0}]
    (swap! app-state merge (cond
                             (= 0 round)
                             (assoc next-round
                               :creatures (sort-by-init creatures))

                             (= next-creature (count creatures)) next-round

                             :else {:active-creature next-creature}))))

(defn debug-out
  [n v]
  [:pre
   (str n ":\n\n")
   (with-out-str (pprint/pprint v))])

(defn encounter []
  (let [{:keys [active-creature creatures round]} @app-state]
    [:div
     [add-creature]
     [:div.controls
      [:span.round "Round: " round]
      [:button.next {:on-click next-creature} ">>"]
      [:button.undo {:on-click undo} "undo"]]
     [:div
      (map-indexed
        (fn [idx c]
          ^{:key idx} [creature c idx (= idx active-creature)])
        creatures)]
     (debug-out "state" @app-state)
     (debug-out "undo stack" @undo-stack)]))

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
  (add-watch app-state :undo-stack store-undo-state)
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
