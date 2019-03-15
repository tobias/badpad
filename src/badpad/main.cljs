(ns ^:figwheel-hooks badpad.main
  (:require
   [clojure.pprint :as pprint]
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom cursor]]))

(defn new-encounter-state
  [creatures]
  {:active-creature 0
   :round 0
   :creatures (vec creatures)})

(defonce current-encounter (atom (new-encounter-state [])))

(defonce encounters (atom []))

(defonce party (atom []))

(defonce undo-stack (atom '()))

(defonce in-undo (atom false))

(defn store-undo-state
  [_ _ old-state new-state]
  (when-not @in-undo
    (swap! undo-stack #(cons old-state %))))

(defn undo
  []
  (when-let [state (first @undo-stack)]
    (swap! undo-stack rest)
    (reset! in-undo true)
    (reset! current-encounter state)
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
  ([pc? name init-mod]
   (new-creature pc? name init-mod 10 10))
  ([pc? name init-mod init hp]
   {:name name
    :key (random-uuid)
    :pc? pc?
    :init init
    :hp hp
    :damage {:lethal 0 :non-lethal 0}
    :init-mod init-mod}))

(reset! encounters
  [{:name "A1"
    :creatures [(new-creature false "goblin" 4 15 3)]}])

(defn add-creature []
  (let [val (atom nil)]
    (fn [creatures pc?]
      [:div.add-creature
       [:input
        {:type "text"
         :placeholder "name|init-mod"
         :on-change #(reset! val (-> % .-target .-value))
         :on-key-down (fn [e]
                        (when
                            (and (= 13 (.-which e))
                              (not (empty? @val)))
                          (swap! creatures conj (apply new-creature pc? (parse-entry @val)))
                          (reset! val nil)
                          (set! (.-value (.-target e)) "")))}]])))

(defn counter-pane []
  (let [internal-state (atom 0)
        button (fn [amount]
                 [:button {:on-click #(swap! internal-state + amount)} (with-sign amount)])]
    (fn [parent-state {:keys [visible? position close]}]
      (let [[left top] position
            close' (fn []
                     (reset! internal-state 0)
                     (close))]
        [:div.counter-pane {:class [(when (not visible?) "hidden")]
                            :style {:top (str top "px") :left (str left "px")}}
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
       [:button
        {:on-click (fn [e]
                     (let [pos (.getBoundingClientRect (.-target e))]
                               (swap! internal-state assoc
                                 count-state {:visible? true
                                              :position [(.-left pos)
                                                         (.-top pos)]})))}
        "+"]
       [counter-pane count-state
        (assoc (select-keys (get @internal-state count-state)
                 #{:position :visible?})
          :close #(swap! internal-state update-in [count-state :visible?] not))]])))

(defn swap-creatures
  [a-idx b-idx]
  (let [creatures (:creatures @current-encounter)]
    (if (<= 0 b-idx (dec (count creatures)))
      (let [creature-a (nth creatures a-idx)
            creature-b (nth creatures b-idx)]
        (swap! current-encounter update :creatures assoc
          a-idx creature-b
          b-idx creature-a)))))

(defn remove-creature
  [list idx]
  (swap! list #(vec
                 (concat (take idx %)
                   (drop (inc idx) %)))))

(defn creature
  [{:keys [name damage hp init init-mod pc?] :as creature} parent idx active?]
  (let [{:keys [lethal non-lethal]} damage]
    [:div.creature.flex {:class [(when active? "active")]}
     [:div.name.left
      [:span.bold name]
      [:span.init-mod (with-sign init-mod)]]
     [:div.flex.left
      [:span.init "Init: " init]
      [counter-button (cursor parent [idx :init])]]
     (when-not pc?
       [:div.hp.flex.left {:class (when (>= (+ lethal non-lethal) hp) "ouch")}
        [:span "HP: " hp]
        [counter-button (cursor parent [idx :hp])]
        [:span "Dmg - L: " lethal]
        [counter-button (cursor parent [idx :damage :lethal])]
        [:span "NL: " non-lethal]
        [counter-button (cursor parent [idx :damage :non-lethal])]])
     [:div.clear]

     [:div.creature-controls
      [:button {:on-click #(swap-creatures idx (dec idx))} "↑"]
      [:button {:on-click #(swap-creatures idx (inc idx))} "↓"]
      [:button {:on-click #(remove-creature parent idx)} "x"]]]))

(defn pc
  [{:keys [name init-mod]} parent idx]
  [:div.pc
   [:span.bold name]
   [:span.init-mod (with-sign init-mod)]
   [:button {:on-click #(remove-creature parent idx)} "x"]])

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
  (let [{:keys [active-creature creatures round]} @current-encounter
        next-creature (inc active-creature)
        next-round {:round (inc round)
                    :active-creature 0}]
    (swap! current-encounter merge (cond
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
  (let [{:keys [active-creature creatures round]} @current-encounter]
    [:div#encounter
     [:div.controls
      [:div.left
       [:span.round "Round: " round]
       [:button.next {:on-click next-creature} ">>"]
       [:button.undo {:on-click undo} "undo"]]
      [:div.left
       [add-creature (cursor current-encounter [:creatures]) false]]
      [:div.clear]]
     [:div
      (map-indexed
        (fn [idx c]
          ^{:key (:key c)} [creature c (cursor current-encounter [:creatures])
                            idx (= idx active-creature)])
        creatures)]
     (debug-out "state" @current-encounter)
     (debug-out "undo stack" @undo-stack)]))

(defn sidebar []
  [:div#sidebar
   [:div.section
    [:span.header "Party"]
    [add-creature party :pc]
    (map-indexed
      (fn [idx {:keys [key] :as p}]
        ^{:key key} [pc p party idx])
      @party)
    ]
   [:div.section
    [:span.header "Encounters"]
    (map
      (fn [{:keys [name creatures]}]
        ^{:key name} [:div.encounter
                      [:button.encounter
                       {:on-click #(reset! current-encounter
                                     (new-encounter-state (concat @party creatures)))}
              name]])
      @encounters)]
   ])

(defn container []
  [:div.container
   [sidebar]
   [encounter]])

(defn mount [el]
  (reagent/render-component [container] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

;; conditionally start your application based on the presence of an "app" element
;; this is particularly helpful for testing this ns without launching the app
(mount-app-element)

;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  (mount-app-element)
  (add-watch current-encounter :undo-stack store-undo-state))
