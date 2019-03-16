(ns ^:figwheel-hooks badpad.main
  (:require
   [clojure.pprint :as pprint]
   [goog.dom :as gdom]
   [historian.core :as hist]
   [reagent.core :as reagent :refer [atom cursor]]))

(defn new-encounter-state
  [creatures]
  {:active-creature 0
   :round 0
   :creatures (vec creatures)})

(defonce current-encounter (atom (new-encounter-state [])))

(hist/record! current-encounter :current-encounter)

(defonce encounters (atom []))

(defonce party (atom []))

(defn get-app-element []
  (gdom/getElement "app"))

(defn with-sign [v]
  (if (>= v 0)
    (str "+" v)
    (str v)))

(defn parse-creature
  [e]
  (let [[_ name init-mod] (re-find #"([^|]+)\|?(.*)" e)]
    [name (if (empty? init-mod) 0 (js/parseInt init-mod))]))

(defn new-creature
  ([pc? name init-mod]
   (new-creature pc? name init-mod 10 10 0))
  ([pc? name init-mod init hp mc]
   {:name name
    :key (random-uuid)
    :pc? pc?
    :init init
    :hp hp
    :morale-threshold mc
    :damage {:lethal 0 :non-lethal 0}
    :init-mod init-mod
    :conditions []}))

(reset! encounters
  [{:name "A1"
    :creatures [(new-creature false "goblin" 4 15 20 10)]}])

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
                          (swap! creatures conj (apply new-creature pc? (parse-creature @val)))
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

(defn plus-button
  [v]
  [:button
   {:on-click (fn [] (swap! v #(inc (if (= "-" %) 0 %))))}
   "+"])

(def conditions
  ["Conditions"
   "Bleed"
   "Blinded"
   "Confused"
   "Cowering"
   "Dazed"
   "Dazzled"
   "Deafened"
   "Disabled"
   "Dying"
   "Entangled"
   "Exhausted"
   "Fascinated"
   "Fatigued"
   "Flat-Footed"
   "Frightened"
   "Grappled"
   "Helpless"
   "Incorporeal"
   "Invisible"
   "Nauseated"
   "Panicked"
   "Paralyzed"
   "Petrified"
   "Pinned"
   "Prone"
   "Shaken"
   "Sickened"
   "Stable"
   "Staggered"
   "Stunned"
   "Unconscious"])

(defn remove-item
  [list idx]
  (swap! list #(vec
                 (concat (take idx %)
                   (drop (inc idx) %)))))

(defn conditions-pane
  [active-creature creature-conditions]
  (let [option (fn [name]
                 ^{:key name} [:option name])]
    [:div.conditions
     [:select
      {:on-change #(let [value (-> % .-target .-value)]
                     (when (not= (first conditions) value)
                       (swap! creature-conditions conj {:name value
                                                        :rounds "-"
                                                        :on-active-creature active-creature})
                       (set! (-> % .-target .-value) (first conditions))))}
      (map option conditions)]
     (map-indexed
       (fn [idx {:keys [name rounds]}]
         ^{:key name} [:div.condition
                       [:span (str name " " rounds)]
                       [plus-button (cursor creature-conditions [idx :rounds])]
                       [:button {:on-click #(remove-item creature-conditions idx)} "x"]])
       @creature-conditions)]))

(defn swap-creatures
  [creatures a-idx b-idx]
  (if (<= 0 b-idx (dec (count @creatures)))
    (let [creature-a (nth @creatures a-idx)
          creature-b (nth @creatures b-idx)]
      (swap! creatures assoc
        a-idx creature-b
        b-idx creature-a))))

(defn creature
  [creature creatures idx started? active-creature]
  (let [{:keys [name damage hp init init-mod morale-threshold pc?]} creature
        {:keys [lethal non-lethal]} damage
        active? (and (= idx active-creature)
                  started?)]
    [:div.creature {:class [(when active? "active")]}
     [:div.name.left
      [:span.bold name]
      [:span.init-mod (with-sign init-mod)]]
     [:div.flex.left
      [:span.init "Init:"
       [:span.bold init]]
      (when-not started?
        [counter-button (cursor creatures [idx :init])])]
     (when-not pc?
       [:div.hp.flex.left
        {:class (cond
                  (>= (+ lethal non-lethal) hp)       "ouch"
                  (>= lethal (- hp morale-threshold)) "flee")}
        [:span "HP:"
         [:span.bold hp]]
        (when-not started?
          [counter-button (cursor creatures [idx :hp])])
        [:span "MC:"
         [:span.bold morale-threshold]]
        (when-not started?
          [counter-button (cursor creatures [idx :morale-threshold])])
        (when started?
          (list
            [:span "L:"
             [:span.bold lethal]]
            [counter-button (cursor creatures [idx :damage :lethal])]
            [:span "NL:"
             [:span.bold non-lethal]]
            [counter-button (cursor creatures [idx :damage :non-lethal])]))])
     [:div.clear]
     [conditions-pane active-creature (cursor creatures [idx :conditions])]
     [:div.creature-controls
      [:button {:on-click #(swap-creatures creatures idx (dec idx))} "↑"]
      [:button {:on-click #(swap-creatures creatures idx (inc idx))} "↓"]
      [:button {:on-click #(remove-item creatures idx)} "x"]]]))

(defn pc
  [{:keys [name init-mod]} parent idx]
  [:div.pc
   [:span.bold name]
   [:span.init-mod (with-sign init-mod)]
   [:button {:on-click #(remove-item parent idx)} "x"]])

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

(defn count-down-conditions
  [active-creature-idx conditions]
  (reduce
    (fn [acc {:keys [rounds on-active-creature] :as condition}]
      (cond
        (or
          (not= active-creature-idx on-active-creature)
          (= "-" rounds))
        (conj acc condition)
        
        (= 1 rounds) acc
        
        :else (conj acc (update condition :rounds dec))))
    []
    conditions))

(defn next-creature*
  [next-creature-idx creatures]
  {:active-creature next-creature-idx
   :creatures (reduce
                (fn [acc creature]
                  (conj acc
                    (update creature :conditions
                      #(count-down-conditions next-creature-idx %))))
                []
                creatures)})

(defn next-creature
  []
  (let [{:keys [active-creature creatures round]} @current-encounter
        next-creature-idx (inc active-creature)
        next-round {:round (inc round)
                    :active-creature 0}
        next-state (cond
                     (= 0 round)
                     (assoc next-round
                       :creatures (sort-by-init creatures))

                     (= next-creature-idx (count creatures))
                     (merge
                       (next-creature* 0 creatures)
                       next-round)

                     :else (next-creature* next-creature-idx creatures))]
    (swap! current-encounter merge next-state)))

(defn debug-out
  [n v]
  [:pre
   (str n ":\n\n")
   (with-out-str (pprint/pprint v))])

(defn encounter []
  (let [{:keys [active-creature creatures round]} @current-encounter
        started? (< 0 round)]
    [:div#encounter
     [:div.controls
      [:div.left
       [:span.round "Round:"
        [:span.bold round]]
       [:button.next {:on-click next-creature} ">>"]
       [:button.undo {:on-click hist/undo!} "undo"]
       [:button.redo {:on-click hist/redo!} "redo"]]
      [:div.right
       [add-creature (cursor current-encounter [:creatures]) false]]
      [:div.clear]]
     [:div
      (map-indexed
        (fn [idx c]
          (let [creatures (cursor current-encounter [:creatures])]
            ^{:key (:key c)} [creature
                              c
                              creatures
                              idx
                              started?
                              active-creature]))
        creatures)]
     #_(debug-out "state" @current-encounter)]))

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
      @encounters)]])

(defn container []
  [:div.container
   [sidebar]
   [encounter]])

(defn mount [el]
  (reagent/render-component [container] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

(defn ^:after-load on-reload []
  (mount-app-element))
