(ns ^:figwheel-hooks badpad.main
  (:require
   [clojure.pprint :as pprint]
   [cljs.reader :as reader]
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

(defn ensure-value
  [m v keys]
  (reduce
    (fn [acc k]
      (if (k acc)
        acc
        (assoc acc k v)))
    m
    keys))

(defn new-creature
  ([pc? name init-mod]
   (new-creature {:pc?      pc?
                  :name     name
                  :init-mod init-mod
                  :init     10
                  :hp       10}))
  ([creature]
   (-> creature
     (assoc
       :key        (random-uuid)
       :damage     {:lethal 0 :non-lethal 0}
       :conditions [])
     (ensure-value 0 #{:init :init-mod :hp :morale-threshold}))))

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
    (fn [amounts parent-state {:keys [visible? position close]}]
      (let [[left top] position
            close' (fn []
                     (reset! internal-state 0)
                     (close))
            value @parent-state
            value (if (= "-" value) 0 value)]
        [:div.counter-pane {:class [(when (not visible?) "hidden")]
                            :style {:top (str top "px") :left (str left "px")}}
         [:button {:on-click close'} "x"]
         [:span.bold (str (+ value @internal-state) " (" (with-sign @internal-state) ")")]
         (map button amounts)
         [:button {:on-click (fn []
                               (reset! parent-state (+ value @internal-state))
                               (close'))} "✓"]]))))

(defn counter-button
  [amounts]
  (fn []
    (let [internal-state (atom {})]
      (fn [count-state]
        [:span
         [:button
          {:on-click (fn [e]
                       (let [pos (.getBoundingClientRect (.-target e))]
                         (swap! internal-state assoc
                           count-state {:visible? true
                                        :position [(.-left pos)
                                                   (.-top pos)]})))}
          "+"]
         [counter-pane amounts count-state
          (assoc (select-keys (get @internal-state count-state)
                   #{:position :visible?})
            :close #(swap! internal-state update-in [count-state :visible?] not))]]))))

(def big-counter-button (partial counter-button [10 5 1 -1]))
(def small-counter-button (partial counter-button [5 3 1 -1]))

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
     [:div.active-conditions
      (map-indexed
        (fn [idx {:keys [name rounds]}]
          ^{:key name} [:span.condition
                        [:span name [:span.italic rounds]]
                        [small-counter-button (cursor creature-conditions [idx :rounds])]
                        [:button {:on-click #(remove-item creature-conditions idx)} "x"]])
        @creature-conditions)]]))

(defn ready-delay-pane
  [creature]
  [:div.ready-delay
   [:span.bold
    (cond
      (:readied? @creature) "Readied"
      (:delayed? @creature) "Delayed"
      :else (list
              [:button {:on-click #(swap! creature assoc :readied? true)} "ready"]
              [:button {:on-click #(swap! creature assoc :delayed? true)} "delay"]))]])

(defn clear-ready-delay
  [creature]
  (assoc creature
    :readied? false
    :delayed? false))

(defn move-creature
  [creatures a-idx b-idx]
  (if (<= 0 b-idx (dec (count @creatures)))
    (let [creature-a (nth @creatures a-idx)
          creature-b (nth @creatures b-idx)]
      (swap! creatures assoc
        a-idx creature-b
        b-idx (clear-ready-delay creature-a)))))

(defn creature
  [creature creatures idx started? active-creature]
  (let [{:keys [name damage hp init init-mod morale-threshold pc?
                readied? delayed?]} creature
        {:keys [lethal non-lethal]} damage
        active? (and (= idx active-creature)
                  started?)]
    [:div.creature {:class [(when active? "active")
                            (when (or readied? delayed?) "held")]}
     [:div.name.left
      [:span.bold.italic name]
      [:span.init-mod (with-sign init-mod)]]
     [:div.flex.left
      [:span.init "Init:"
       [:span.bold init]]
      (when-not started?
        [big-counter-button (cursor creatures [idx :init])])]
     (when-not pc?
       [:div.hp.flex.left
        [:span {:class (cond
                         (>= (+ lethal non-lethal) hp)       "ouch"
                         (>= lethal (- hp morale-threshold)) "flee")}
         "HP:" [:span.bold hp]]
        (when-not started?
          [big-counter-button (cursor creatures [idx :hp])])
        [:span "MC:"
         [:span.bold morale-threshold]]
        (when-not started?
          [big-counter-button (cursor creatures [idx :morale-threshold])])
        (when started?
          (list
            [:span "L:"
             [:span.bold lethal]]
            [big-counter-button (cursor creatures [idx :damage :lethal])]
            [:span "NL:"
             [:span.bold non-lethal]]
            [big-counter-button (cursor creatures [idx :damage :non-lethal])]))])
     [:div.clear]
     [ready-delay-pane (cursor creatures [idx])]
     [conditions-pane active-creature (cursor creatures [idx :conditions])]
     [:div.creature-controls
      [:div.right
       [:button {:on-click #(remove-item creatures idx)} "x"]]
      [:div
       [:button {:on-click #(move-creature creatures idx (dec idx))} "↑"]
       [:button {:on-click #(move-creature creatures idx (inc idx))} "↓"]]]]))

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
   :creatures (->> creatures
                (map-indexed
                  (fn [idx creature]
                    (cond-> creature
                      true (update :conditions
                             #(count-down-conditions next-creature-idx %))
                      (= idx next-creature-idx) clear-ready-delay)))
                vec)})

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
       [:div.round.left
        [:span.bold "Round " round]
        [:span.next-round
         [:button {:on-click next-creature}
          (if (= 0 round)
            "start"
            "next creature >>")]]]
       [:span.undo-redo
        [:button.undo {:on-click hist/undo!} "undo"]
        [:button.redo {:on-click hist/redo!} "redo"]]]
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
     (debug-out "state" @current-encounter)]))

(defn read-creatures
  [creatures]
  (vec
    (map new-creature creatures)))

(defn read-encounters
  [s]
  (reset! encounters
    (->> (reader/read-string s)
      (map #(update % :creatures read-creatures))
      vec)))

(defn load-encounters
  [f]
  (let [reader (js/FileReader.)]
    (set! (.-onload reader) #(read-encounters (.-result (.-target %))))
    (.readAsText reader f)))

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
    (if (seq @encounters)
      (map
        (fn [{:keys [name creatures]}]
          ^{:key name} [:div.encounter
                        [:button.encounter
                         {:on-click #(reset! current-encounter
                                       (new-encounter-state (concat @party creatures)))}
                         name]])
        @encounters)
      [:input {:type "file"
               :accept ".edn"
               :on-change #(load-encounters (-> % .-target .-files (.item 0)))}])]])

(defn container []
  [:div.container
   [sidebar]
   [encounter]])

(defn mount [el]
  (reagent/render-component [container] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

(mount-app-element)

(defn ^:after-load on-reload []
  (mount-app-element))
