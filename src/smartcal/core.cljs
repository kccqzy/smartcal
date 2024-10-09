(ns smartcal.core
  (:require [reagent.core :as r]
            [reagent.dom :as dom]))

;; -------------------------
;; Model

(def weeks-to-show (r/atom 5))

(def start-date (r/atom {:y 2025, :m 0, :d 1}))

(defn into-js-date [{:keys [y m d]}] (js/Date. y m d))

(defn decompose-js-date
  [jsdate]
  (let [d (.getDate jsdate)
        m (.getMonth jsdate)
        y (.getFullYear jsdate)]
    {:y y, :m m, :d d}))

(defn to-key [{:keys [y m d]}] (+ (* y 10000) (* m 100) d))

(defn actual-start
  [start-ymd]
  (let [jsdate (into-js-date start-ymd)
        day (.getDay jsdate)]
    (decompose-js-date (into-js-date (update-in start-ymd [:d] #(- % day))))))

(defn next-week
  [ymd]
  (decompose-js-date (into-js-date (update-in ymd [:d] #(+ % 7)))))

(defn prev-week
  [ymd]
  (decompose-js-date (into-js-date (update-in ymd [:d] #(- % 7)))))

(defn nd-weekday-of-month
  [occurrence day-of-week m y]
  (let [day-1 (into-js-date {:y y, :m m, :d 1})
        first-occurrence-day (+ 1 (mod (- day-of-week (.getDay day-1)) 7))
        occurrence-day (+ first-occurrence-day (* 7 (- occurrence 1)))]
    occurrence-day))

;; -------------------------
;; Views

(def month-names
  ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"])

(defn day-component
  [{:keys [y m d]}]
  [:div.td
   [:p.daynum
    (str (if (= 1 d) (str (get month-names m) " "))
         d
         (if (and (= 1 d) (= 0 m)) (str ", " y)))]
   [:ul.events
    ;; Currently only bank holidays
    (cond (and (= m 0) (= d 1)) [:li "New Year's Day"]
          (and (= m 0) (= d (nd-weekday-of-month 3 1 m y)))
            [:li "Martin Luther King Jr. Day"]
          (and (= m 1) (= d (nd-weekday-of-month 3 1 m y))) [:li
                                                             "Presidents' Day"]
          (and (= m 5) (= d 19)) [:li "Juneteenth"]
          (and (= m 6) (= d 4)) [:li "Independence Day"])]])

(defn calendar-component
  []
  [:div#cal [:h1 "Simple Calendar"]
   [:div#control [:p "Weeks to display: "]
    [:input
     {:type "range",
      :value @weeks-to-show,
      :min 1,
      :max 12,
      :on-change (fn [e]
                   (let [new-value (js/parseInt (.. e -target -value))]
                     (reset! weeks-to-show new-value)))}] [:p @weeks-to-show]]
   [:div#obsolete
    [:input
     {:type "button", :value "Prev", :on-click #(swap! start-date prev-week)}]
    [:input
     {:type "button", :value "Next", :on-click #(swap! start-date next-week)}]]
   [:div#table
    {:style {:grid-template-rows
               (str "30px repeat(" @weeks-to-show ", minmax(5rem, 1fr))")}}
    [:div.td.th "Sun"] [:div.td.th "Mon"] [:div.td.th "Tue"] [:div.td.th "Wed"]
    [:div.td.th "Thu"] [:div.td.th "Fri"] [:div.td.th "Sat"]
    (let [start (actual-start @start-date)]
      (doall (for [x (range (* 7 @weeks-to-show))]
               (let [date (decompose-js-date
                            (into-js-date (update-in start [:d] #(+ x %))))]
                 ^{:key (to-key date)} [day-component date]))))]])

(defn home-page [] [calendar-component])

;; -------------------------
;; Initialize app

(defn mount-root
  []
  (dom/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! [] (mount-root))
