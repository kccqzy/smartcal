(ns smartcal.core
  (:require [reagent.core :as r]
            [reagent.dom :as dom]
            [instaparse.core :as insta :refer [defparser]]))

;; -------------------------
;; State

(def weeks-to-show (r/atom 5))

(def start-date (r/atom {:y 2025, :m 0, :d 1}))

(def cmdline-input (r/atom ""))

;; -------------------------
;; Functions

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
        all-occurrences-days
          (take-while #(= m (.getMonth (into-js-date {:y y, :m m, :d %})))
                      (iterate #(+ 7 %) first-occurrence-day))]
    (if (>= occurrence 0)
      (nth all-occurrences-days occurrence)
      (nth (reverse all-occurrences-days) (- -1 occurrence)))))

;; -------------------------
;; Control language

(defparser
  cmdline-parser
  "cmd = ws? (help-cmd | add-cmd | display-cmd | next-cmd | prev-cmd) ws?
   ws = #' +'
   help-cmd = 'help' (ws help-topic)?
   help-topic = 'add' | 'display' | 'next' | 'prev'
   add-cmd = 'add' (ws 'event')? ws str-lit
   display-cmd = 'display' ws 'week' ws int-lit
   next-cmd = ('next' | 'n') (ws int-lit)?
   prev-cmd = ('prev' | 'p') (ws int-lit)?
   str-lit = '\"'  #'[^\"]*' '\"'
   int-lit = #'[0-9]+'
  ")

(defn remove-ws
  [parsed]
  (if (vector? parsed)
    (if (= :ws (first parsed)) nil (filter some? (map remove-ws parsed)))
    parsed))

;; -------------------------
;; Components

(def month-names
  ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"])

(defn day-component
  [{:keys [y m d]} show-complete]
  [:div.td
   [:p.daynum
    (str (if (or show-complete (= 1 d)) (str (get month-names m) " "))
         d
         (if (or show-complete (and (= 1 d) (= 0 m))) (str ", " y)))]
   [:ul.events
    ;; Currently only bank holidays
    (cond (and (= m 0) (= d 1)) [:li "New Year's Day"]
          (and (= m 0) (= d (nd-weekday-of-month 2 1 m y)))
            [:li "Martin Luther King Jr. Day"]
          (and (= m 1) (= d (nd-weekday-of-month 2 1 m y))) [:li
                                                             "Presidents' Day"]
          (and (= m 4) (= d (nd-weekday-of-month -1 1 m y))) [:li
                                                              "Memorial Day"]
          (and (= m 5) (= d 19)) [:li "Juneteenth"]
          (and (= m 6) (= d 4)) [:li "Independence Day"]
          (and (= m 8) (= d (nd-weekday-of-month 0 1 m y))) [:li "Labor Day"]
          (and (= m 9) (= d (nd-weekday-of-month 1 1 m y))) [:li "Columbus Day"]
          (and (= m 10) (= d 11)) [:li "Veterans Day"]
          (and (= m 10) (= d (nd-weekday-of-month 3 4 m y)))
            [:li "Thanksgiving Day"]
          (and (= m 11) (= d 25)) [:li "Christmas Day"])]])

(def cmdline-prompt ">>> ")

(def cmdline-prompt-length (.-length cmdline-prompt))

(defn cmdline-display-component
  [[production-kw & remaining]]
  (into [:span {:class (name production-kw)}]
        (map #(cond (string? %) [:span %]
                    (vector? %) [cmdline-display-component %])
          remaining)))

(defn cmdline-component
  []
  (let [textarea-ref (atom nil)]
    (fn []
      [:div#cmdline
       [:textarea#cmdline-in.cmdline
        {:ref #(reset! textarea-ref %),
         :spell-check "false",
         :value (str cmdline-prompt @cmdline-input),
         :on-change (fn [ev]
                      (let [val (-> ev
                                    .-target
                                    .-value)]
                        ;; The handling of the prompt is somewhat ad-hoc
                        ;; and arbitrary. Basically the <textarea> element
                        ;; doesn't have a way to restrict editing to some
                        ;; portion of it. So the prompt is included.
                        (cond
                          ;; The user keeps the prompt and appends to it.
                          ;; Happy case.
                          (.startsWith val cmdline-prompt)
                            (reset! cmdline-input (.substring
                                                    val
                                                    cmdline-prompt-length))
                          ;; The user tries to insert at the beginning.
                          (.endsWith val cmdline-prompt)
                            (do (reset! cmdline-input
                                  (.slice val 0 (- 0 cmdline-prompt-length)))
                                (when-let [el @textarea-ref]
                                  (let [end (+ (.-length cmdline-input)
                                               cmdline-prompt-length)]
                                    (.setSelectionRange el end end))))
                          ;; The user somehow removed the prompt and
                          ;; replaced it with something short (hopefully
                          ;; just a few characters).
                          (< (.-length val) cmdline-prompt-length)
                            (do (reset! cmdline-input
                                  (clojure.string/replace val #"[> ]" ""))
                                (when-let [el @textarea-ref]
                                  (let [end (+ (.-length cmdline-input)
                                               cmdline-prompt-length)]
                                    (.setSelectionRange el end end))))))),
         :on-select (fn [ev]
                      (let [start (-> ev
                                      .-target
                                      .-selectionStart)
                            end (-> ev
                                    .-target
                                    .-selectionEnd)]
                        (when-let [el @textarea-ref]
                          (.setSelectionRange el
                                              (max start cmdline-prompt-length)
                                              (max end
                                                   cmdline-prompt-length)))))}]
       (let [parsed (cmdline-parser @cmdline-input :total true)
             did-fail (insta/failure? parsed)]
         (js/console.log (pr-str parsed))
         [:pre#cmdline-disp.cmdline
          {:aria-hidden "true",
           :style {:background-color (if (empty? @cmdline-input)
                                       "transparent"
                                       (if did-fail "#fcaca8" "#c2f3a2"))}}
          [:code cmdline-prompt
           (if (seq parsed) [cmdline-display-component parsed])
           (if-not (empty? @cmdline-input)
             [:span.comment " # "
              (if did-fail "Parse error" (pr-str (remove-ws parsed)))])]])])))

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
                 ^{:key (to-key date)} [day-component date (= x 0)]))))]
   [cmdline-component]])

(defn home-page [] [calendar-component])

;; -------------------------
;; Initialize app

(defn mount-root
  []
  (dom/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! [] (mount-root))
