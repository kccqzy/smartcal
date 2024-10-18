(ns smartcal.core
  (:require [reagent.core :as r]
            [reagent.dom :as dom]
            [instaparse.core :as insta :refer [defparser]]
            [cljs.core.match :refer [match]]
            [goog.string :as gstr]))

(goog-define VERBOSE false)

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
  ([ymd] (next-week 1 ymd))
  ([n ymd]
   (decompose-js-date (into-js-date (update-in ymd [:d] #(+ % (* n 7)))))))

(defn prev-week ([ymd] (next-week -1 ymd)) ([n ymd] (next-week (- n) ymd)))

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

(defn us-bank-holiday
  "Returns whether a day is a U.S. bank holiday. If so, return its name. Otherwise return nil."
  [{:keys [y m d]}]
  (cond (and (= m 0) (= d 1)) "New Year's Day"
        (and (= m 0) (= d (nd-weekday-of-month 2 1 m y)))
          "Martin Luther King Jr. Day"
        (and (= m 1) (= d (nd-weekday-of-month 2 1 m y))) "Presidents' Day"
        (and (= m 4) (= d (nd-weekday-of-month -1 1 m y))) "Memorial Day"
        (and (= m 5) (= d 19)) "Juneteenth"
        (and (= m 6) (= d 4)) "Independence Day"
        (and (= m 8) (= d (nd-weekday-of-month 0 1 m y))) "Labor Day"
        (and (= m 9) (= d (nd-weekday-of-month 1 1 m y))) "Columbus Day"
        (and (= m 10) (= d 11)) "Veterans Day"
        (and (= m 10) (= d (nd-weekday-of-month 3 4 m y))) "Thanksgiving Day"
        (and (= m 11) (= d 25)) "Christmas Day"))

(defn add-event
  [name {:keys [y m d]} events]
  (update-in events [y m d] #(conj % name)))

;; -------------------------
;; State

(def initial-app-state
  {:weeks-to-show 5, :start-date (decompose-js-date (js/Date.))})

(def app-state-validators
  {:weeks-to-show #(and (integer? %) (>= % 1) (<= % 12)),
   :start-date #(and (:y %)
                     (:m %)
                     (:d %)
                     (>= (:y %) 1900)
                     (< (:y %) 2100)
                     (= % (decompose-js-date (into-js-date %))))})

(defn load-state
  [reloaded-edn]
  (into {}
        (filter #(if-let [validator (get app-state-validators (first %))]
                   (validator (second %))
                   false)
          reloaded-edn)))

(defn reload-from-local-storage
  []
  (try (js->clj (js/JSON.parse (.getItem js/window.localStorage "appstate"))
                :keywordize-keys
                true)
       (catch :default e
         (do (when VERBOSE (println "Reloading from localStorage failed"))
             {}))))

(def app-state (r/atom (merge initial-app-state (reload-from-local-storage))))

(defn save-to-local-storage
  []
  (when VERBOSE (println "Saving to localStorage:" @app-state))
  (try (.setItem js/window.localStorage
                 "appstate"
                 (js/JSON.stringify (clj->js @app-state)))
       (catch :default e
         (when VERBOSE (println "Saving to localStorage failed")))))

(def saver (r/track! save-to-local-storage))

(def weeks-to-show (r/cursor app-state [:weeks-to-show]))

(def start-date (r/cursor app-state [:start-date]))

(def cmdline-input (r/atom ""))

(def cmdline-output (r/atom ""))

;; Currenty we just have a map from year to a map from month to a map from day
;; to a vec of event names. TODO: we really ought to have a custom structure
;; that directly supports assoc-in, get-in, and update-in passing {:y :m :d}.
;; TODO we also need a record for events. TODO we need to store this in
;; localStorage.
(def user-defined-events (r/atom {}))

;; -------------------------
;; Control language

(defparser
  cmdline-parser
  "cmd = <ws?> (help-cmd | add-cmd | display-cmd | next-cmd | prev-cmd | today-cmd | goto-cmd) <ws?>
   <ws> = #' +'
   help-cmd = <'help'>
   add-cmd = <'add' (ws 'event')? ws> str-lit <ws> (single-occ | recurring)
   display-cmd = 'display' ws 'week' ws int-lit
   next-cmd = <('next' | 'n')> (<ws> int-lit)?
   prev-cmd = <('prev' | 'p')> (<ws> int-lit)?
   today-cmd = <'today'>
   goto-cmd = <'goto' ws> date-lit
   <str-lit> = <'\"'>  #'[^\"]*' <'\"'>
   <int-lit> = #'[0-9]+'
   single-occ = <('on' ws)?> date-lit
   recurring
     = <'every' ws>
       (recur-day | recur-week | recur-month | recur-year)
       recur-start? recur-end?
   recur-day = recur-day-freq
   recur-day-freq = (int-lit <ws 'days'> | <'day'>)
   recur-week
     = dow-lit-plus
     | recur-week-freq <ws 'on' ws> dow-lit-plus
   recur-week-freq = (int-lit <ws 'weeks'> | <'week'>)
   recur-month
    = recur-month-freq <ws 'on' ws ('the' ws)?> (d-lit-plus | occurrence-ordinal <ws> dow-lit)
    | (<'on' ws 'the' ws>)? occurrence-ordinal <ws> dow-lit <ws 'of' ws ('the' | 'each') ws 'month'>
   recur-month-freq = (int-lit <ws 'months'> | <'month'>)
   recur-year
     = recur-year-freq <ws 'on' ws>
       (md-lit
       | <('the' ws)?> occurrence-ordinal <ws> dow-lit <ws 'of' ws> month-lit-plus)
   recur-year-freq = (int-lit <ws 'years'> | <'year'>)
   recur-start = <ws 'from' ws> date-lit
   recur-end
     = <ws 'until' ws> date-lit
     | <ws 'for' ws> int-lit <ws> 'times'
   date-lit
     = yyyy-lit mm-lit dd-lit
     | yyyy-lit <'-'> mm-lit <'-'> dd-lit
     | yyyy-lit <ws> mm-lit <ws> dd-lit
     | md-lit <','? ws> yyyy-lit
   <md-lit>
     = (mmm-lit | mmmm-lit) <ws> d-lit
     | d-lit <ws> (mmm-lit | mmmm-lit)
     | mm-lit dd-lit
   yyyy-lit = #'19[0-9][0-9]|20[0-9][0-9]'
   mm-lit = #'0[1-9]|1[0-2]'
   dd-lit = #'0[1-9]|1[0-9]|2[0-9]|3[01]'
   d-lit = (#'0?[1-9]' | #'1[0-9]|2[0-9]|3[01]') <ordinal-suffix?>
   mmm-lit = 'Jan' | 'Feb' | 'Mar' | 'Apr' | 'May' | 'Jun' | 'Jul' | 'Aug' | 'Sep' | 'Oct' | 'Nov' | 'Dec'
   mmmm-lit = 'January' | 'February' | 'March' | 'April' | 'May' | 'June' | 'July' | 'August' | 'September' | 'October' | 'November' | 'December'
   short-dow-lit = 'Sun' | 'Mon' | 'Tue' | 'Wed' | 'Thu' | 'Fri' | 'Sat'
   full-dow-lit = 'Sunday' | 'Monday' | 'Tuesday' | 'Wednesday' | 'Thursday' | 'Friday' | 'Saturday'
   <dow-lit> = short-dow-lit | full-dow-lit
   <month-lit> = mmm-lit | mmmm-lit
   d-lit-plus = d-lit (<ws? ',' ws?> d-lit)*
   dow-lit-plus = dow-lit (<ws? ',' ws?> dow-lit)*
   month-lit-plus = month-lit (<ws? ',' ws?> month-lit)*
   <ordinal-suffix> = 'st' | 'nd' | 'rd' | 'th'
   occurrence-ordinal = 'first' | 'second' | 'third' | 'fourth' | 'fifth' | 'last'
  ")

(def month-names
  ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"])

(def full-month-names
  ["January" "February" "March" "April" "May" "June" "July" "August" "September"
   "October" "November" "December"])

(def day-names ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"])

(def full-day-names
  ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"])

(def occurrence-ordinals
  {"first" 0, "second" 1, "third" 2, "fourth" 4, "fifth" 5, "last" -1})

(defn transform-parsed-dates
  [parsed]
  (insta/transform
    {:yyyy-lit (comp #(assoc nil :y %) #(js/parseInt % 10)),
     :mm-lit (comp #(assoc nil :m %) dec #(js/parseInt % 10)),
     :dd-lit (comp #(assoc nil :d %) #(js/parseInt % 10)),
     :d-lit (comp #(assoc nil :d %) #(js/parseInt % 10)),
     :mmm-lit #(assoc nil :m (.indexOf month-names %)),
     :mmmm-lit #(assoc nil :m (.indexOf full-month-names %)),
     :date-lit (comp decompose-js-date into-js-date conj),
     :recur-day-freq (fn ([] {:freq 1}) ([s] {:freq (js/parseInt s 10)})),
     :recur-day (fn [& a] (into {:recur-type :day, :freq 1} a)),
     :short-dow-lit #(assoc nil :dow (.indexOf day-names %)),
     :full-dow-lit #(assoc nil :dow (.indexOf full-day-names %)),
     :dow-lit-plus (fn [& ms] {:dow (into #{} (map :dow ms))}),
     :recur-week-freq (fn ([] {:freq 1}) ([s] {:freq (js/parseInt s 10)})),
     :recur-week (fn [& a] (into {:recur-type :week, :freq 1} a)),
     :d-lit-plus (fn [& ms] {:d (into #{} (map :d ms))}),
     :recur-month-freq (fn ([] {:freq 1}) ([s] {:freq (js/parseInt s 10)})),
     :recur-month (fn [& a] (into {:recur-type :month, :freq 1} a)),
     :occurrence-ordinal #(assoc nil :occ (get occurrence-ordinals %)),
     :recur-year-freq (fn ([] {:freq 1}) ([s] {:freq (js/parseInt s 10)})),
     :recur-year (fn [& a] (into {:recur-type :year, :freq 1} a)),
     :month-lit-plus (fn [& ms] {:m (into #{} (map :m ms))}),
     :recurring (fn [b & a] [:recurring (into b a)])}
    parsed))

;; -------------------------
;; Components

(defn day-component
  [{:keys [y m d], :as ymd} show-complete]
  [:div.td
   [:p.daynum
    (str (if (or show-complete (= 1 d)) (str (get month-names m) " "))
         d
         (if (or show-complete (and (= 1 d) (= 0 m))) (str ", " y)))]
   (let [user-events (get-in @user-defined-events [y m d] [])
         all-events (if-let [bank-hol (us-bank-holiday ymd)]
                      (conj user-events bank-hol)
                      user-events)
         all-events-sorted (sort-by identity gstr/intAwareCompare all-events)]
     (into [:ul.events] (map #(vector :li %) all-events-sorted)))])

(def cmdline-prompt ">>> ")

(def cmdline-prompt-length (.-length cmdline-prompt))

(defn cmdline-display-component
  [[production-kw & remaining]]
  (into [:span {:class (name production-kw)}]
        (map #(cond (string? %) [:span %]
                    (vector? %) [cmdline-display-component %])
          remaining)))

(defn execute-input
  [input]
  (reset! cmdline-output "")
  (let [parsed (transform-parsed-dates (cmdline-parser input))]
    (if (insta/failure? parsed)
      (reset! cmdline-output (pr-str parsed))
      (match parsed
        [:cmd [:next-cmd]] (swap! start-date next-week)
        [:cmd [:next-cmd n]] (swap! start-date #(next-week (js/parseInt n 10)
                                                           %))
        [:cmd [:prev-cmd]] (swap! start-date prev-week)
        [:cmd [:prev-cmd n]] (swap! start-date #(prev-week (js/parseInt n 10)
                                                           %))
        [:cmd [:today-cmd]] (reset! start-date (decompose-js-date (js/Date.)))
        [:cmd [:goto-cmd ymd]] (reset! start-date ymd)
        [:cmd [:add-cmd name [:single-occ ymd]]] (swap! user-defined-events
                                                   #(add-event name ymd %))
        :else (js/window.alert (str "TODO: " (pr-str parsed)))))))

(defn explain-input-component
  [input]
  (let [parsed (transform-parsed-dates (cmdline-parser input))]
    (if-not (insta/failure? parsed)
      (match parsed
        [:cmd [:next-cmd]] "Scroll down by one week"
        [:cmd [:next-cmd n]] (str "Scroll down by " n " weeks")
        [:cmd [:prev-cmd]] "Scroll up by one week"
        [:cmd [:prev-cmd n]] (str "Scroll up by " n " weeks")
        [:cmd [:today-cmd]] "Scroll to today"
        [:cmd [:goto-cmd {:y y, :m m, :d d}]]
          (str "Go to date " (get month-names m) " " d ", " y)
        [:cmd [:add-cmd name [:single-occ {:y y, :m m, :d d}]]]
          (str "Add an event named \"" name
               "\" on " (get month-names m)
               " " d
               ", " y)
        :else (pr-str parsed)))))

(defn cmdline-component
  []
  (let [textarea-ref (atom nil)
        textarea-change
          (fn [ev]
            ;; We do not support tab characters for now. The
            ;; browser is supposed to interpret the tab character
            ;; as focusing on the next input and should not
            ;; result in any real tab characters.
            (let [val (-> ev
                          .-target
                          .-value
                          (.replaceAll "\t" ""))]
              ;; The handling of the prompt is somewhat ad-hoc
              ;; and arbitrary. Basically the <textarea> element
              ;; doesn't have a way to restrict editing to some
              ;; portion of it. So the prompt is included.
              (cond
                ;; The user keeps the prompt and appends to it. Happy
                ;; case.
                (.startsWith val cmdline-prompt)
                  (let [input (.substring val cmdline-prompt-length)]
                    (if (> (.indexOf input "\n") -1)
                      (do (reset! cmdline-input "")
                          (execute-input (.replaceAll input "\n" "")))
                      (reset! cmdline-input input)))
                ;; The user tries to insert at the beginning.
                (.endsWith val cmdline-prompt)
                  (do (reset! cmdline-input (-> val
                                                (.slice
                                                  0
                                                  (- 0 cmdline-prompt-length))
                                                (.replaceAll "\n" "")))
                      (when-let [el @textarea-ref]
                        (let [end (+ (.-length cmdline-input)
                                     cmdline-prompt-length)]
                          (.setSelectionRange el end end))))
                ;; The user somehow removed the prompt and
                ;; replaced it with something short (hopefully
                ;; just a few characters).
                (< (.-length val) cmdline-prompt-length)
                  (do (reset! cmdline-input (-> val
                                                (.replaceAll ">" "")
                                                (.replaceAll " " "")
                                                (.replaceAll "\n" "")))
                      (when-let [el @textarea-ref]
                        (let [end (+ (.-length cmdline-input)
                                     cmdline-prompt-length)]
                          (.setSelectionRange el end end)))))))
        textarea-select (fn [ev]
                          (let [start (-> ev
                                          .-target
                                          .-selectionStart)
                                end (-> ev
                                        .-target
                                        .-selectionEnd)]
                            (when-let [el @textarea-ref]
                              (.setSelectionRange
                                el
                                (max start cmdline-prompt-length)
                                (max end cmdline-prompt-length)))))]
    (fn [] [:div#cmdline
            [:textarea#cmdline-in.cmdline
             {:ref #(reset! textarea-ref %),
              :spell-check "false",
              :value (str cmdline-prompt @cmdline-input),
              :on-change textarea-change,
              :on-select textarea-select}]
            (let [input @cmdline-input
                  parsed (cmdline-parser input :total true :unhide :all)
                  did-fail (insta/failure? parsed)]
              [:pre#cmdline-disp.cmdline
               {:aria-hidden "true",
                :class
                  (if (empty? input) "" (if did-fail "failed" "succeeded"))}
               [:code cmdline-prompt
                (if (seq parsed) [cmdline-display-component parsed])
                (if-not (empty? input)
                  [:span.comment " # "
                   (if did-fail
                     "Parse Error"
                     [explain-input-component input])])]])])))

(defn cmdline-output-component
  []
  [:div#cmdline-out
   (let [o @cmdline-output] (if (seq o) [:pre [:code (.trimEnd o)]]))])

(defn calendar-component
  []
  [:div#cal
   [:div#control [:p "Weeks to display: "]
    [:input
     {:type "range",
      :value @weeks-to-show,
      :min 1,
      :max 12,
      :on-change (fn [e]
                   (let [new-value (js/parseInt (.. e -target -value))]
                     (reset! weeks-to-show new-value)))}] [:p @weeks-to-show]]
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
   [:div#cmdline-inout [cmdline-output-component] [cmdline-component]]])

(defn home-page [] [calendar-component])

;; -------------------------
;; Initialize app

(defn mount-root
  []
  (dom/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! [] (mount-root))
