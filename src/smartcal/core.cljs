(ns smartcal.core
  (:require [reagent.core :as r]
            [reagent.dom :as dom]
            [instaparse.core :as insta :refer [defparser]]
            [cljs.core.match :refer [match]]
            [clojure.string :as cstr]
            [goog.string :as gstr]))

(goog-define VERBOSE false)

;; -------------------------
;; Types

(defrecord Date [y m d weekday daynum])

(defn adjust-month
  [y m]
  (let [new-y (+ y (quot m 12)) new-m (mod m 12)] [new-y new-m]))

(defn ymd-to-day-num
  [y m d]
  (let [[y m] (adjust-month y m)
        ;; A cycle is 400 years
        cycles (quot (- y 1600) 400)
        remaining-years-in-cycle (mod (- y 1600) 400)
        ;; Each cycle has 97 leap years.
        leaps-from-cycles (* 97 cycles)
        ;; A century is 100 years.
        centuries (quot remaining-years-in-cycle 100)
        is-first-year-in-cycle? (== remaining-years-in-cycle 0)
        ;; Each century has 24 leap years from XX04 to XX96, except for the
        ;; first year of a cycle which is handled by first-year-in-cycle.
        leaps-from-centuries (* 24 centuries)
        remaining-years (mod remaining-years-in-cycle 100)
        leaps-from-years (quot remaining-years 4)
        is-leap? (or is-first-year-in-cycle?
                     (and (> remaining-years 0) (== (mod remaining-years 4) 0)))
        leaps (+ 1
                 leaps-from-cycles
                 leaps-from-centuries
                 leaps-from-years
                 ;; Remove the current year from consideration.
                 (if is-leap? -1 0))
        day-num-jan-1 (+ leaps (* 365 (- y 1600)))
        day-num-in-year
          (+ (aget #js [0 31 59 90 120 151 181 212 243 273 304 334] m)
             (if (and is-leap? (>= m 2)) 1 0)
             (- d 1))]
    (+ day-num-jan-1 day-num-in-year)))

(defn day-num-to-date
  [day-num]
  (assert (>= day-num 0))
  (let [days-in-4y (+ 1 (* 4 365))
        days-in-100y (+ 24 (* 100 365))
        days-in-400y (+ 97 (* 400 365))
        ;; For convenience, we want leap days to be at the end. Here
        ;; remaining-days is the number of days since 1200-03-01. So that
        ;; here a year begins in March. (Why not 1600-03-01? We hate
        ;; negative numbers in modulus calculations.)
        remaining-days (+ day-num (- days-in-400y 31 29))
        cycles (quot remaining-days days-in-400y)
        remaining-days (mod remaining-days days-in-400y)
        centuries (quot remaining-days days-in-100y)
        remaining-days (mod remaining-days days-in-100y)
        ;; Fix centuries being 4.
        is-centuries-4? (== centuries 4)
        remaining-days (if is-centuries-4? days-in-100y remaining-days)
        centuries (if is-centuries-4? 3 centuries)
        fouryears (quot remaining-days days-in-4y)
        remaining-days (mod remaining-days days-in-4y)
        remaining-years (quot remaining-days 365)
        remaining-days (mod remaining-days 365)
        ;; Fix remaining-years being 4.
        is-remaining-years-4? (== remaining-years 4)
        remaining-days (if is-remaining-years-4? 365 remaining-days)
        remaining-years (if is-remaining-years-4? 3 remaining-years)
        y (+ 1200
             (* 400 cycles)
             (* 100 centuries)
             (* 4 fouryears)
             remaining-years)
        ;; Since March is the first month, we don't have to think about
        ;; leap years here and February always has 29 days. If it is not
        ;; actually a leap year, the year calculation has taken care of it.
        days-in-month [31 30 31 30 31 31 30 31 30 31 31 29]
        [m remaining-days _]
          (reduce (fn [[cur-month remaining finished :as st] days-in-cur-month]
                    (if finished
                      st
                      (if (<= days-in-cur-month remaining)
                        [(inc cur-month) (- remaining days-in-cur-month) false]
                        [cur-month remaining true])))
            [0 remaining-days false]
            days-in-month)
        ;; Fix the month, since we previously assumed March is the first
        ;; month.
        m (+ 2 m)
        ;; Fix the year for the same reason.
        is-month-overlarge? (>= m 12)
        y (if is-month-overlarge? (inc y) y)
        m (if is-month-overlarge? (- m 12) m)
        d (inc remaining-days)]
    (Date. y m d (mod (+ 6 day-num) 7) day-num)))

(def epoch (day-num-to-date 0))

(def user-date-min (ymd-to-day-num 1900 0 1))

(def user-date-max (ymd-to-day-num 2099 11 31))

(defn is-daynum-within-limit?
  "We try to restrict user-visible dates to be within this range."
  [daynum]
  (and (>= daynum user-date-min) (<= daynum user-date-max)))

(defn month-num
  "Calculate the ordinal for a particular month from the epoch (1600)."
  [date]
  (+ (* 12 (- (:y date) 1600)) (:m date)))

(defn ymd-to-date [y m d] (day-num-to-date (ymd-to-day-num y m d)))

(defn ymd-map-to-date
  [{:keys [y m d]}]
  (day-num-to-date (ymd-to-day-num y m d)))

(defn ymd-map-to-date-checked
  "A version of ymd-map-to-date that performs checks to ensure the date range is
  within user limits."
  [{:keys [y m d]}]
  (let [dn (ymd-to-day-num y m d)]
    (if (is-daynum-within-limit? dn)
      (day-num-to-date dn)
      (throw {:date-outside-range dn}))))

(defn today
  []
  (let [js-date (js/Date.)]
    (day-num-to-date (ymd-to-day-num (.getFullYear js-date)
                                     (.getMonth js-date)
                                     (.getDate js-date)))))

;; -------------------------
;; Functions

(defn actual-start
  [start-date]
  (day-num-to-date (- (:daynum start-date) (:weekday start-date))))

(defn next-week
  ([ymd] (next-week 1 ymd))
  ([n ymd] (day-num-to-date (+ (:daynum ymd) (* n 7)))))

(defn prev-week ([ymd] (next-week -1 ymd)) ([n ymd] (next-week (- n) ymd)))

(defn modulo-remainder-seq
  "Returns a sequence of integers in a range where it is congruent to a specified value modulo another specified value."
  ([divisor val from]
   (let [remainder (mod val divisor)
         from-remainder (mod from divisor)
         first-selected (+ from (mod (- remainder from-remainder) divisor))]
     (iterate #(+ divisor %) first-selected)))
  ([divisor val from to]
   (take-while #(< % to) (modulo-remainder-seq divisor val from))))

(defn soonest-day-of-week
  "Return the daynum for the given day of week that is the soonest on or after the specified start date."
  [start dow]
  (+ (:daynum start) (mod (- dow (:weekday start)) 7)))

(defn weekdays-of-month
  "Find all days that are of the given day of week in a month. Returns a vector."
  [day-of-week m y]
  (let [day-1 (ymd-to-date y m 1)
        first-occurrence-day-num (soonest-day-of-week day-1 day-of-week)
        all-occurrences-days-num (take-while
                                   #(== (:m day-1) (:m (day-num-to-date %)))
                                   (iterate #(+ 7 %) first-occurrence-day-num))]
    (mapv day-num-to-date all-occurrences-days-num)))

(defn get-neg
  "Just like get on vector except it supports negative indexing."
  [v i]
  (get v (if (>= i 0) i (+ (count v) i))))

(defn all-nd-weekdays-of-month
  [occurrences day-of-week m y]
  (let [all-dows (weekdays-of-month day-of-week m y)]
    (->> occurrences
         (map #(get-neg all-dows %))
         (filter identity)
         (sort-by :d))))

(defn select-dates-from-month-recur
  "Select days from a monthly recurring pattern according to :day-selection."
  [recur-pat monthnum]
  (case (:day-selection recur-pat)
    :d (filter identity
         (map #(let [date (ymd-to-date 1600 monthnum %)]
                 ;; Need to filter away non-exist dates such as Feb 31
                 ;; instead of wrapping.
                 (if (= (:d date) %) date nil))
           (sort (:d recur-pat))))
    :dow (all-nd-weekdays-of-month (:occ recur-pat)
                                   (:dow recur-pat)
                                   monthnum
                                   1600)))

(defn select-dates-from-year-recur
  [recur-pat y]
  (case (:day-selection recur-pat)
    :md (let [date (ymd-to-date y (:m recur-pat) (:d recur-pat))]
          ;; Need to filter away non-exist dates such as Feb 31 instead
          ;; of wrapping.
          (if (and (= (:d date) (:d recur-pat))) [date] nil))
    :occ-dow-month
      (mapcat #(all-nd-weekdays-of-month (:occ recur-pat) (:dow recur-pat) % y)
        (:m recur-pat))))

(defn recurrent-event-occurrences
  [recur-pat default-recur-start query-start query-end]
  (let [recur-start (get recur-pat :recur-start default-recur-start)
        actual-start-daynum (max (:daynum query-start) (:daynum recur-start))
        actual-end-daynum (if-let [recur-end (get recur-pat :recur-end)]
                            (min (:daynum query-end) (:daynum recur-end))
                            (:daynum query-end))]
    (case (:recur-type recur-pat)
      :day (let [divisor (:freq recur-pat)
                 result-daynums (modulo-remainder-seq divisor
                                                      (:daynum recur-start)
                                                      actual-start-daynum
                                                      actual-end-daynum)]
             (map day-num-to-date result-daynums))
      :week (let [divisor (* 7 (:freq recur-pat))
                  first-week-occurrences-daynum
                    (map #(soonest-day-of-week recur-start %) (:dow recur-pat))
                  result-daynums (sort (mapcat #(modulo-remainder-seq
                                                  divisor
                                                  %
                                                  actual-start-daynum
                                                  actual-end-daynum)
                                         first-week-occurrences-daynum))]
              (map day-num-to-date result-daynums))
      :month
        ;; The semantics of determining the first occurrence is slightly
        ;; different from "week" mode: in week mode, we always start by
        ;; finding the soonest days matching the dow (which may be in the
        ;; following week); here, we always start from the current month.
        ;; The difference is deliberate since people have a stronger
        ;; conception of the month but not the week.
        (let [monthnum-divisor (:freq recur-pat)
              monthnum-recur-start (month-num recur-start)
              actual-start-monthnum (max (month-num query-start)
                                         monthnum-recur-start)
              actual-end-monthnum
                (inc (if-let [recur-end (get recur-pat :recur-end)]
                       (min (month-num recur-end) (month-num query-end))
                       (month-num query-end)))
              selected-months (modulo-remainder-seq monthnum-divisor
                                                    monthnum-recur-start
                                                    actual-start-monthnum
                                                    actual-end-monthnum)
              all-days (mapcat #(select-dates-from-month-recur recur-pat %)
                         selected-months)]
          (drop-while #(< (:daynum %) actual-start-daynum)
                      (take-while #(< (:daynum %) actual-end-daynum) all-days)))
      :year
        ;; The semantics of "year" mode is similar to "month" mode. TODO:
        ;; reduce code duplication.
        (let [year-divisor (:freq recur-pat)
              year-recur-start (:y recur-start)
              actual-start-y (max (:y query-start) year-recur-start)
              actual-end-y (inc (if-let [recur-end (get recur-pat :recur-end)]
                                  (min (:y recur-end) (:y query-end))
                                  (:y query-end)))
              selected-years (modulo-remainder-seq year-divisor
                                                   year-recur-start
                                                   actual-start-y
                                                   actual-end-y)
              all-days (mapcat #(select-dates-from-year-recur recur-pat %)
                         selected-years)]
          (drop-while #(< (:daynum %) actual-start-daynum)
                      (take-while #(< (:daynum %) actual-end-daynum)
                                  all-days))))))

(defn add-event
  [name date-spec events]
  (let [modified-date-spec
          (if (and (:recurring date-spec)
                   (nil? (:recur-start (:recurring date-spec))))
            (assoc-in date-spec [:recurring :recur-start] (today))
            date-spec)]
    (conj events (assoc modified-date-spec :name name))))

(defn remove-events
  "Remove events by name."
  [name events]
  (filterv #(not= (:name %) name) events))

(defn get-visible-events
  "Determine which events are visible in the current view, given the boundaries of
  the current view and all available events. Return a map with :date :event."
  [from until events]
  (mapcat (fn [ev]
            (if-let [single-occ (:single-occ ev)]
              (if (and (>= (:daynum single-occ) (:daynum from))
                       (< (:daynum single-occ) (:daynum until)))
                [{:date single-occ, :event ev}]
                nil)
              (if-let [recur-pat (:recurring ev)]
                (map #(hash-map :date % :event ev)
                  (recurrent-event-occurrences recur-pat epoch from until)))))
    events))

(defn get-days-with-events
  "Similar to get-visible-events except that the result is a map from date to a seq of event names."
  [from until events]
  (update-vals (group-by :date (get-visible-events from until events))
               #(mapv :event %)))

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

(def occurrence-ordinals-inv (clojure.set/map-invert occurrence-ordinals))

(defn format-date-en-us
  [{:keys [y m d]}]
  (str (get month-names m) " " d ", " y))

(defn day-to-ordinal
  [n]
  (str n
       (cond (and (not= n 11) (= (mod n 10) 1)) "st"
             (and (not= n 12) (= (mod n 10) 2)) "nd"
             (and (not= n 13) (= (mod n 10) 3)) "rd"
             :else "th")))

(defn format-freq
  "Formats the recurrent period. The plural is formed by simply adding an 's'."
  [recur-pat]
  (let [n (name (:recur-type recur-pat))]
    (str "every "
         (if (= 1 (:freq recur-pat)) n (str (:freq recur-pat) " " n "s")))))

(defn format-occ
  [recur-pat]
  (cstr/join ", "
             (map #(get occurrence-ordinals-inv %) (sort (:occ recur-pat)))))

(defn format-recur-pat
  [recur-pat]
  (let [start (if-let [start (:recur-start recur-pat)]
                (if (> (:daynum start) 0)
                  (str " from " (format-date-en-us start))))
        until (if-let [until (:recur-end recur-pat)]
                (str " until " (format-date-en-us until)))
        pat (case (:recur-type recur-pat)
              :day (format-freq recur-pat)
              :week (str "every " (if (= 1 (:freq recur-pat))
                                    "week"
                                    (str (:freq recur-pat) " weeks"))
                         " on " (cstr/join ", "
                                           (map #(get day-names %)
                                             (sort (:dow recur-pat)))))
              :month (str (format-freq recur-pat)
                          " on the "
                          (case (:day-selection recur-pat)
                            :d (cstr/join ", "
                                          (map day-to-ordinal
                                            (sort (:d recur-pat))))
                            :dow (str (format-occ recur-pat)
                                      " "
                                      (get day-names (:dow recur-pat)))))
              :year (str (format-freq recur-pat)
                         " on "
                         (case (:day-selection recur-pat)
                           :md (str (get month-names (:m recur-pat))
                                    " "
                                    (:d recur-pat))
                           :occ-dow-month
                             (str "the " (format-occ recur-pat)
                                  " " (get day-names (:dow recur-pat))
                                  " of " (cstr/join ", "
                                                    (map #(get month-names %)
                                                      (sort (:m
                                                              recur-pat))))))))]
    (str pat start until)))

(defn format-future-occurrences
  [recur-pat start until]
  (if (or (nil? start) (nil? until))
    ""
    (let [visible-occurrences-etc
            ;; TODO get rid of the implicit (today) so we can test this.
            (take 4 (recurrent-event-occurrences recur-pat (today) start until))
          displayed-occurrences (take 3 visible-occurrences-etc)
          ellipsis (= 4 (count visible-occurrences-etc))]
      (if (seq displayed-occurrences)
        (str " (occurring "
             (cstr/join "; " (map format-date-en-us displayed-occurrences))
             (if ellipsis "; \u2026")
             ")")))))

(defn format-event
  [name occ start until]
  (if-let [date (:single-occ occ)]
    (str "an event named \"" name "\" on " (format-date-en-us date))
    (if-let [recur-pat (:recurring occ)]
      (str "a recurrent event named \""
           name
           "\" repeating "
           (format-recur-pat recur-pat)
           (format-future-occurrences recur-pat start until)))))

(defn format-str-exprs
  [str-exprs]
  (cstr/join ", "
             (map #(if-let [pat (:str-glob-fun %)]
                     (str "matching the pattern \"" pat "\"")
                     (str "named \"" % "\""))
               str-exprs)))

(defn simplified-glob-to-regex
  "Transform a simplified version of the glob string matching language into regex.
  See glob(3), glob(7), fnmatch(3). Here we only support ? and * and their
  escapes."
  [glob-pat]
  (-> glob-pat
      (.split #"(\\\*|\\\?|\*|\?)")
      (.map #(case %
               "*" ".*"
               "?" "."
               "\\*" "\\*"
               "\\?" "\\?"
               (gstr/regExpEscape %)))
      (.join "")))

(defn eval-str-exprs
  [str-exprs event-names]
  (let [regex (js/RegExp. (str "^"
                               (cstr/join "|"
                                          (map #(if-let [pat (:str-glob-fun %)]
                                                  (simplified-glob-to-regex pat)
                                                  (gstr/regExpEscape %))
                                            str-exprs))
                               "$"))]
    (into #{} (filter #(.test regex %) event-names))))

;; -------------------------
;; State

(def initial-app-state {:weeks-to-show 15, :start-date (today)})

(def app-state-validators
  {:weeks-to-show #(and (integer? %) (>= % 1) (<= % 60)),
   :start-date #(and (map? %)
                     (:y %)
                     (:m %)
                     (:d %)
                     (>= (:y %) 1600)
                     (< (:y %) 2400)
                     (>= (:m %) 0)
                     (< (:m %) 12)
                     (>= (:d %) 1)
                     (<= (:d %) 31))})

(defn save-state
  [app-state]
  (update app-state :start-date #(select-keys % [:y :m :d])))

(defn load-state
  [reloaded-edn]
  (let [r (into {}
                (filter #(if-let [validator (get app-state-validators
                                                 (first %))]
                           (validator (second %))
                           false)
                  reloaded-edn))]
    (if (:start-date r) (update r :start-date ymd-map-to-date) r)))

(defn reload-from-local-storage
  []
  (try (load-state (js->clj (js/JSON.parse (.getItem js/window.localStorage
                                                     "appstate"))
                            :keywordize-keys
                            true))
       (catch :default e
         (do (when VERBOSE (println "Reloading from localStorage failed"))
             {}))))

(def app-state (r/atom (merge initial-app-state (reload-from-local-storage))))

(defn save-to-local-storage
  []
  (when VERBOSE (println "Saving to localStorage:" @app-state))
  (try (.setItem js/window.localStorage
                 "appstate"
                 (js/JSON.stringify (clj->js (save-state @app-state))))
       (catch :default e
         (when VERBOSE (println "Saving to localStorage failed")))))

(def saver (r/track! save-to-local-storage))

(def weeks-to-show (r/cursor app-state [:weeks-to-show]))

(def start-date (r/cursor app-state [:start-date]))

(def cmdline-input (r/atom ""))

(def cmdline-output (r/atom "Welcome to smartcal. Type \"help\" for help."))

;; This defines what kind of content the modal is showing. It may be nil,
;; :help, :ls-all, :ls-visible etc.
(def modal-content (r/atom nil))

;; When we decide to stop showing the modal, instead of resetting the
;; modal-content to nil, we change this instead to preserve the original modal
;; contents. This is nicer since we have an animation (technically a CSS
;; transition) to dismiss the modal. The user won't have to see a flash of
;; empty
;; modal.
(def modal-shown (r/atom false))

(defn show-modal [x] (reset! modal-shown true) (reset! modal-content x))
(defn hide-modal [] (reset! modal-shown false))

(def us-bank-holidays
  (mapv #(-> %
             (assoc :system true)
             (assoc-in [:recurring :recur-start] epoch)
             (assoc-in [:recurring :recur-type] :year)
             (assoc-in [:recurring :freq] 1))
    [{:name "New Year's Day", :recurring {:day-selection :md, :m 0, :d 1}}
     {:name "Martin Luther King Jr. Day",
      :recurring {:day-selection :occ-dow-month, :occ #{2}, :dow 1, :m #{0}}}
     {:name "Presidents' Day",
      :recurring {:day-selection :occ-dow-month, :occ #{2}, :dow 1, :m #{1}}}
     {:name "Memorial Day",
      :recurring {:day-selection :occ-dow-month, :occ #{-1}, :dow 1, :m #{4}}}
     {:name "Juneteenth", :recurring {:day-selection :md, :m 5, :d 19}}
     {:name "Independence Day", :recurring {:day-selection :md, :m 6, :d 4}}
     {:name "Labor Day",
      :recurring {:day-selection :occ-dow-month, :occ #{0}, :dow 1, :m #{8}}}
     {:name "Columbus Day",
      :recurring {:day-selection :occ-dow-month, :occ #{1}, :dow 1, :m #{9}}}
     {:name "Veterans Day", :recurring {:day-selection :md, :m 10, :d 11}}
     {:name "Thanksgiving Day",
      :recurring {:day-selection :occ-dow-month, :occ #{3}, :dow 4, :m #{10}}}
     {:name "Christmas Day", :recurring {:day-selection :md, :m 11, :d 25}}]))

(def events (r/atom us-bank-holidays))

;; -------------------------
;; Control language

(defparser
  cmdline-parser
  "cmd = <ws?> (help-cmd | add-cmd | ls-cmd | rm-cmd | next-cmd | prev-cmd | goto-cmd | config-cmd) <ws?>
   <ws> = #' +'
   help-cmd = <'help'>
   config-cmd = <'config' | 'settings' | 'prefs'>
   add-cmd = <'add' (ws 'event')? ws> str-lit <ws> (single-occ | recurring)
   ls-cmd = <'ls' | 'list'> (ls-all | ls-visible? | ls-only)
   ls-all = <ws 'all'>
   ls-visible = <ws 'visible'>
   ls-only = <ws 'only'> str-expr-star
   next-cmd = <('next' | 'n')> (<ws> int-lit | <ws? '(' ws?> int-expr <ws? ')'>)?
   prev-cmd = <('prev' | 'p')> (<ws> int-lit | <ws? '(' ws?> int-expr <ws? ')'>)?
   goto-cmd = <'goto' ws> date-expr
   rm-cmd = <('rm' | 'remove' | 'del' | 'delete') ws> str-lit
   <str-lit> = <'\"'>  #'[^\"]*' <'\"'>
   <str-expr> = str-lit | str-glob-fun
   str-glob-fun = <'glob' ws? '(' ws?> str-lit <ws? ')'>
   int-lit = #'[0-9]+'
   single-occ = <('on' ws)?> date-expr
   recurring
     = (recur-day | recur-week | recur-month | recur-year)
       recur-start? recur-end?
   recur-day = recur-day-freq
   recur-day-freq = <'every'> ((<ws> int-lit <ws> | <ws? '(' ws?> int-expr <ws? ')' ws?>) <'days'> | <ws 'day'>)
   recur-week
     = <'every' ws> dow-lit-plus
     | recur-week-freq <ws 'on' ws> dow-lit-plus
   recur-week-freq = <'every'> ((<ws> int-lit <ws> | <ws? '(' ws?> int-expr <ws? ')' ws?>) <'weeks'> | <ws 'week'>)
   recur-month
    = recur-month-freq <ws 'on' ws ('the' ws)?> recur-month-type
    | <'every' ws> recur-month-type <ws 'of' ws ('the' | 'each') ws 'month'>
   <recur-month-type> = recur-month-by-d | recur-month-by-dow
   recur-month-by-d = d-lit-plus
   recur-month-by-dow = occurrence-ordinal-plus <ws> dow-lit
   recur-month-freq = <'every'> ((<ws> int-lit <ws> | <ws? '(' ws?> int-expr <ws? ')' ws?>) <'months'> | <ws 'month'>)
   recur-year = recur-year-freq <ws 'on' ws> recur-year-type
   <recur-year-type> = recur-year-by-md | recur-year-by-occ-dow-month
   recur-year-by-md = md-lit
   recur-year-by-occ-dow-month = <('the' ws)?> occurrence-ordinal-plus <ws> dow-lit <ws 'of' ws> month-lit-plus
   recur-year-freq = <'every'> ((<ws> int-lit <ws> | <ws? '(' ws?> int-expr <ws? ')' ws?>) <'years'> | <ws 'year'>)
   recur-start = <ws 'from' ws> date-expr
   recur-end = <ws 'until' ws> date-expr
   <date-expr> = date-funs | date-lit
   <date-funs> = date-plus-fun | date-minus-fun
   date-plus-fun = <'plus' ws? '(' ws?> date-expr <ws? ',' ws?> interval-expr <ws? ')'>
   date-minus-fun = <'minus' ws? '(' ws?> date-expr <ws? ',' ws?> interval-expr <ws? ')'>
   <interval-expr> = day-interval-fun | month-interval-fun | year-interval-fun
   day-interval-fun = <'d' ws? '(' ws?> int-expr <ws? ')'> | int-lit <'d'>
   month-interval-fun = <'m' ws? '(' ws?> int-expr <ws? ')'> | int-lit <'m'>
   year-interval-fun = <'y' ws? '(' ws?> int-expr <ws? ')'> | int-lit <'y'>
   <int-expr> = int-add-sub-expr
   <int-add-sub-expr> = int-mul-quot-mod-expr | int-add-expr | int-sub-expr
   int-add-expr = int-add-sub-expr <ws? '+' ws?> int-mul-quot-mod-expr
   int-sub-expr = int-add-sub-expr <ws? '-' ws?> int-mul-quot-mod-expr
   <int-mul-quot-mod-expr> = int-term-expr | int-mul-expr | int-quot-expr | int-mod-expr
   int-mul-expr = int-mul-quot-mod-expr <ws? '*' ws?> int-term-expr
   int-quot-expr = int-mul-quot-mod-expr <ws? '/' ws?> int-term-expr
   int-mod-expr = int-mul-quot-mod-expr <ws? '%' ws?> int-term-expr
   <int-term-expr> = int-lit | <'(' ws?> int-add-sub-expr <ws? ')'>
   date-lit
     = yyyy-lit mm-lit dd-lit
     | yyyy-lit <'-'> mm-lit <'-'> dd-lit
     | yyyy-lit <ws> mm-lit <ws> dd-lit
     | md-lit <','? ws> yyyy-lit
   <md-lit>
     = (mmm-lit | mmmm-lit) <ws> d-lit
     | d-lit <ws> (mmm-lit | mmmm-lit)
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
   <ordinal-suffix> = 'st' | 'nd' | 'rd' | 'th'
   occurrence-ordinal = 'first' | 'second' | 'third' | 'fourth' | 'fifth' | 'last'

   (* Repetitions. It sucks that we don't have parametrized rules or the fact
      that we are not using combinators here. Here we want to allow either
      comma-separated with optional whitespace, or whitespace separated with
      mandatory whitespace. *)
   d-lit-plus = d-lit ((<ws? ',' ws?> d-lit)* | (<ws> d-lit)*)
   dow-lit-plus = dow-lit ((<ws? ',' ws?> dow-lit)* | (<ws> dow-lit)*)
   month-lit-plus = month-lit ((<ws? ',' ws?> month-lit)* | (<ws> month-lit)*)
   occurrence-ordinal-plus = occurrence-ordinal ((<ws? ',' ws?> occurrence-ordinal)* | (<ws> occurrence-ordinal)*)
   <str-expr-star> = (<ws> str-expr ((<ws? ',' ws?> str-expr)* | <ws> str-expr))?
  ")

(defn transform-parsed
  "This function transforms the parse tree generated by instaparse to make it
  nicer. The bulk of the date processing and recurrence pattern processing
  happens here."
  [parsed]
  (insta/transform
    {:int-lit #(js/parseInt % 10),
     :int-add-expr +,
     :int-sub-expr -,
     :int-mul-expr *,
     :int-quot-expr (fn [x y] (if (== y 0) 0 (quot x y))),
     :int-mod-expr (fn [x y] (if (== y 0) 0 (mod x y))),
     :day-interval-fun #(array-map :d %),
     :month-interval-fun #(array-map :m %),
     :year-interval-fun #(array-map :y %),
     :date-plus-fun (fn [date interval]
                      (ymd-map-to-date-checked (merge-with + date interval))),
     :date-minus-fun (fn [date interval]
                       (ymd-map-to-date-checked (merge-with - date interval))),
     :yyyy-lit (comp #(assoc nil :y %) #(js/parseInt % 10)),
     :mm-lit (comp #(assoc nil :m %) dec #(js/parseInt % 10)),
     :dd-lit (comp #(assoc nil :d %) #(js/parseInt % 10)),
     :d-lit (comp #(assoc nil :d %) #(js/parseInt % 10)),
     :mmm-lit #(assoc nil :m (.indexOf month-names %)),
     :mmmm-lit #(assoc nil :m (.indexOf full-month-names %)),
     :date-lit (comp ymd-map-to-date conj),
     :recur-day-freq (fn ([] {:freq 1}) ([f] {:freq f})),
     :recur-day (fn [& a] (into {:recur-type :day, :freq 1} a)),
     :short-dow-lit #(assoc nil :dow (.indexOf day-names %)),
     :full-dow-lit #(assoc nil :dow (.indexOf full-day-names %)),
     :dow-lit-plus (fn [& ms] {:dow (into #{} (map :dow ms))}),
     :recur-week-freq (fn ([] {:freq 1}) ([f] {:freq f})),
     :recur-week (fn [& a] (into {:recur-type :week, :freq 1} a)),
     :d-lit-plus (fn [& ms] {:d (into #{} (map :d ms))}),
     :recur-month-freq (fn ([] {:freq 1}) ([f] {:freq f})),
     :recur-month (fn [& a] (into {:recur-type :month, :freq 1} a)),
     :recur-month-by-d (fn [& a] (into {:day-selection :d} a)),
     :recur-month-by-dow (fn [& a] (into {:day-selection :dow} a)),
     :occurrence-ordinal #(assoc nil :occ (get occurrence-ordinals %)),
     :recur-year-freq (fn ([] {:freq 1}) ([f] {:freq f})),
     :recur-year (fn [& a] (into {:recur-type :year, :freq 1} a)),
     :recur-year-by-md (fn [& a] (into {:day-selection :md} a)),
     :recur-year-by-occ-dow-month (fn [& a]
                                    (into {:day-selection :occ-dow-month} a)),
     :month-lit-plus (fn [& ms] {:m (into #{} (map :m ms))}),
     :occurrence-ordinal-plus (fn [& ms] {:occ (into #{} (map :occ ms))}),
     :recurring (fn [b & a] {:recurring (into b a)}),
     :str-glob-fun (fn [pat] {:str-glob-fun pat}),
     :single-occ (fn [d] {:single-occ d})}
    parsed))

;; -------------------------
;; Components

(defn day-component
  [{:keys [y m d], :as ymd} show-complete events-on-this-date]
  [:div.td
   [:p.day
    (if show-complete
      (format-date-en-us ymd)
      (str (if (= 1 d) (str (get month-names m) " "))
           d
           (if (and (= 1 d) (= 0 m)) (str ", " y))))]
   (let [events-sorted (sort-by :name gstr/intAwareCompare events-on-this-date)]
     (into [:ul.events]
           (map (fn [ev] [:li
                          {:role "button",
                           :title (format-event (:name ev) ev nil nil),
                           :on-click #(do (show-modal [:ls-only #{(:name ev)}])
                                          (.stopPropagation %))} (:name ev)])
             events-sorted)))])

(def cmdline-prompt ">>> ")

(def cmdline-prompt-length (.-length cmdline-prompt))

(defn cmdline-display-component
  [[production-kw & remaining]]
  (into [:span {:class (name production-kw)}]
        (map #(cond (string? %) [:span %]
                    (vector? %) [cmdline-display-component %])
          remaining)))

(defn help-modal-component
  []
  [:<>
   [:p
    "This is a smart calendar app that runs completely in your browser. It is
    controlled by typing commands into the command area at the bottom."]
   [:details {:open true} [:summary "Navigating the calendar"]
    [:p "Type " [:code "next"] " or " [:code "prev"]
     " to move the calendar view forward or backward by one week. When followed
   with an integer, the calendar view is moved by that many weeks. For example "
     [:code "next 7"] " moves the calendar forward by 7 weeks."]
    [:p "To go to a specific date, use the " [:code "goto"]
     " command, followed by a date literal. There are many ways you can specify a
   date literal. So all of these work: "
     [:code "goto 20241001"] ", " [:code "goto 2024-10-01"] ", "
     [:code "goto Oct 1, 2024"] ", " [:code "goto 01 Oct 2024"]
     ". However you cannot specify the month as a number unless the year is specified first.
    This is because some put the day before the month, and some after, so a date
    like 10/01/2024 is inherently ambiguous."]]
   [:details {:open true} [:summary "Adding events"]
    [:p "The " [:code "add"]
     " command is used to add new events. An event always has a name, which need
   not be unique. An event can be a single occurrence or a recurring event."]
    [:p "A single event has its occurrence date specified using the "
     [:code "on"] " keyword. So "
     [:code "add \"Celebrate Jack's 60th birthday\" on 20241018"]
     " creates a single event with that name and on that date."]
    [:p "A recurring event has its recurrence pattern specified using the "
     [:code "every"]
     " keyword, followed by the recurrence period, which may be specified in
    units of days, weeks, months, or years."]
    [:p [:em "Day-based recurrence. "] "The command "
     [:code "add \"Daily reflection\" every day"] " is an example. The command "
     [:code "add \"Take out the trash\" every 3 days"] " is another example."]
    [:p [:em "Week-based recurrence. "]
     "Week-based recurrences are simply day-based recurrences where the period is
    a multiple of 7. It allows you to specify the days in the first 7 days of
    that period. The command "
     [:code "add \"TGIF\" every week on Fri"]
     " is an example. There may be multiple days specified, so "
     [:code "add \"Go to the gym\" every week on Mon, Fri"]
     " creates an event that repeats on Monday and Friday. (The repeated days
     may be separated by commas or simply whitespace, so "
     [:code "add \"Go to the gym\" every week on Mon Fri"]
     " also works, but the two styles cannot be mixed.) The command "
     [:code "add \"Get payslips\" every 2 weeks on Fri"]
     " sets 14 days as the period of recurrence, so only the first Friday of each
   period is specified."]
    [:p [:em "Month-based recurrence. "]
     "Month-based recurrences specify the period of recurrence in units of
    months, as well as the selection of a day within a month. The command "
     [:code "add \"Pay credit card\" every month on 28"]
     " sets the recurrence period to be one month, and it specifies the 28th day
    of each selected month. The command "
     [:code
      "add \"Review personal finances\" every 2 months on the first Saturday"]
     " sets the recurrence period to be two
    months, and in the first month of each period, specifies the first
    Saturday."]
    [:p [:em "Year-based recurrence. "]
     "Year-based recurrences similarly specify the period of recurrence in units
    of years, as well as the selection of a day within a year. The command "
     [:code "add \"Celebrate Dad's birthday\" every year on Apr 30"]
     " is an example. The command "
     [:code
      "add \"Pay property tax\" every year on the first Monday of Apr, Dec"]
     " is another example."]]
   [:details {:open true} [:summary "Listing and inspecting events"]
    [:p "The " [:code "ls"]
     " command can be used to list added events and inspect them. The output is
    shown graphically, containing the name of the event, whether it is recurrent
    or not, and if so, the recurrence pattern and the next occurrences visible.
    There is also a minigrid that highlights the occurrences."]
    [:p
     "Just like the POSIX ls tool, by default it hides any events not visible
    in the calendar. If you wish to look at all events including hidden ones,
    use "
     [:code "ls all"]
     " instead. You can also verbosely specify the default behavior with "
     [:code "ls visible"] "."]]])

(defn ls-modal-component
  [show-invisible selected-or-nil]
  (let [start (actual-start @start-date)
        until (day-num-to-date (+ (* 7 @weeks-to-show) (:daynum start)))]
    (into
      [:div#ls-grid]
      (mapcat
        (fn [ev]
          (if (or (nil? selected-or-nil) (contains? selected-or-nil (:name ev)))
            (let [days-with-event
                    (set (map :date (get-visible-events start until [ev])))]
              (if (or show-invisible (seq days-with-event))
                [(into [:div.ls-minigrid]
                       (for [x (range (* 7 @weeks-to-show))]
                         (let [date (day-num-to-date (+ x (:daynum start)))]
                           ^{:key (:daynum date)}
                           [:div.ls-minigrid-day
                            {:class (if (days-with-event date)
                                      "present"
                                      "absent")}])))
                 [:div.ls-desc [:h4 (:name ev)]
                  (if-let [date (:single-occ ev)]
                    [:p "On " (format-date-en-us date)]
                    (if-let [recur-pat (:recurring ev)]
                      [:details
                       [:summary "Repeating " (format-recur-pat recur-pat)]
                       (if-let [occs (seq (recurrent-event-occurrences recur-pat
                                                                       (today)
                                                                       start
                                                                       until))]
                         (into [:ul]
                               (map #(vector :li (format-date-en-us %)) occs))
                         [:p "No occurrences in visible date range."])]))]]))))
        (sort-by :name gstr/intAwareCompare @events)))))

(defn config-modal-component
  []
  [:div#control [:p "Weeks to display: "]
   [:input
    {:type "range",
     :value @weeks-to-show,
     :min 1,
     :max 60,
     :on-change (fn [e]
                  (let [new-value (js/parseInt (.. e -target -value))]
                    (reset! weeks-to-show new-value)))}] [:p @weeks-to-show]])

(defn execute-input
  [input]
  (reset! cmdline-output "")
  (reset! modal-shown false)
  (try
    (when-not (empty? (.trim input))
      (let [parsed (transform-parsed (cmdline-parser input))]
        (if (insta/failure? parsed)
          (reset! cmdline-output (pr-str parsed))
          (match parsed
            [:cmd [:next-cmd]] (swap! start-date next-week)
            [:cmd [:next-cmd n]] (swap! start-date #(next-week n %))
            [:cmd [:prev-cmd]] (swap! start-date prev-week)
            [:cmd [:prev-cmd n]] (swap! start-date #(prev-week n %))
            [:cmd [:goto-cmd ymd]] (reset! start-date ymd)
            [:cmd [:add-cmd name date-spec]]
              (swap! events #(add-event name date-spec %))
            [:cmd [:rm-cmd name]]
              (let [[old new] (swap-vals! events #(remove-events name %))
                    removals (- (count old) (count new))]
                (reset! cmdline-output (str "Removed "
                                            (if (== 1 removals)
                                              "one event."
                                              (str removals " events.")))))
            [:cmd [:help-cmd]] (show-modal :help)
            [:cmd [:config-cmd]] (show-modal :config)
            [:cmd [:ls-cmd]] (show-modal :ls-visible)
            [:cmd [:ls-cmd [t]]] (show-modal t)
            [:cmd [:ls-cmd [:ls-only & exprs]]]
              (let [selected-events (eval-str-exprs exprs (map :name @events))]
                (if (empty? selected-events)
                  (reset! cmdline-output
                    "No events selected for display in \"ls only\" command.")
                  (show-modal [:ls-only selected-events])))
            :else (js/window.alert (str "TODO: " (pr-str parsed)))))))
    (catch :default e
      (reset! cmdline-output
        (if (and (map? e) (:date-outside-range e))
          "The specified date is outside the supported range."
          "An unknown error occurred.")))))

(defn explain-input-component
  [input start until]
  (let [parsed (transform-parsed (cmdline-parser input))]
    (if-not (insta/failure? parsed)
      (match parsed
        [:cmd [:next-cmd]] "Scroll down by one week"
        [:cmd [:next-cmd n]] (str "Scroll down by " n " weeks")
        [:cmd [:prev-cmd]] "Scroll up by one week"
        [:cmd [:prev-cmd n]] (str "Scroll up by " n " weeks")
        [:cmd [:goto-cmd date]] (str "Go to date " (format-date-en-us date))
        [:cmd [:add-cmd name occ]] (str "Add "
                                        (format-event name occ start until))
        [:cmd [:rm-cmd name]] (str "Remove events named \"" name "\"")
        [:cmd [:ls-cmd]] "List events visible in the current view"
        [:cmd [:ls-cmd [:ls-all]]] "List all events"
        [:cmd [:ls-cmd [:ls-visible]]] "List events visible in the current view"
        [:cmd [:ls-cmd [:ls-only & exprs]]] (str "List events that are "
                                                 (format-str-exprs exprs))
        [:cmd [:help-cmd]] "Show help"
        [:cmd [:config-cmd]] "Configure calendar display"
        :else (pr-str parsed)))))

(defn cmdline-component
  [start until]
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
    (fn [start until]
      [:div#cmdline
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
           :class (if (empty? input) "" (if did-fail "failed" "succeeded"))}
          [:code cmdline-prompt
           (if (seq parsed) [cmdline-display-component parsed])
           (if-not (empty? input)
             [:span.comment " # "
              (if did-fail
                "Parse Error"
                [explain-input-component input start until])])]])])))

(defn cmdline-output-component
  []
  [:div#cmdline-out
   (let [o @cmdline-output] (if (seq o) [:pre [:code (.trimEnd o)]]))])

(defn modal-component
  []
  (match @modal-content
    nil [:div#modal.hidden]
    :help [:div#modal.help {:class (if @modal-shown "" "hidden")}
           [help-modal-component]]
    :config [:div#modal.config {:class (if @modal-shown "" "hidden")}
             [config-modal-component]]
    :ls-visible [:div#modal.ls {:class (if @modal-shown "" "hidden")}
                 [ls-modal-component false nil]]
    :ls-all [:div#modal.ls {:class (if @modal-shown "" "hidden")}
             [ls-modal-component true nil]]
    [:ls-only selected] [:div#modal.ls {:class (if @modal-shown "" "hidden")}
                         [ls-modal-component true selected]]))

(defn calendar-component
  []
  (let [start (actual-start @start-date)
        until (day-num-to-date (+ (* 7 @weeks-to-show) (:daynum start)))]
    [:div#cal [modal-component]
     [:div#table {:on-click #(hide-modal)} [:div.td.th "Sun"] [:div.td.th "Mon"]
      [:div.td.th "Tue"] [:div.td.th "Wed"] [:div.td.th "Thu"]
      [:div.td.th "Fri"] [:div.td.th "Sat"]
      (let [days-with-events (get-days-with-events start until @events)]
        (doall (for [x (range (* 7 @weeks-to-show))]
                 (let [date (day-num-to-date (+ x (:daynum start)))]
                   ^{:key (:daynum date)}
                   [day-component date (= x 0) (get days-with-events date)]))))]
     [:div#cmdline-inout [cmdline-output-component]
      [cmdline-component start until]]]))

(defn home-page [] [calendar-component])

;; -------------------------
;; Initialize app

(defn mount-root
  []
  (when VERBOSE (println "Initial app state " @app-state))
  (dom/render [home-page] (.getElementById js/document "app")))

(defn ^:dev/after-load init! [] (mount-root))
