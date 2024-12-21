(ns smartcal.core
  (:require [shadow.grove :as sg :refer (<< defc dev-log)]
            [shadow.grove.kv :as kv]
            [instaparse.core :as insta :refer [defparser]]
            [cljs.core.match :refer [match]]
            [clojure.string :as cstr]
            [clojure.set :as cset]
            [goog.string :as gstr]))

(goog-define VERBOSE false)

;; -------------------------
;; Types

(defrecord Date [y m d dow daynum])

(defn adjust-month
  "Adjust the year and month such that the month is within the correct range."
  [y m]
  (let [new-y (+ y (quot m 12)) new-m (mod m 12)] [new-y new-m]))

(defn ymd-to-day-num
  "Convert the year/month/day into a single number representing the days since
  January 1, 1600."
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
  "Convert the number of days since January 1, 1600 into year/month/day."
  [day-num]
  {:pre [(>= day-num 0)]}
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
        days-in-month #js [31 30 31 30 31 31 30 31 30 31 31 29]
        [m remaining-days] (loop [m 0
                                  remaining-days remaining-days]
                             (let [days-in-cur-month (aget days-in-month m)]
                               (if (<= days-in-cur-month remaining-days)
                                 (recur (inc m)
                                        (- remaining-days days-in-cur-month))
                                 #js [m remaining-days])))
        ;; Fix the month, since we previously assumed March is the first
        ;; month.
        m (+ m 2)
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

(defn ymd-to-date [y m d] (day-num-to-date (ymd-to-day-num y m d)))

(defn month-num
  "Calculate the ordinal for a particular month from the epoch (1600)."
  [date]
  (+ (* 12 (- (:y date) 1600)) (:m date)))

(defn month-num-day-to-date [monthnum d] (ymd-to-date 1600 monthnum d))

(defn week-num
  "Calculate the ordinal for a particular week from the epoch.
  The epoch is Saturday and is the only day with week-num 0."
  [date]
  (quot (+ 6 (:daynum date)) 7))

(defn week-num-day-to-date
  [weeknum dow]
  (day-num-to-date (+ (* weeknum 7) -6 dow)))

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

(defn date-comp [a b] (compare (:daynum a) (:daynum b)))

(defn date-set [] (sorted-set-by date-comp))

;; -------------------------
;; Event type and functions

(defrecord Event [evname specific-occs recur-pats])

(defn event?
  [x]
  ;; We cannot do an (instance? Event x) check because in hot-reloading, we
  ;; actually create new Event types.
  (and (contains? x :evname)
       (contains? x :specific-occs)
       (contains? x :recur-pats)
       (string? (:evname x))
       (set? (:specific-occs x))
       (vector? (:recur-pats x))
       (every? map? (:recur-pats x))))

(defn event-from-single-occ [name occ] (Event. name (conj (date-set) occ) []))

(defn event-from-single-rec [name rec] (Event. name (date-set) [rec]))

(defn merge-event
  "Merge an existing event (which may be nil) and a new event."
  [existing-event new-event]
  {:pre [(or (nil? existing-event) (event? existing-event)) (event? new-event)
         (or (nil? existing-event)
             (= (:evname existing-event) (:evname new-event)))],
   :post [(event? %)]}
  (let [existing-specific-occs (or (:specific-occs existing-event) (date-set))
        existing-recur-pats (or (:recur-pats existing-event) [])
        new-occ (:specific-occs new-event)
        new-recur-pat (:recur-pats new-event)]
    (Event. (:evname new-event)
            (reduce conj existing-specific-occs new-occ)
            (reduce conj existing-recur-pats new-recur-pat))))

;; -------------------------
;; Other Functions

(defn actual-start
  [start-date]
  (day-num-to-date (- (:daynum start-date) (:dow start-date))))

(defn next-week
  ([ymd] (next-week 1 ymd))
  ([n ymd] (day-num-to-date (+ (:daynum ymd) (* n 7)))))

(defn prev-week ([ymd] (next-week -1 ymd)) ([n ymd] (next-week (- n) ymd)))

(defn modulo-remainder-seq
  "Returns a sequence of integers in a range where it is congruent to a specified
  value modulo another specified value."
  [divisor val from to]
  {:pre [(integer? divisor) (integer? val) (integer? from) (integer? to)
         (>= divisor 1)]}
  (if (== divisor 1)
    (range from to)
    (range (+ from (mod (- val from) divisor)) to divisor)))

(defn modulo-remainder-rseq
  "Same as modulo-remainder-seq but reversed."
  [divisor val from to]
  {:pre [(integer? divisor) (integer? val) (integer? from) (integer? to)
         (>= divisor 1)],
   :post [(= % (reverse (modulo-remainder-seq divisor val from to)))]}
  (if (== divisor 1)
    (range (dec to) (dec from) -1)
    (range (- (+ to (mod (- val to) divisor)) divisor) (dec from) (- divisor))))

(defn modulo-remainder-seq-not-empty?
  [divisor val from to]
  {:post [(= % (not (empty? (modulo-remainder-seq divisor val from to))))]}
  (< (+ from (mod (- val from) divisor)) to))

(defn modulo-remainder-seq-sole-element
  [divisor val from to]
  {:post [(if (nil? %)
            (not= 1 (count (modulo-remainder-seq divisor val from to)))
            (and (= 1 (count (modulo-remainder-seq divisor val from to)))
                 (= % (first (modulo-remainder-seq divisor val from to)))))]}
  (let [fst (+ from (mod (- val from) divisor))]
    (if (and (< fst to) (>= (+ fst divisor) to)) fst)))

(defn soonest-day-of-week
  "Return the daynum for the given day of week that is the soonest on or after the specified start date."
  [start dow]
  (+ (:daynum start) (mod (- dow (:dow start)) 7)))

(defn weekdays-of-month
  "Find all days that are of the given day of week in a month. Returns a vector."
  [day-of-week m y]
  (let [day-1 (ymd-to-date y m 1)
        first-occurrence-day-num (soonest-day-of-week day-1 day-of-week)
        all-occurrences (keep #(let [date (day-num-to-date %)]
                                 (if (== (:m day-1) (:m date)) date))
                              (range first-occurrence-day-num
                                     (+ 31 first-occurrence-day-num)
                                     7))]
    (into [] all-occurrences)))

(defn get-neg
  "Like get on a vector, except works with negative indices."
  [v i]
  {:pre [(vector? v) (integer? i)]}
  (if (>= i 0) (get v i) (get v (+ (count v) i))))

(defn all-nd-weekdays-of-month
  "Find all days that are of the given ordinals of the given day of week in a
  month. The ordinals may be non-negative (usual indexing) or negative (counting
  from the back.)"
  [occurrences day-of-week m y]
  {:pre [(set? occurrences)],
   :post [(= %
             (dedupe (sort-by :d
                              (keep
                                #(get-neg (weekdays-of-month day-of-week m y) %)
                                occurrences))))]}
  (let [all-dows (weekdays-of-month day-of-week m y)
        cnt (count all-dows)]
    (keep-indexed (fn [idx date]
                    (if (or (contains? occurrences idx)
                            (contains? occurrences (- idx cnt)))
                      date))
                  all-dows)))

(defn select-dates-from-week-recur
  "Given a weekly recurrence pattern and a seq of week numbers, return a seq of
  dates for the selected days within the given weeks."
  [recur-pat weeknums]
  {:pre [(keyword-identical? :week (:recur-type recur-pat))
         (= weeknums (sort weeknums)) (seq (:dow recur-pat))],
   :post [(= % (sort-by :daynum %))]}
  (for [weeknum weeknums
        dow (sort (:dow recur-pat))]
    (week-num-day-to-date weeknum dow)))

(defn select-dates-from-month-recur
  "Given a monthly recurrence pattern and a seq of month numbers, return a seq of
  dates for the selected days within the given months."
  [recur-pat monthnums]
  {:pre [(keyword-identical? :month (:recur-type recur-pat))
         (= monthnums (sort monthnums))],
   :post [(= % (sort-by :daynum %))]}
  (case (:day-selection recur-pat)
    :d (do (assert (seq (:d recur-pat)))
           (for [monthnum monthnums
                 d (sort (:d recur-pat))
                 :let [date (month-num-day-to-date monthnum d)]
                 ;; Need to filter away non-existent dates such as Feb 31
                 ;; instead of wrapping.
                 :when (== (:d date) d)]
             date))
    :dow (do (assert (integer? (:dow recur-pat)))
             (assert (seq (:occ recur-pat)))
             (for [monthnum monthnums
                   dt (all-nd-weekdays-of-month (:occ recur-pat)
                                                (:dow recur-pat)
                                                monthnum
                                                1600)]
               dt))))

(defn select-dates-from-year-recur
  "Given a yearly recurrence pattern and a seq of years, return a seq of dates for
  the selected days within the given years."
  [recur-pat years]
  {:pre [(keyword-identical? :year (:recur-type recur-pat))
         (= years (sort years))],
   :post [(= % (sort-by :daynum %))]}
  (case (:day-selection recur-pat)
    :md (for [y years
              :let [date (ymd-to-date y (:m recur-pat) (:d recur-pat))]
              ;; Need to filter away non-existent dates such as Feb 31
              ;; instead of wrapping.
              :when (== (:d date) (:d recur-pat))]
          date)
    :occ-dow-month
      (for [y years
            m (sort (:m recur-pat))
            dt (all-nd-weekdays-of-month (:occ recur-pat) (:dow recur-pat) m y)]
        dt)))

(defn recurrent-event-occurrences
  "Given a recurrence pattern and a default start date if that start date is not
  present inside the recurrence pattern, return a seq of dates for the selected
  days within the query window."
  [{:keys [recur-type recur-start], :as recur-pat} query-start query-end]
  {:pre [(integer? (:freq recur-pat)) (contains? recur-pat :recur-start)]}
  (let [actual-start-daynum (max (:daynum query-start) (:daynum recur-start))
        actual-end-daynum (if-let [recur-end (get recur-pat :recur-end)]
                            (min (:daynum query-end) (:daynum recur-end))
                            (:daynum query-end))
        divisor (:freq recur-pat)
        to-period-num (case recur-type
                        :day :daynum
                        :week week-num
                        :month month-num
                        :year :y)
        period-recur-start (to-period-num recur-start)
        actual-start (max (to-period-num query-start) period-recur-start)
        actual-end (inc (if-let [recur-end (get recur-pat :recur-end)]
                          (min (to-period-num recur-end)
                               (to-period-num query-end))
                          (to-period-num query-end)))
        selected-periods (modulo-remainder-seq divisor
                                               period-recur-start
                                               actual-start
                                               actual-end)
        all-matching-days
          (case recur-type
            :day (map day-num-to-date selected-periods)
            :week (select-dates-from-week-recur recur-pat selected-periods)
            :month (select-dates-from-month-recur recur-pat selected-periods)
            :year (select-dates-from-year-recur recur-pat selected-periods))]
    (drop-while #(< (:daynum %) actual-start-daynum)
                (take-while #(< (:daynum %) actual-end-daynum)
                            all-matching-days))))

(defn gcd
  [a b]
  {:pre [(integer? a) (integer? b)]}
  (if (== b 0) a (recur b (mod a b))))

(defn recur-month-possibly-feb
  "Whether or not February can be reached in this month recurrence where the
  starting month and period are specified.

  This is calculating whether there exists some natural number x such that
  s+x*p===1 (mod 12). So we have x*p===(1-s)(mod 12). This equation has
  solutions if and only if gcd(p,12) divides (1-s).
  "
  [start-month period]
  {:pre [(integer? period) (> period 0) (integer? start-month)]}
  (or (== 1 start-month) (== 0 (mod (- 1 start-month) (gcd period 12)))))

(defn find-first-occ
  "Find the first occurrence of a recurrence series. NOTE: for pathological data,
  this function is quite slow because the first occurrence can be really far
  away. The function recurrent-event-occurrences should be preferred since it
  takes a query window which is limited by the UI."
  [{:keys [recur-start recur-type], :as recur-pat}]
  {:pre [(contains? recur-pat :recur-start)],
   :post [(or (nil? %)
              (= (recurrent-event-occurrences recur-pat
                                              epoch
                                              (day-num-to-date (inc (:daynum
                                                                      %))))
                 [%]))]}
  (case recur-type
    :day recur-start
    :week (first (drop-while #(< (:daynum %) (:daynum recur-start))
                             (select-dates-from-week-recur
                               recur-pat
                               (let [wn (week-num recur-start)]
                                 [wn (+ wn (:freq recur-pat))]))))
    :month
      (first
        (drop-while
          #(< (:daynum %) (:daynum recur-start))
          (select-dates-from-month-recur
            recur-pat
            (cons
              ;; The current month has an occurrence.
              (month-num recur-start)
              ;; The following month(s) instead. The basic idea is that we
              ;; keep iterating to find a month that satisfies the
              ;; condition. Sometimes this will never satisfy, such as. Feb
              ;; 31. So we cannot allow infinite iteration. Instead we have
              ;; an upper bound on the number of iterations. And lazy-seq
              ;; is just here to avoid the computation for this upper bound
              ;; when the current month is already enough.
              (lazy-seq
                (let [max-iter (case (:day-selection recur-pat)
                                 ;; If the day selection is :d, and if the
                                 ;; user wishes to select the 29th day.
                                 ;; Because the frequency may be a multiple
                                 ;; of 12, it may end up only selecting
                                 ;; February. The worst case scenario is
                                 ;; when the starting year is 25 (mod 400)
                                 ;; and the period is also 25. We need 15
                                 ;; iterations to reach 0 (mod 400).
                                 :d 15
                                 ;; If the day selection is :dow, the worst
                                 ;; case scenario is selecting the fifth
                                 ;; occurrence of a certain day.
                                 ;; Empirically we need 457 iterations
                                 ;; maximum if Feb is possible, otherwise
                                 ;; 120 iterations.
                                 :dow (if (= (:occ recur-pat) #{4})
                                        (if (recur-month-possibly-feb
                                              (:m recur-start)
                                              (:freq recur-pat))
                                          457
                                          120)
                                        1))
                      step (:freq recur-pat)
                      start (+ step (month-num recur-start))
                      end (+ start (* max-iter step))]
                  (range start end step)))))))
    :year
      (first
        (drop-while
          #(< (:daynum %) (:daynum recur-start))
          (select-dates-from-year-recur
            recur-pat
            (cons
              ;; The current year has an occurrence.
              (:y recur-start)
              ;; The following year(s) instead.
              (lazy-seq
                (let [max-iter (case (:day-selection recur-pat)
                                 ;; If the user wishes to select Feb 29,
                                 ;; run more iterations. The worst case
                                 ;; scenario is when the starting year is
                                 ;; 25 (mod 400) and the period is also 25.
                                 ;; We need 15 iterations to reach 0 (mod
                                 ;; 400).
                                 :md 15
                                 ;; Empirically we need 327 iterations in
                                 ;; the worst case if February is involved.
                                 ;; If the month has
                                 ;; 30 days, 230 iterations. Otherwise 178.
                                 :occ-dow-month
                                   (let [max-iters-per-month
                                           #js [178 327 178 230 178 230 178 178
                                                230 178 230 178]]
                                     (reduce (fn [cur-max m]
                                               (max cur-max
                                                    (aget max-iters-per-month
                                                          m)))
                                       0
                                       (:m recur-pat))))
                      step (:freq recur-pat)
                      start (+ step (:y recur-start))
                      end (+ start (* max-iter step))]
                  (range start end step)))))))))

(defn find-last-occ
  "Find the last occurrence of a recurrence series. If there is no occurrence,
  return nil (but the caller should have already detected this through the
  function find-first-occ).

  This function is not particularly optimized because several functions it calls
  returns values in the normal and not reversed order."
  [{:keys [freq recur-start recur-end recur-type], :as recur-pat}]
  {:pre [(not (nil? recur-end))],
   :post [(or (nil? %)
              (= [%] (recurrent-event-occurrences recur-pat % recur-end)))]}
  (if (keyword-identical? recur-type :day)
    (some-> (first (modulo-remainder-rseq freq
                                          (:daynum recur-start)
                                          (:daynum recur-start)
                                          (:daynum recur-end)))
            (day-num-to-date))
    (let [to-period-num (case recur-type
                          :week week-num
                          :month month-num
                          :year :y)
          selector (case recur-type
                     :week select-dates-from-week-recur
                     :month select-dates-from-month-recur
                     :year select-dates-from-year-recur)
          start (to-period-num recur-start)
          end (inc (to-period-num recur-end))
          periods (modulo-remainder-rseq freq start start end)]
      (->> periods
           (map #(selector recur-pat [%]))
           (drop-while #(or (empty? %)
                            (>= (:daynum (first %)) (:daynum recur-end))))
           (first)
           ;; Maybe nil here, but it is interpreted as the empty seq.
           (take-while #(< (:daynum %) (:daynum recur-end)))
           (last)))))

(defn adjust-recur-bounds
  "Adjusts the recur-start such that it coincides with the first day of actual
  occurrence, and adjust the recur-end such that it is one past the last day of
  actual occurrence."
  [recur-pat]
  {:pre [(contains? recur-pat :recur-start)]}
  ;; TODO: do not dissoc :recur-end for efficiency reasons
  (let [adjusted-recur-start (find-first-occ (dissoc recur-pat :recur-end))]
    (if (or (nil? adjusted-recur-start)
            (and (contains? recur-pat :recur-end)
                 (>= (:daynum adjusted-recur-start)
                     (:daynum (:recur-end recur-pat)))))
      nil
      (let [recur-pat (assoc recur-pat :recur-start adjusted-recur-start)]
        (if (nil? (:recur-end recur-pat))
          recur-pat
          (let [last-occ (find-last-occ recur-pat)]
            (assert (not (nil? last-occ)))
            (assoc recur-pat
              :recur-end (day-num-to-date (inc (:daynum last-occ))))))))))

(defn merges-by
  "Merge sorted sequences, removing duplicates."
  ([keyfn x] x)
  ([keyfn x y]
   (cond (empty? x) y
         (empty? y) x
         (= (keyfn (first x)) (keyfn (first y)))
           (cons (first x) (lazy-seq (merges-by keyfn (rest y) (rest x))))
         (< (keyfn (first x)) (keyfn (first y)))
           (cons (first x) (lazy-seq (merges-by keyfn y (rest x))))
         :else (cons (first y) (lazy-seq (merges-by keyfn x (rest y))))))
  ([keyfn x y & more]
   (apply merges-by
     keyfn
     (for [[a b] (partition-all 2 (list* x y more))] (merges-by keyfn a b)))))

(defn event-to-dates
  "Determine the occurrences of an event in the current view. Return a vec of dates."
  [from until event]
  {:pre [(event? event)],
   :post [(vector? %) (every? map? %) (every? :daynum %)]}
  (into []
        ;; TODO Is it really a good idea to merge sorted rather than using
        ;; sorted-set?
        (apply merges-by
          :daynum
          ;; Specific occurrences.
          (subseq (:specific-occs event) >= from < until)
          ;; Recurrences.
          (map #(recurrent-event-occurrences % from until)
            (:recur-pats event)))))

(defn adjust-start-end-with-freq
  "Adjust the start and end of abstract periods to make later operations nicer.

  The start value is adjusted to be a multiple of the period.

  The end is the greatest value such that it does not include any new values
  into the arithmetic progression.

  TODO: explain the motivation of this adjustment."
  [start end freq]
  {:pre [(integer? start) (integer? freq) (or (nil? end) (integer? end))]}
  [(- start (mod start freq))
   (if (nil? end) nil (+ end (mod (- start end) freq)))])

(defn day-rec-to-period
  "Convert a day recurrence pattern into an abstract recurrence period."
  ;; TODO: consider whether the abstract recurrence period should have been
  ;; present since the very beginning.
  [{:keys [freq recur-start recur-end], :as day-rec}]
  (let [start-daynum (:daynum recur-start)
        end-daynum (:daynum recur-end)
        [adjusted-start-daynum adjusted-end-daynum]
          (adjust-start-end-with-freq start-daynum end-daynum freq)]
    (-> day-rec
        (dissoc :recur-start)
        (dissoc :recur-end)
        (assoc :recur-period-remainder (mod start-daynum freq))
        (assoc :recur-period-start adjusted-start-daynum)
        (assoc :recur-period-end adjusted-end-daynum))))

(defn split-partial-period
  "Split a period by modifying the intra-period day selection, based on comparison
  against a split point. The comparison should be >=, >, <=, or < but this is
  not enforced. The rec must be a partial period where the start and end differ
  by 1."
  [{period :recur-period-start, :as rec} split-point comp]
  {:pre [(= (:freq rec) 1) (= (:recur-period-remainder rec) 0)
         (= 1 (- (:recur-period-end rec) (:recur-period-start rec)))]}
  (case (:recur-type rec)
    :week (do (assert (<= (:daynum (week-num-day-to-date period 0))
                          (:daynum split-point)
                          (:daynum (week-num-day-to-date period 6)))
                      "split point must be within range")
              (update rec
                      :dow
                      (fn [dows]
                        {:post [(not (empty? %))]}
                        (into (hash-set)
                              (filter #(comp % (:dow split-point)) dows)))))
    :month (do
             (assert (<= (:daynum (month-num-day-to-date period 1))
                         (:daynum split-point)
                         (dec (:daynum (month-num-day-to-date (inc period) 1))))
                     "split point must be within range")
             (case (:day-selection rec)
               :d (update rec
                          :d
                          (fn [ds]
                            {:post [(not (empty? %))]}
                            (into (hash-set)
                                  (filter #(comp % (:d split-point)) ds))))
               :dow (let [all-wds (weekdays-of-month (:dow rec) period 1600)]
                      (update rec
                              :occ
                              (fn [occs]
                                {:post [(not (empty? %))]}
                                (into (hash-set)
                                      (filter #(let [d (get-neg all-wds %)]
                                                 (and (not (nil? d))
                                                      (comp (:d d)
                                                            (:d split-point))))
                                        occs)))))))))

(defn week-month-rec-to-periods
  "Convert a week/month recurrence pattern into some abstract recurrence
  periods. In the ideal case there will be only one period. But since we do not
  allow any further filtering of start and end dates in this abstract
  representation, there may be some separate periods for incomplete recurrences.
  An incomplete recurrence refers to the scenario where the period containing
  the recur-start and/or recur-end has some occurrences before the
  recur-start/recur-end and some other."
  [{:keys [recur-type freq recur-start recur-end], :as rec}]
  {:pre [(or (= :week recur-type) (= :month recur-type))]}
  (let [to-period-num (case recur-type
                        :week week-num
                        :month month-num)
        start (to-period-num recur-start)
        remainder (mod start freq)
        end (if (nil? recur-end) nil (to-period-num recur-end))
        rv-tmpl (-> rec
                    (dissoc :recur-start)
                    (dissoc :recur-end))
        first-period-all-selected-days
          (case recur-type
            :week (select-dates-from-week-recur rec [start])
            :month (select-dates-from-month-recur rec [start]))
        [first-period-skipped-occ first-period-occ]
          (split-with #(< (:daynum %) (:daynum recur-start))
                      first-period-all-selected-days)
        front-partial-period (if (and (seq first-period-occ)
                                      (seq first-period-skipped-occ))
                               (-> rv-tmpl
                                   (assoc :recur-period-remainder 0)
                                   (assoc :freq 1)
                                   (assoc :recur-period-start start)
                                   (assoc :recur-period-end (inc start))
                                   (split-partial-period recur-start >=)))
        final-period-all-selected-days
          (if (nil? end)
            nil
            (case recur-type
              :week (select-dates-from-week-recur rec [end])
              :month (select-dates-from-month-recur rec [end])))
        [final-period-occ final-period-skipped-occ]
          (split-with #(< (:daynum %) (:daynum recur-end))
                      final-period-all-selected-days)
        back-partial-period (if (and (seq final-period-skipped-occ)
                                     (seq final-period-occ))
                              (-> rv-tmpl
                                  (assoc :recur-period-remainder 0)
                                  (assoc :freq 1)
                                  (assoc :recur-period-start end)
                                  (assoc :recur-period-end (inc end))
                                  (split-partial-period recur-end <)))
        start (if (and (empty? first-period-skipped-occ) (seq first-period-occ))
                start
                (+ freq start))
        end (if (nil? end)
              nil
              (if (and (seq final-period-occ) (empty? final-period-skipped-occ))
                (+ freq end)
                end))
        [start end] (adjust-start-end-with-freq start end freq)
        middle-periods (if (or (nil? end) (> end start))
                         (-> rv-tmpl
                             (assoc :recur-period-remainder remainder)
                             (assoc :recur-period-start start)
                             (assoc :recur-period-end end)))]
    (assert
      (or (nil? front-partial-period)
          (not= (dissoc front-partial-period
                  :recur-period-start
                  :recur-period-end
                  :recur-period-remainder
                  :freq)
                (dissoc rec
                  :recur-period-start
                  :recur-period-end
                  :recur-period-remainder
                  :freq)))
      "if there is a front partial period, then its attributes must have been different")
    (assert
      (or (nil? back-partial-period)
          (not= (dissoc back-partial-period
                  :recur-period-start
                  :recur-period-end
                  :recur-period-remainder
                  :freq)
                (dissoc rec
                  :recur-period-start
                  :recur-period-end
                  :recur-period-remainder
                  :freq)))
      "if there is a back partial period, then its attributes must have been different")
    (remove nil? [front-partial-period middle-periods back-partial-period])))

(defn rec-period-has-occ
  "Determine whether a recurrence has at least one occurrence."
  [{from :recur-period-start,
    to :recur-period-end,
    rem :recur-period-remainder,
    divisor :freq}]
  (or (nil? to) (modulo-remainder-seq-not-empty? divisor rem from to)))

(defn rec-period-sole-occ
  "Determine whether a recurrence has exactly one occurrence. If so, return that
  occurrence; otherwise, return nil."
  [{from :recur-period-start,
    to :recur-period-end,
    rem :recur-period-remainder,
    divisor :freq}]
  (if (nil? to) nil (modulo-remainder-seq-sole-element divisor rem from to)))

(defn period-to-day-rec
  "Convert a day recurrence with abstract periods back into recur-start and
  recur-end."
  [{:keys [recur-period-start recur-period-end recur-period-remainder freq],
    :as day-rec}]
  (let [start (day-num-to-date
                (+ recur-period-start
                   (mod (- recur-period-remainder recur-period-start) freq)))
        result (-> day-rec
                   (dissoc :recur-period-remainder)
                   (dissoc :recur-period-start)
                   (dissoc :recur-period-end)
                   (assoc :recur-start start))]
    (if (nil? recur-period-end)
      result
      (assoc result :recur-end (day-num-to-date recur-period-end)))))

(defn period-to-week-rec
  "Convert a week recurrence with abstract periods back into recur-start and
  recur-end."
  [{:keys [recur-period-start recur-period-end recur-period-remainder freq dow],
    :as week-rec}]
  (let [start (week-num-day-to-date
                (+ recur-period-start
                   (mod (- recur-period-remainder recur-period-start) freq))
                0)
        result (-> week-rec
                   (dissoc :recur-period-remainder)
                   (dissoc :recur-period-start)
                   (dissoc :recur-period-end)
                   (assoc :recur-start start))]
    (if (nil? recur-period-end)
      result
      (assoc result :recur-end (week-num-day-to-date recur-period-end 0)))))

(defn split-recs-without-overlap
  "Split a seq of recurrence patterns into new patterns without any overlapping
  start and end."
  [recs]
  {:pre [(every? map? recs) (every? #(integer? (:recur-period-start %)) recs)
         (every? #(or (nil? (:recur-period-end %))
                      (integer? (:recur-period-end %)))
                 recs)]}
  (let [split-points (into (sorted-set)
                           (mapcat (fn [{:keys [recur-period-start
                                                recur-period-end]}]
                                     (conj (if (nil? recur-period-end)
                                             nil
                                             (conj nil recur-period-end))
                                           recur-period-start))
                             recs))]
    (mapcat (fn [{:keys [recur-period-start recur-period-end], :as rec}]
              (let [relevant-points
                      (if (nil? recur-period-end)
                        (subseq split-points >= recur-period-start)
                        (subseq split-points
                                >=
                                recur-period-start
                                <=
                                recur-period-end))
                    intervals (if (nil? recur-period-end)
                                (partition 2 1 nil relevant-points)
                                (partition 2 1 relevant-points))]
                (map (fn [[new-start new-end]]
                       (-> rec
                           (assoc :recur-period-start new-start)
                           (assoc :recur-period-end new-end)))
                  intervals)))
      recs)))

(defn rec-group-remove-redundant-high-divisors
  "Optimize a group (where the period start and end are identical) using the rule
  that given [x===r1(mod d1), x===r2(mod d2)], if d2 mod d1=0 and r2 mod d1=r1
  then the latter can be eliminated. For example [x===1(mod 2), x===3(mod 4)]
  can be simplified to [x===1(mod 2)]."
  [grouped-recs]
  {:pre [(sequential? grouped-recs)
         (apply = (map :recur-period-start grouped-recs))
         (apply = (map :recur-period-end grouped-recs))
         (every? integer? (map :freq grouped-recs))]}
  ;; WARNING: QUADRATIC LOOP
  (persistent!
    (reduce (fn [grp {r2 :recur-period-remainder, d2 :freq, :as new}]
              (if (some (fn [i]
                          (let [{r1 :recur-period-remainder, d1 :freq}
                                  (get grp i)]
                            (and (== 0 (mod d2 d1)) (== r1 (mod r2 d1)))))
                        (range (count grp)))
                grp
                (conj! grp new)))
      (transient [])
      (sort-by :freq grouped-recs))))

(defn divisors
  "Find all divisors for a positive integer."
  [n]
  {:pre [(integer? n) (> n 0)]}
  (let [[asc desc] (reduce (fn [[asc desc :as acc] i]
                             (if (== 0 (mod n i))
                               (let [q (quot n i)]
                                 (if (== i q)
                                   [(conj! asc i) desc]
                                   [(conj! asc i) (conj! desc q)]))
                               acc))
                     [(transient []) (transient [])]
                     (range 1 (inc (Math/floor (Math/sqrt n)))))]
    (concat (persistent! asc) (rseq (persistent! desc)))))

(defn opt-divisor-remainders-set
  "Given a divisor and a set of remainders, representing the union of sets
  bigcup_i {x===r_i(mod d) | x}, optimize it into smaller divisors when possible."
  [d rs]
  {:pre [(integer? d) (> d 0) (set? rs) (every? integer? rs)
         (every? #(>= % 0) rs) (every? #(< % d) rs)]}
  (if (empty? rs)
    []
    (loop [rs (transient rs)
           result (transient [])
           divisors-to-check (divisors d)
           r 0]
      (assert (seq divisors-to-check))
      (assert (> (count rs) 0))
      (let [d2 (first divisors-to-check)
            cnt (quot d d2)]
        (if (< (count rs) cnt)
          (recur rs result (rest divisors-to-check) 0)
          (let [required-rs (range r d d2)]
            (if (every? rs required-rs)
              (let [new-result (conj! result [d2 r])
                    new-rs (reduce disj! rs required-rs)]
                (cond (empty? new-rs) (persistent! new-result)
                      (or (== (inc r) d2) (< (count new-rs) cnt))
                        (recur new-rs new-result (rest divisors-to-check) 0)
                      :else
                        (recur new-rs new-result divisors-to-check (inc r))))
              (if (== (inc r) d2)
                (recur rs result (rest divisors-to-check) 0)
                (recur rs result divisors-to-check (inc r))))))))))

(defn rec-group-reduce-large-period-single-freq
  "Optimize a group (where the period start, end, and freq are identical) using
  the rule that given [x===r1(mod d), x===r2(mod d), ...] if there exists some
  d2 such that d mod d2 = 0, and r1 mod d2 = r2 mod d2 = ... = r for every
  multiple of d2 then it can be replaced with x===r(mod d2). For
  example [x===0(mod 4), x===2(mod 4)] can be simplified to [x===0(mod 2)],
  and [x===0(mod 2), x===1(mod 2)] can be simplified to [x===0(mod 1)]."
  [recs]
  {:pre [(apply = (map :freq recs))]}
  (let [distinct-rems (into (hash-set) (map :recur-period-remainder recs))
        rec (first recs)
        d (:freq rec)
        result (opt-divisor-remainders-set d distinct-rems)]
    (map (fn [[d2 r]]
           (-> rec
               (assoc :recur-period-remainder r)
               (assoc :freq d2)))
      result)))

(defn rec-group-reduce-large-period
  "Same as rec-group-reduce-large-period-single-freq, except that it also handles
  recs with different periods by starting from large periods."
  [recs]
  {:pre [(seq recs)]}
  (let [freq-groups (group-by :freq recs)]
    (if (== 1 (count freq-groups))
      (rec-group-reduce-large-period-single-freq recs)
      (loop [freq-groups (into (sorted-map) freq-groups)
             current-group (first (first (rseq freq-groups)))]
        (assert (sorted? freq-groups))
        (let [current-vals (get freq-groups current-group)
              updated-vals (rec-group-reduce-large-period-single-freq
                             current-vals)
              updated-vals-grouped (group-by :freq updated-vals)
              new-freq-groups (merge-with into
                                          (dissoc freq-groups current-group)
                                          updated-vals-grouped)
              next-group (first (first
                                  (rsubseq new-freq-groups < current-group)))]
          (if (nil? next-group)
            (apply concat (vals new-freq-groups))
            (recur new-freq-groups next-group)))))))

(defn recombine-periods
  "Recombine different recurrence patterns. This function assumes that the caller
  has grouped the recurrences by everything other than start and end, so only an
  analysis on the start and end would be necessary."
  [grouped-recs]
  {:pre [(sequential? grouped-recs)
         (every? #(integer? (:recur-period-start %)) grouped-recs)
         (every? #(or (nil? (:recur-period-end %))
                      (integer? (:recur-period-end %)))
                 grouped-recs)]}
  (loop [result (transient [])
         recs grouped-recs]
    (if (empty? recs)
      (persistent! result)
      (let [rec (first recs)
            rest-recs (rest recs)]
        (if (== 0 (count result))
          (recur (conj! result rec) rest-recs)
          (let [last-rec (nth result (dec (count result)))]
            (assert (not (nil? (:recur-period-end last-rec)))
                    "only the last rec may have no end")
            (assert (<= (:recur-period-end last-rec) (:recur-period-start rec))
                    "recs must not overlap")
            (if (== (:recur-period-end last-rec) (:recur-period-start rec))
              (let [new-start (:recur-period-start last-rec)
                    new-rec (assoc rec :recur-period-start new-start)]
                (recur (-> result
                           (pop!)
                           (conj! new-rec))
                       rest-recs))
              (recur (conj! result rec) rest-recs))))))))

(defn try-get-single-occ-from-rec
  "Try to extract the only occurrence from a recurrence if it has only one occurrence."
  [rec]
  (if-let [end (:recur-period-end rec)]
    (let [occs (modulo-remainder-seq (:freq rec)
                                     (:recur-period-remainder rec)
                                     (:recur-period-start rec)
                                     end)]
      (if (nil? (next occs)) (first occs)))))

(defn try-absorb-one-forwards
  "Try to absorb one or more single occurrences by expanding the recurrence
  forwards (i.e. moving the recur-period-end later)."
  [single-occs {:keys [recur-period-end freq recur-period-remainder], :as rec}]
  {:pre [(set? single-occs)]}
  (loop [single-occs (transient single-occs)
         next-occ (+ recur-period-end
                     (mod (- recur-period-remainder recur-period-end) freq))
         rec (transient rec)]
    (if (contains? single-occs next-occ)
      (recur (disj! single-occs next-occ)
             (+ next-occ freq)
             (assoc! rec :recur-period-end (+ next-occ freq)))
      [(persistent! single-occs) (persistent! rec)])))

(defn try-absorb-one-backwards
  "Try to absorb one or more single occurrences by expanding the recurrence
  backwards (i.e. moving the recur-period-start earlier)."
  [single-occs
   {:keys [recur-period-start freq recur-period-remainder], :as rec}]
  {:pre [(set? single-occs)]}
  (loop [single-occs (transient single-occs)
         prev-occ (let [diff (mod (- recur-period-start recur-period-remainder)
                                  freq)
                        adjusted-diff (if (== 0 diff) freq diff)]
                    (- recur-period-start adjusted-diff))
         rec (transient rec)]
    (if (contains? single-occs prev-occ)
      (recur (disj! single-occs prev-occ)
             (- prev-occ freq)
             (assoc! rec :recur-period-start prev-occ))
      [(persistent! single-occs) (persistent! rec)])))

(defn try-absorb-one
  "Try to absorb one or more single occurrences by eliminating those that coincide
  with the given recurrence, and also expanding the recurrence both backwards
  and forwards."
  [single-occs
   {:keys [recur-period-start recur-period-end recur-period-remainder freq],
    :as rec}]
  (let [single-occs (into (hash-set)
                          ;; Filter away occs that correspond to the
                          ;; recurrence.
                          ;; TODO: consider whether this linear-time
                          ;; filtering should be rewritten so that the
                          ;; overall algorithm is not quadratic.
                          (remove (fn [occ]
                                    (and (>= occ recur-period-start)
                                         (or (nil? recur-period-end)
                                             (< occ recur-period-end))
                                         (== (mod occ freq)
                                             recur-period-remainder)))
                            single-occs))
        [s1 r1] (try-absorb-one-forwards single-occs rec)]
    (try-absorb-one-backwards s1 r1)))

(defn try-absorb
  "Try to absorb one or more single occurrences into multiple recurrences. This
  simply iterates over all recurrences."
  [single-occs multi-occ-recs]
  {:pre [(seq multi-occ-recs)]}
  (loop [single-occs single-occs
         multi-occ-recs multi-occ-recs
         rv (transient [])]
    (if (or (empty? single-occs) (empty? multi-occ-recs))
      (let [rv (reduce conj! rv multi-occ-recs)
            a-rec (get rv 0)
            rv (transduce (map (fn [occ]
                                 {:pre [(map? a-rec)]}
                                 (-> a-rec
                                     (assoc :freq 1)
                                     (assoc :recur-period-start occ)
                                     (assoc :recur-period-end (inc occ))
                                     (assoc :recur-period-remainder 0))))
                          conj!
                          rv
                          single-occs)]
        (persistent! rv))
      (let [[new-single-occs new-rec] (try-absorb-one single-occs
                                                      (first multi-occ-recs))]
        (recur new-single-occs (rest multi-occ-recs) (conj! rv new-rec))))))

(defn absorb-single-occs-from-recs
  "Find single-occurrence recurrences and try to absorb them into other larger
  recurrences."
  [original-recs]
  (loop [single-occs (transient [])
         multi-occ-recs (transient [])
         recs original-recs]
    (if (empty? recs)
      (if (== 0 (count multi-occ-recs))
        ;; When there are no multi-occ recurrences, we don't know how to
        ;; absorb them so we give up. TODO: find a way.
        original-recs
        (try-absorb (persistent! single-occs) (persistent! multi-occ-recs)))
      (if-let [occ (try-get-single-occ-from-rec (first recs))]
        (recur (conj! single-occs occ) multi-occ-recs (rest recs))
        (recur single-occs (conj! multi-occ-recs (first recs)) (rest recs))))))

(defn optimize-recs-generic
  [recs]
  ;; This feels very ad-hoc to me: it's like a compiler's optimizer being
  ;; from a manually composed set of passes.
  (->>
    recs
    ;; We need sort-by and partition-by; if we just used group-by the
    ;; period starts are still unsorted, violating preconditions for
    ;; passes later in the pipeline.
    (sort-by :recur-period-start)
    (partition-by :recur-period-start)
    (mapcat #(rec-group-reduce-large-period
               (rec-group-remove-redundant-high-divisors %)))
    (group-by (juxt :freq :recur-period-remainder))
    (vals)
    (mapcat recombine-periods)
    (absorb-single-occs-from-recs)
    ;; The following three passes are repeats. Here is a concrete example
    ;; of why. Consider the following four recs as an intermediate value:
    ;;
    ;;     freq | remainder | start | end
    ;;     ==============================
    ;;        3 |         2 |    96 |  99
    ;;        3 |         1 |    99 | 102
    ;;        3 |         2 |    99 | 102
    ;;        1 |         0 |   102 | nil
    ;;
    ;; But the recombine-periods pass combines the first and the third. So
    ;; we run the following functions again.
    (split-recs-without-overlap)
    (filter rec-period-has-occ)
    (absorb-single-occs-from-recs)
    ;; And then we need another recombine. See unit tests for reason.
    (sort-by :recur-period-start)
    (group-by (juxt :freq :recur-period-remainder))
    (vals)
    (mapcat recombine-periods)))

(defn optimize-day-recs
  [day-recs]
  (->> day-recs
       (map day-rec-to-period)
       (split-recs-without-overlap)
       (filter rec-period-has-occ)
       (optimize-recs-generic)
       (map period-to-day-rec)))

(defn merge-week-recs
  [recs]
  {:pre [(not (empty? recs)) (every? map? recs)
         (every? #(= (:recur-type %) :week) recs) (apply = (map :freq recs))
         (apply = (map :recur-period-start recs))
         (apply = (map :recur-period-end recs))]}
  (assoc (first recs) :dow (apply cset/union (map :dow recs))))

(defn optimize-week-recs
  [week-recs]
  (->> week-recs
       (mapcat week-month-rec-to-periods)
       (split-recs-without-overlap)
       (filter rec-period-has-occ)
       (group-by (juxt :freq :recur-period-remainder :recur-period-start))
       (vals)
       (map merge-week-recs)
       (group-by :dow)
       (vals)
       (mapcat optimize-recs-generic)
       (map period-to-week-rec)))

(defn optimize-month-recs [x] x) ;; TODO
(defn optimize-year-recs [x] x) ;; TODO

(defn- optimize-event-once
  "Optimize the recurrences of an event. The notion of optimality is not precisely
  defined, but it corresponds to a subjective notion of ease of comprehension."
  [{:keys [recur-pats], :as event}]
  {:pre [(event? event) (every? :recur-start recur-pats)]}
  ;; TODO include event single-occs for optimization
  (let [grouped-recs (group-by :recur-type recur-pats)
        opt-day (optimize-day-recs (:day grouped-recs))
        opt-week (optimize-week-recs (:week grouped-recs))
        opt-month (optimize-month-recs (:month grouped-recs))
        opt-year (optimize-year-recs (:year grouped-recs))]
    (assoc event
      :recur-pats (into []
                        (keep adjust-recur-bounds
                              (concat opt-day opt-week opt-month opt-year))))))

(defn optimize-event
  "Same as optimize-event-once, but with a check that the function is idempotent."
  [event]
  {:post [(= % (optimize-event-once %))]}
  (optimize-event-once event))

(defn merge-and-possibly-optimize-event
  [existing new opt]
  (let [merged (merge-event existing new)]
    (if opt (optimize-event merged) merged)))

(defn get-visible-events
  "Determine which events are visible in the current view, given the boundaries of
  the current view and all available events. Return a map with :date :event."
  [from until events]
  (mapcat (fn [ev]
            (map #(array-map :date % :event ev) (event-to-dates from until ev)))
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
  {"first" 0, "second" 1, "third" 2, "fourth" 3, "fifth" 4, "last" -1})

(def occurrence-ordinals-inv (cset/map-invert occurrence-ordinals))

(defn format-date-en-us
  [{:keys [y m d]}]
  (str (get month-names m) " " d ", " y))

(defn day-to-ordinal
  [n]
  (str n
       (cond (and (not (== n 11)) (== (mod n 10) 1)) "st"
             (and (not (== n 12)) (== (mod n 10) 2)) "nd"
             (and (not (== n 13)) (== (mod n 10) 3)) "rd"
             :else "th")))

(defn format-freq
  "Formats the recurrent period. The plural is formed by simply adding an 's'."
  [recur-pat]
  (let [n (name (:recur-type recur-pat))]
    (str "every "
         (if (== 1 (:freq recur-pat)) n (str (:freq recur-pat) " " n "s")))))

(defn format-occ
  [recur-pat]
  (cstr/join ", "
             (map #(get occurrence-ordinals-inv %) (sort (:occ recur-pat)))))

(defn format-month-recur-day-selection
  [recur-pat]
  (case (:day-selection recur-pat)
    :d (cstr/join ", " (map day-to-ordinal (sort (:d recur-pat))))
    :dow
      (str (format-occ recur-pat) " " (get full-day-names (:dow recur-pat)))))

(defn format-recur-pat
  [recur-pat]
  (let [start (if-let [start (:recur-start recur-pat)]
                (if (> (:daynum start) 0)
                  (str " from " (format-date-en-us start))
                  " since time immemorial")
                (case (:recur-type recur-pat)
                  :day " from today"
                  :week " from this week"
                  :month " from this month"
                  :year " from this year"))
        until (if-let [until (:recur-end recur-pat)]
                (str " until " (format-date-en-us until)))
        pat (case (:recur-type recur-pat)
              :day (format-freq recur-pat)
              :week (str "every " (if (== 1 (:freq recur-pat))
                                    "week"
                                    (str (:freq recur-pat) " weeks"))
                         " on " (cstr/join ", "
                                           (map #(get day-names %)
                                             (sort (:dow recur-pat)))))
              :month (str (format-freq recur-pat)
                          " on the "
                          (format-month-recur-day-selection recur-pat))
              :year (str (format-freq recur-pat)
                         " on "
                         (case (:day-selection recur-pat)
                           :md (str (get month-names (:m recur-pat))
                                    " "
                                    (:d recur-pat))
                           :occ-dow-month
                             (str "the " (format-occ recur-pat)
                                  " " (get full-day-names (:dow recur-pat))
                                  " of " (cstr/join
                                           ", "
                                           (map #(get full-month-names %)
                                             (sort (:m recur-pat))))))))]
    (str pat start until)))

(defn format-future-occurrences
  [recur-pat start until]
  (if (or (nil? start) (nil? until))
    ""
    (let [visible-occurrences-etc
            (take 4 (recurrent-event-occurrences recur-pat start until))
          displayed-occurrences (take 3 visible-occurrences-etc)
          ellipsis (== 4 (count visible-occurrences-etc))]
      (if (seq displayed-occurrences)
        (str " (occurring "
             (cstr/join "; " (map format-date-en-us displayed-occurrences))
             (if ellipsis "; \u2026")
             ")")))))

(defn format-recur-pat-single-period
  [recur-pat]
  (case (:recur-type recur-pat)
    :day (if-let [occ (rec-period-sole-occ (day-rec-to-period recur-pat))]
           (str "On " (format-date-en-us (day-num-to-date occ))))
    :week (let [recs (week-month-rec-to-periods recur-pat)]
            (if (== 1 (count recs))
              (if-let [occ (rec-period-sole-occ (first recs))]
                (str "During the week of " (format-date-en-us
                                             (week-num-day-to-date occ 0))
                     " on " (cstr/join ", "
                                       (map #(str (get day-names %)
                                                  " ("
                                                  (format-date-en-us
                                                    (week-num-day-to-date occ
                                                                          %))
                                                  ")")
                                         (sort (:dow recur-pat))))))))
    :month (let [recs (week-month-rec-to-periods recur-pat)]
             (if (== 1 (count recs))
               (if-let [occ (rec-period-sole-occ (first recs))]
                 (str "During the month of " (get month-names (mod occ 12))
                      " " (+ 1600 (quot occ 12))
                      " on the " (format-month-recur-day-selection
                                   recur-pat)))))
    nil))

(defn format-recur-pat-with-single-period-exception
  "Format a recur pat, except that when the recur pat refers to a single abstract
  recur period, use a different format."
  [recur-pat]
  (or (format-recur-pat-single-period recur-pat)
      (str "Repeating " (format-recur-pat recur-pat))))

(defn format-event
  [{:keys [evname specific-occs recur-pats]} start until]
  (str "an event named \"" evname
       "\"" (cstr/join ", "
                       (concat
                         (map #(str " on " (format-date-en-us %)) specific-occs)
                         (map #(str " repeating "
                                    (format-recur-pat %)
                                    (format-future-occurrences % start until))
                           recur-pats)))))

(defn fill-in-missing-fields-from-recur-pat
  "The user is allowed to omit the recur-start, recur-end, as well as
  for :week/:month/:year the selection of days. We default to characteristics
  from today."
  [res today-val]
  (let [res (transient res)
        res (if (contains? res :recur-start)
              res
              (assoc! res :recur-start today-val))
        start (:recur-start res)
        res (case (:recur-type res)
              :day res
              :week
                (if (contains? res :dow) res (assoc! res :dow #{(:dow start)}))
              :month (if (contains? res :day-selection)
                       res
                       (-> res
                           (assoc! :day-selection :d)
                           (assoc! :d #{(:d start)})))
              :year (if (contains? res :day-selection)
                      res
                      (-> res
                          (assoc! :day-selection :md)
                          (assoc! :d (:d start))
                          (assoc! :m (:m start)))))]
    (persistent! res)))

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
  (let [regex (js/RegExp. (str "^("
                               (cstr/join "|"
                                          (map #(if-let [pat (:str-glob-fun %)]
                                                  (simplified-glob-to-regex pat)
                                                  (gstr/regExpEscape %))
                                            str-exprs))
                               ")$")
                          "u")]
    (into #{} (filter #(.test regex %) event-names))))

(defn align-sorted-seqs
  [seq1 seq2]
  (loop [s1 seq1
         s2 seq2
         result (transient [])]
    (cond (and (empty? s1) (empty? s2)) (persistent! result)
          (empty? s1) (recur s1 (next s2) (conj! result [nil (first s2)]))
          (empty? s2) (recur (next s1) s2 (conj! result [(first s1) nil]))
          (= (first s1) (first s2))
            (recur (next s1) (next s2) (conj! result [(first s1) (first s2)]))
          (< (first s1) (first s2))
            (recur (next s1) s2 (conj! result [(first s1) nil]))
          :else (recur s1 (next s2) (conj! result [nil (first s2)])))))


;; -------------------------
;; Command line history and search

(def history-limit 500)

;; This section implements the following history features: (a) when a command
;; is
;; executed with no parse errors, it is added to the history; (b) the user can
;; search the history entries backwards and forwards using the arrow key. The
;; search is inspired by the fish shell. It has complex semantics:
;;
;; When the user is typing a command input and it is non-empty, it is
;; considered
;; as a prefix with which to search the history. Every additional character
;; typed by the user further restricts the search.
;;
;; The search has a state boolean indicating whether it is committed or not. If
;; the user has not committed to a search, the search proceeds backwards
;; chronologically and displays the first (i.e. most recent) match.
;;
;; If the user has committed to a search, then a pointer points to an entry in
;; the history.

(def history-initial-state
  {:hist-entries [], :hist-cur-idx nil, :hist-cur-prefix nil})

(defn history-add
  [ui-state entry]
  (-> ui-state
      (update :hist-entries
              (fn [v]
                (if (identical? (peek v) entry)
                  v
                  (let [new (conj v entry)
                        new-count (count new)]
                    ;; If we only do subvec, the problem is that it retains
                    ;; a reference of the original vector and further calls
                    ;; to conj will simply add more contents to the
                    ;; original vector, leading to a classic memory leak.
                    (if (>= new-count (* 2 history-limit))
                      (into [] (subvec new (- new-count history-limit)))
                      new)))))
      (assoc :hist-cur-prefix nil)
      (assoc :hist-cur-idx nil)))

(defn history-is-search-uncommitted?
  "Returns whether the search is uncommitted, i.e. no user action yet."
  [{:keys [hist-cur-idx hist-cur-prefix]}]
  {:pre [(= (nil? hist-cur-idx) (nil? hist-cur-prefix))]}
  (nil? hist-cur-idx))

(defn history-search-find-next-index
  [hist-entries hist-cur-idx prefix backward?]
  (let [effective-start (max 0 (- (count hist-entries) history-limit))
        remaining-entries (if (nil? hist-cur-idx)
                            (subvec hist-entries effective-start)
                            (if backward?
                              (subvec hist-entries effective-start hist-cur-idx)
                              (subvec hist-entries (inc hist-cur-idx))))
        entries-seq
          (if backward? (rseq remaining-entries) (seq remaining-entries))
        matching-indices
          (keep-indexed (fn [idx entry]
                          (if (.startsWith entry prefix)
                            (if backward?
                              (- (or hist-cur-idx (count hist-entries)) 1 idx)
                              (+ (or hist-cur-idx -1) 1 idx))))
                        entries-seq)
        new-idx (first matching-indices)
        adjusted-idx (or new-idx (if backward? hist-cur-idx))]
    adjusted-idx))

(defn history-search
  "Searches the history record for a prefix match and saves it in the hist-cur-idx
  field.

  We represent the search state as follows:

  The search state may be committed or uncommitted. When it is uncommitted,
  hist-cur-idx and hist-cur-prefix are both nil. Otherwise they are both
  non-nil.

  When uncommitted, the search can only be backwards. It returns the most recent
  prefix match.

  When committed, the search must be of the same prefix as the previous search.
  The search may be backwards or forwards. If backwards and it exhausts the
  search space, the hist-cur-idx stays the same. If forwards and it exhausts the
  search space, the search becomes uncommitted.
  "
  [{:keys [hist-entries hist-cur-idx hist-cur-prefix], :as ui-state} prefix
   backward?]
  (if (history-is-search-uncommitted? ui-state)
    (assert
      backward?
      "Cannot execute a forward search when the user has not committed to searching.")
    (assert (= prefix hist-cur-prefix)
            "When committed the search must be of the same prefix as before."))
  (let [adjusted-idx (history-search-find-next-index hist-entries
                                                     hist-cur-idx
                                                     prefix
                                                     backward?)
        new-prefix (if (nil? adjusted-idx) nil prefix)]
    (-> ui-state
        (assoc :hist-cur-prefix new-prefix)
        (assoc :hist-cur-idx adjusted-idx))))

(defn history-current
  "Returns the currently active history entry."
  [{:keys [hist-entries hist-cur-idx], :as ui-state}]
  (get hist-entries hist-cur-idx nil))

(defn history-current-set-input
  [ui-state fallback]
  (assoc ui-state :cmdline-input (or (history-current ui-state) fallback)))

(defn history-search-current-completion
  "Calculates the current history-based completion."
  [{:keys [cmdline-input hist-entries], :as ui-state}]
  (get hist-entries
       (if (and (history-is-search-uncommitted? ui-state) (seq cmdline-input))
         (history-search-find-next-index hist-entries nil cmdline-input true))))

(defn history-search-navigate
  [{:keys [cmdline-input hist-cur-prefix], :as ui-state} backward?]
  (if (history-is-search-uncommitted? ui-state)
    (if backward?
      ;; Uncommitted state + backwards search => committed state
      (-> ui-state
          (history-search cmdline-input true)
          (history-current-set-input cmdline-input))
      ;; The user cannot execute a forward search. Ignore this.
      ui-state)
    ;; Committed search using the current prefix.
    (-> ui-state
        (history-search hist-cur-prefix backward?)
        (history-current-set-input hist-cur-prefix))))

(defn history-search-navigate-up
  [ui-state]
  ;; If not committed and not empty, the user would have seen the most
  ;; recent completion, so we navigate up twice. Otherwise, once.
  (if (and (seq (:cmdline-input ui-state))
           (history-is-search-uncommitted? ui-state))
    (-> ui-state
        (history-search-navigate true)
        (history-search-navigate true))
    (history-search-navigate ui-state true)))

(defn history-search-navigate-down
  [ui-state]
  (history-search-navigate ui-state false))

(defn commit-current-completion
  [{:keys [cmdline-input], :as ui-state} current-completion]
  ;; Disallows empty input, and sets the command
  (if (empty? cmdline-input)
    ui-state
    (if-let [comp current-completion]
      (assoc ui-state :cmdline-input comp)
      ui-state)))

(defn history-search-navigate-finish
  [ui-state]
  (-> ui-state
      (assoc :hist-cur-idx nil)
      (assoc :hist-cur-prefix nil)))

(sg/reg-event :app :cmdline-arrow-up #(update % :ui history-search-navigate-up))
(sg/reg-event :app
              :cmdline-arrow-down
              #(update % :ui history-search-navigate-down))
(sg/reg-event
  :app
  :cmdline-arrow-right
  #(update %1 :ui commit-current-completion (:current-completion %2)))
(sg/reg-event :app
              :cmdline-history-finish
              #(update % :ui history-search-navigate-finish))

;; -------------------------
;; State

(defonce rt-ref (sg/get-runtime :app))

(defn init-ui-state!
  []
  (sg/add-kv-table rt-ref
                   :ui
                   {}
                   (merge history-initial-state
                          {:weeks-to-show 15,
                           :start-date (today),
                           :optimize-events true,
                           :autocomplete true,
                           :explain-cmd true,
                           :alt-color true,
                           :cmdline-input "",
                           :cmdline-output
                             "Welcome to smartcal. Type \"help\" for help.",
                           ;; This defines what kind of content the modal
                           ;; is showing. It may be nil, :help, :ls-all,
                           ;; :ls-visible etc.
                           :modal-content nil,
                           ;; When we decide to stop showing the modal,
                           ;; instead of resetting the modal-content to
                           ;; nil, we change this instead to preserve the
                           ;; original modal contents. This is nicer since
                           ;; we have an animation
                           ;; (technically a CSS transition) to dismiss the
                           ;; modal. The user won't have to see a flash of
                           ;; the empty modal.
                           :modal-shown false})))

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

(sg/reg-event :app
              :modify-ui-prefs
              (fn [env {:keys [key value]}]
                {:pre [(keyword? key)]}
                (when VERBOSE
                  (js/console.log "Changing setting" (name key) "to" value))
                (assoc-in env [:ui key] value)))

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
         (do (dev-log "Reloading from localStorage failed" e) {}))))

(defn show-modal
  [env content]
  (-> env
      (assoc-in [:ui :modal-shown] true)
      (assoc-in [:ui :modal-content] content)))
(sg/reg-event :app
              :show-modal
              (fn [env {:keys [content]}] (show-modal env content)))


(defn hide-modal [env _] (assoc-in env [:ui :modal-shown] false))
(sg/reg-event :app :hide-modal hide-modal)

(defn show-message [env msg] (assoc-in env [:ui :cmdline-output] msg))
(sg/reg-event :app
              :show-message
              (fn [env {:keys [msg]}] (show-message env msg)))

(defn us-bank-holidays
  []
  (map (fn [[name recur-pat]]
         (let [new-recur-pat (-> recur-pat
                                 (assoc :recur-start epoch)
                                 (assoc :recur-type :year)
                                 (assoc :freq 1))]
           (event-from-single-rec name new-recur-pat)))
    {"New Year's Day" {:day-selection :md, :m 0, :d 1},
     "Martin Luther King Jr. Day"
       {:day-selection :occ-dow-month, :occ #{2}, :dow 1, :m #{0}},
     "Presidents' Day"
       {:day-selection :occ-dow-month, :occ #{2}, :dow 1, :m #{1}},
     "Memorial Day"
       {:day-selection :occ-dow-month, :occ #{-1}, :dow 1, :m #{4}},
     "Juneteenth" {:day-selection :md, :m 5, :d 19},
     "Independence Day" {:day-selection :md, :m 6, :d 4},
     "Labor Day" {:day-selection :occ-dow-month, :occ #{0}, :dow 1, :m #{8}},
     "Columbus Day" {:day-selection :occ-dow-month, :occ #{1}, :dow 1, :m #{9}},
     "Veterans Day" {:day-selection :md, :m 10, :d 11},
     "Thanksgiving Day"
       {:day-selection :occ-dow-month, :occ #{3}, :dow 4, :m #{10}},
     "Christmas Day" {:day-selection :md, :m 11, :d 25}}))

(defn init-events-state!
  []
  (sg/add-kv-table rt-ref
                   :events
                   {:primary-key :evname}
                   (into {} (map #(vector (:evname %) %) (us-bank-holidays)))))

(defn add-event-to-env
  [env event]
  {:pre [(event? event)]}
  (if (and (empty? (:specific-occs event)) (empty? (:recur-pats event)))
    (show-message env "The event will never occur, therefore it is not added.")
    (kv/update-val env
                   :events
                   (:evname event)
                   merge-and-possibly-optimize-event
                   event
                   (get-in env [:ui :optimize-events]))))

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
   ls-only = <ws 'only' ws> str-expr-plus
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
     | recur-week-freq (<ws 'on' ws> dow-lit-plus)?
   recur-week-freq = <'every'> ((<ws> int-lit <ws> | <ws? '(' ws?> int-expr <ws? ')' ws?>) <'weeks'> | <ws 'week'>)
   recur-month
    = recur-month-freq (<ws 'on' ws ('the' ws)?> recur-month-type)?
    | <'every' ws> recur-month-type <ws 'of' ws ('the' | 'each') ws 'month'>
   <recur-month-type> = recur-month-by-d | recur-month-by-dow
   recur-month-by-d = d-lit-plus
   recur-month-by-dow = occurrence-ordinal-plus <ws> dow-lit
   recur-month-freq = <'every'> ((<ws> int-lit <ws> | <ws? '(' ws?> int-expr <ws? ')' ws?>) <'months'> | <ws 'month'>)
   recur-year = recur-year-freq (<ws 'on' ws> recur-year-type)?
   <recur-year-type> = recur-year-by-md | recur-year-by-occ-dow-month
   recur-year-by-md = md-lit
   recur-year-by-occ-dow-month = <('the' ws)?> occurrence-ordinal-plus <ws> dow-lit <ws 'of' ws> month-lit-plus
   recur-year-freq = <'every'> ((<ws> int-lit <ws> | <ws? '(' ws?> int-expr <ws? ')' ws?>) <'years'> | <ws 'year'>)
   recur-start = <ws 'from' ws> date-expr
   recur-end = <ws 'until' ws> date-expr
   <date-expr> = date-funs | date-lit
   <date-funs> = date-plus-fun | date-minus-fun | date-today-fun
   date-plus-fun = <'plus' ws? '(' ws?> date-expr <ws? ',' ws?> interval-expr <ws? ')'>
   date-minus-fun = <'minus' ws? '(' ws?> date-expr <ws? ',' ws?> interval-expr <ws? ')'>
   date-today-fun = <'today' ws? '(' ws? ')'>
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
   <str-expr-plus> = str-expr ((<ws? ',' ws?> str-expr)* | (<ws> str-expr)*)
  ")

(defn transform-parsed-with-specific-today
  "This function transforms the parse tree generated by instaparse to make it
  nicer. The bulk of the date processing and recurrence pattern processing
  happens here. To make this function testable, the value for the today()
  function is also supplied here."
  [parsed today-val]
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
     :date-today-fun (fn [] today-val),
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
     :str-glob-fun (fn [pat] {:str-glob-fun pat}),
     :recurring (fn [b & a]
                  {:recurring (fill-in-missing-fields-from-recur-pat
                                (into b a)
                                today-val)}),
     :single-occ (fn [d] {:single-occ d}),
     :add-cmd (fn [name date-spec]
                [:add-cmd
                 (if-let [occ (:single-occ date-spec)]
                   (event-from-single-occ name occ)
                   (if-let [pat (:recurring date-spec)]
                     (event-from-single-rec name pat)
                     (throw (js/Error "unexpected add-cmd structure"))))])}
    parsed))

(defn transform-parsed
  "This function transforms the parse tree generated by instaparse to make it
  nicer. References to today() are evaluated with the actual today's date."
  [parsed]
  (transform-parsed-with-specific-today parsed (today)))

(defn common-prefix
  [s1 s2]
  {:pre [(string? s1) (string? s2)]}
  (let [maxlen (min (.-length s1) (.-length s2))]
    (loop [i 0]
      (if (== (.charAt s1 i) (.charAt s2 i))
        (if (== i maxlen) (.substring s1 0 i) (recur (inc i)))
        (.substring s1 0 i)))))

(defn common-prefixes
  "Returns the common prefix of a seq of strings. If the seq is empty, returns
  nil. Otherwise returns a string. If there is no common prefix, returns the
  empty string. (Callers usually do not distinguish between nil or empty string,
  and they can use empty? to do that.)"
  [str-seq]
  {:pre [(every? string? str-seq)]}
  (loop [cur-prefix nil
         ss str-seq]
    (cond (empty? ss) cur-prefix
          (nil? cur-prefix) (recur (first ss) (rest ss))
          :else (let [new-common-prefix (common-prefix cur-prefix (first ss))]
                  (if (== 0 (.-length new-common-prefix))
                    new-common-prefix
                    (recur new-common-prefix (rest ss)))))))

(defn find-parser-based-completion
  [total-parsed]
  (if (insta/failure? total-parsed)
    (let [{:keys [text index reason], :as failure} (insta/get-failure
                                                     total-parsed)
          failed-text (.substring text index)
          expected-matching-token
            (keep #(if-let [e (:expecting %)]
                     (case (:tag %)
                       :string (if (.startsWith e failed-text)
                                 (.substring e (.-length failed-text)))
                       :regexp (if (and (identical? "" failed-text)
                                        (identical? "/^ +/" (.toString e))
                                        (not (.endsWith text " ")))
                                 " ")
                       nil))
                  reason)
          suggested-suffix (common-prefixes expected-matching-token)]
      (if (empty? suggested-suffix) nil (str text suggested-suffix)))))

;; -------------------------
;; Components

(defc day-component
  [{:keys [y m d], :as ymd} ^boolean should-alt-color ^boolean show-complete
   events-on-this-date]
  (bind events-sorted
        (sort-by :evname gstr/intAwareCompare events-on-this-date))
  (render
    (<<
      [:div.td {:class (if (and should-alt-color (== 0 (mod m 2))) "alt")}
       [:p.day
        (if show-complete
          (format-date-en-us ymd)
          (str (if (== 1 d) (str (get month-names m) " "))
               d
               (if (and (== 1 d) (== 0 m)) (str ", " y))))]
       [:ul.events
        (sg/keyed-seq events-sorted
                      :evname
                      (fn [ev]
                        {:pre [(event? ev)]}
                        (<< [:li
                             {:role "button",
                              :title (:evname ev),
                              :on-click {:e :event-click, :name (:evname ev)}}
                             (:evname ev)])))]]))
  (event :event-click
         [env {:keys [name]} e]
         (do (.stopPropagation e)
             (sg/run-tx env {:e :show-modal, :content [:ls-only #{name}]}))))

(def cmdline-prompt ">>> ")

(def cmdline-prompt-length (.-length cmdline-prompt))

(defn remove-subsequence
  [val needle]
  (apply str
    (persistent! (loop [fragments (transient [val])
                        i 0]
                   (if (< i (.-length needle))
                     (let [this (get fragments i)
                           ch (.charAt needle i)
                           index (.indexOf this ch)]
                       (if (== index -1)
                         fragments
                         (let [this-before (.substr this 0 index)
                               this-after (.substr this (inc index))
                               new-fragments (-> fragments
                                                 (pop!)
                                                 (conj! this-before)
                                                 (conj! this-after))]
                           (recur new-fragments (inc i)))))
                     fragments)))))

(defc cmdline-display-component
  [[production-kw & remaining]]
  (render (<< [:span {:class (name production-kw)}
               (sg/simple-seq remaining
                              #(cond (string? %) (<< [:span %])
                                     (vector? %) (cmdline-display-component
                                                   %)))])))

(defc help-modal-component
  []
  (render
    (<<
      [:div#help
       [:p
        "This is a smart calendar app that runs completely in your browser. It is controlled by typing commands into the command area at the bottom. The command area supports completion, history search, and a preview. Use the right arrow to accept the completion. Use the up or down arrow to recall the command history."]
       [:details {:open true} [:summary "Navigating the calendar"]
        [:p "Type " [:code "next"] " or " [:code "prev"]
         " to move the calendar view forward or backward by one week. When followed with an integer, the calendar view is moved by that many weeks. For example "
         [:code "next 7"] " moves the calendar forward by 7 weeks."]
        [:p "To go to a specific date, use the " [:code "goto"]
         " command, followed by a date literal. There are many ways you can specify a date literal. So all of these work: "
         [:code "goto 20241001"] ", " [:code "goto 2024-10-01"] ", "
         [:code "goto Oct 1, 2024"] ", " [:code "goto 01 Oct 2024"]
         ". However you cannot specify the month as a number unless the year is specified first. This is because some put the day before the month, and some after, so a date like 10/01/2024 is inherently ambiguous."]]
       [:details {:open true} [:summary "Adding events"]
        [:p "The " [:code "add"]
         " command is used to add new events. An event always has a name. Adding an event with the same name as an existing event merges the new and existing events. An event may contain specific dates of occurrence, or be a recurring event."]
        [:p "A single event has its occurrence date specified using the "
         [:code "on"] " keyword. So "
         [:code "add \"Celebrate Jack's 60th birthday\" on 20241018"]
         " creates a single event with that name and on that date."]
        [:p "A recurring event has its recurrence pattern specified using the "
         [:code "every"]
         " keyword, followed by the recurrence period, which may be specified in units of days, weeks, months, or years."]
        [:p [:em "Day-based recurrence. "] "The command "
         [:code "add \"Daily reflection\" every day"]
         " is an example. The command "
         [:code "add \"Take out the trash\" every 3 days"]
         " is another example."]
        [:p [:em "Week-based recurrence. "]
         "Week-based recurrences specify the period of recurrence in units of months, as well as the days within each chosen week. The command "
         [:code "add \"TGIF\" every week on Fri"]
         " is an example. There may be multiple days specified, so "
         [:code "add \"Go to the gym\" every week on Mon, Fri"]
         " creates an event that repeats on Monday and Friday. (The repeated days may be separated by commas or simply whitespace, so "
         [:code "add \"Go to the gym\" every week on Mon Fri"]
         " also works, but the two styles cannot be mixed.) The command "
         [:code "add \"Get payslips\" every 2 weeks on Fri"]
         " creates an event every other week on Friday; by default it starts this week so if today is a Saturday, the current week occurrence will be omitted and the first occurrence will be two weeks later. If this is not what you want, you can specify a start date explicitly."]
        [:p [:em "Month-based recurrence. "]
         "Month-based recurrences specify the period of recurrence in units of months, as well as the selection of a day within a month. The command "
         [:code "add \"Pay credit card\" every month on 28"]
         " sets the recurrence period to be one month, and it specifies the 28th day of each selected month. The command "
         [:code
          "add \"Review personal finances\" every 2 months on the first Saturday"]
         " sets the recurrence period to be two months, and in the first month of each period, specifies the first Saturday."]
        [:p [:em "Year-based recurrence. "]
         "Year-based recurrences similarly specify the period of recurrence in units of years, as well as the selection of a day within a year. The command "
         [:code "add \"Celebrate Dad's birthday\" every year on Apr 30"]
         " is an example. The command "
         [:code
          "add \"Pay property tax\" every year on the first Monday of Apr, Dec"]
         " is another example."]
        [:p [:em "Recurrence start and end. "]
         "Recurrences may be specified with a definite start date and an end date. For example, the command "
         [:code
          "add \"Prepare for the presidential election\" every 4 years on the first Monday of November from Nov 1, 2028 until Jan 1, 2080"]
         " specifies a start date and an end date for the recurrence. If the start date is not specified, it defaults to today. If the end date is not specified, the recurrence will continue indefinitely."]]
       [:details {:open true} [:summary "Listing and inspecting events"]
        [:p "The " [:code "ls"]
         " command can be used to list added events and inspect them. The output is shown graphically, containing the name of the event, whether it is recurrent or not, and if so, the recurrence pattern and the next occurrences visible. There is also a minigrid that highlights the occurrences."]
        [:p
         "Just like the POSIX ls tool, by default it hides any events not visible in the calendar. If you wish to look at all events including hidden ones, use "
         [:code "ls all"]
         " instead. You can also verbosely specify the default behavior with "
         [:code "ls visible"] "."]
        [:p "Globs can be specified. The " [:strong "?"]
         " symbol represents any character; the " [:strong "*"]
         " symbol represents zero or more characters. (Because discussions of characters are fraught with subtlety, here character simply refers to a Unicode code point.) So "
         [:code "ls only glob(\"*Day\") \"X\""] " displays events ending with "
         [:em "Day"] " and also those named " [:em "X"] "."]]])))

(defc ls-modal-component
  [show-invisible selected-or-nil]
  (bind weeks-to-show (sg/kv-lookup :ui :weeks-to-show))
  (bind start (actual-start (sg/kv-lookup :ui :start-date)))
  (bind until (day-num-to-date (+ (* 7 weeks-to-show) (:daynum start))))
  (bind selected-events
        (sg/query
          (fn [{:keys [events], :as env}]
            (sort-by
              :evname
              gstr/intAwareCompare
              (let [data (vals (if (nil? selected-or-nil)
                                 events
                                 (select-keys events selected-or-nil)))
                    xf (comp (map #(assoc %
                                     :visible-occurrences
                                       (event-to-dates start until %)))
                             (map #(assoc %
                                     :visible-occurrences-daynums
                                       (map :daynum (:visible-occurrences %))))
                             (if show-invisible
                               identity
                               (filter #(seq (:visible-occurrences %)))))]
                (sequence xf data))))))
  (render
    (<<
      [:div#ls-grid
       (sg/keyed-seq
         selected-events
         :evname
         (fn [{:keys [visible-occurrences visible-occurrences-daynums
                      specific-occs recur-pats],
               :as ev}]
           {:pre [(event? ev) (vector? visible-occurrences)
                  (seq? visible-occurrences-daynums)]}
           (<<
             [:div.ls-minigrid
              (sg/simple-seq
                (align-sorted-seqs (range (:daynum start)
                                          (+ (:daynum start)
                                             (* 7 weeks-to-show)))
                                   visible-occurrences-daynums)
                (fn [[this-daynum event-daynum]]
                  (<< [:div.ls-minigrid-day
                       {:class (if (nil? event-daynum) "absent" "present")}])))]
             [:div.ls-desc [:h4 (:evname ev)]
              (let [cnt (+ (count specific-occs) (count recur-pats))]
                (if (== cnt 1)
                  (<< [:p
                       (if-let [date (first specific-occs)]
                         (str "On " (format-date-en-us date)))
                       (if-let [pat (first recur-pats)]
                         (format-recur-pat-with-single-period-exception pat))])
                  (<< [:details {:open true} [:summary (str cnt " rules")]
                       [:ul
                        (sg/simple-seq
                          (seq specific-occs)
                          (fn [date] (<< [:li "On " (format-date-en-us date)])))
                        (sg/simple-seq
                          recur-pats
                          (fn [pat]
                            (<< [:li
                                 (format-recur-pat-with-single-period-exception
                                   pat)])))]])))
              (let [cnt (count visible-occurrences)]
                (if (== cnt 0)
                  (<< [:p "No occurrences shown"])
                  (<<
                    [:details
                     [:summary cnt
                      (if (== 1 cnt) " occurrence shown" " occurrences shown")]
                     [:ul
                      (sg/simple-seq
                        visible-occurrences
                        (fn [occ]
                          (<< [:li (format-date-en-us occ)])))]])))])))])))

(defc config-modal-component
  []
  (bind weeks-to-show (sg/kv-lookup :ui :weeks-to-show))
  (bind should-autocomplete (sg/kv-lookup :ui :autocomplete))
  (bind should-opt (sg/kv-lookup :ui :optimize-events))
  (bind should-explain (sg/kv-lookup :ui :explain-cmd))
  (bind should-alt-color (sg/kv-lookup :ui :alt-color))
  (render
    (<<
      [:div.slider-control
       [:label {:for "weeks-to-display"} "Weeks to display: "]
       [:input
        {:type "range",
         :id "weeks-to-display",
         :value weeks-to-show,
         :min 1,
         :max 60,
         :on-change :modify-weeks-to-show}] [:p weeks-to-show]]
      [:div.checkbox-control
       [:input#alt-month
        {:type "checkbox",
         :checked should-alt-color,
         :on-click :modify-alt-color}]
       [:label {:for "alt-month"} "Alternate colors every other month"]]
      [:div.checkbox-control
       [:input#opt
        {:type "checkbox", :checked should-opt, :on-click :modify-optimization}]
       [:label {:for "opt"} "Optimize events (experimental)"]
       [:p.expl
        "Attempt to optimize recurrence rules to make them easier to understand when adding or modifying events. For example, if an event has a rule to repeat every 2 days, and another rule to repeat every 2 days starting from the following day, these two rules will be combined to be repeating every day."]]
      [:div.checkbox-control
       [:input#autocom
        {:type "checkbox",
         :checked should-autocomplete,
         :on-click :modify-autocomplete}]
       [:label {:for "autocom"} "Show auto-complete suggestions"]
       [:p.expl
        "Show suggested completions based on history and syntax (use the right arrow key to accept the suggestion)."]]
      [:div.checkbox-control
       [:input#explain
        {:type "checkbox", :checked should-explain, :on-click :modify-explain}]
       [:label {:for "explain"} "Show explanation of commands if well-formed"]
       [:p.expl
        "Explain the effect of a command when the command can be parsed correctly."]]))
  (event :modify-weeks-to-show
         [env _ e]
         (sg/run-tx env
                    {:e :modify-ui-prefs,
                     :key :weeks-to-show,
                     :value (js/parseInt (.. e -target -value) 10)}))
  (event :modify-alt-color
         [env _ e]
         (sg/run-tx env
                    {:e :modify-ui-prefs,
                     :key :alt-color,
                     :value (.. e -target -checked)}))
  (event :modify-autocomplete
         [env _ e]
         (sg/run-tx env
                    {:e :modify-ui-prefs,
                     :key :autocomplete,
                     :value (.. e -target -checked)}))
  (event :modify-optimization
         [env _ e]
         (sg/run-tx env
                    {:e :modify-ui-prefs,
                     :key :optimize-events,
                     :value (.. e -target -checked)}))
  (event :modify-explain
         [env _ e]
         (sg/run-tx env
                    {:e :modify-ui-prefs,
                     :key :explain-cmd,
                     :value (.. e -target -checked)})))

(defn execute-input-impl
  [env input]
  (try
    (if (empty? (.trim input))
      env
      (let [parsed (transform-parsed (cmdline-parser input))]
        (when VERBOSE (js/console.log "Currently parsed:" (pr-str parsed)))
        (if (insta/failure? parsed)
          (show-message env (pr-str parsed))
          (match (fnext parsed)
            [:next-cmd] (update-in env [:ui :start-date] next-week)
            [:next-cmd n] (update-in env [:ui :start-date] #(next-week n %))
            [:prev-cmd] (update-in env [:ui :start-date] prev-week)
            [:prev-cmd n] (update-in env [:ui :start-date] #(prev-week n %))
            [:goto-cmd ymd] (assoc-in env [:ui :start-date] ymd)
            [:add-cmd event] (add-event-to-env env event)
            [:rm-cmd name] (let [old-count (count (:events env))
                                 new-env (update env :events dissoc name)
                                 new-count (count (:events new-env))
                                 removals (- old-count new-count)]
                             (show-message new-env
                                           (str "Removed "
                                                (if (== 1 removals)
                                                  "one event."
                                                  (str removals " events.")))))
            [:help-cmd] (show-modal env :help)
            [:config-cmd] (show-modal env :config)
            [:ls-cmd] (show-modal env :ls-visible)
            [:ls-cmd [t]] (show-modal env t)
            [:ls-cmd [:ls-only & exprs]]
              (let [selected-events
                      (eval-str-exprs exprs (map :evname (vals (:events env))))]
                (if (empty? selected-events)
                  (show-message
                    env
                    "No events selected for display in \"ls only\" command.")
                  (show-modal env [:ls-only selected-events])))
            :else (show-message env (str "TODO: " (pr-str parsed)))))))
    (catch :default e
      (show-message env
                    (if (and (map? e) (:date-outside-range e))
                      "The specified date is outside the supported range."
                      (do (js/console.log e) "An unknown error occurred."))))))

(defn execute-input
  [env {:keys [input]}]
  (-> env
      (assoc-in [:ui :cmdline-output] "")
      (assoc-in [:ui :cmdline-input] "")
      (hide-modal nil)
      (update :ui history-add input)
      (execute-input-impl input)))
(sg/reg-event :app :execute-input execute-input)

(defc explain-input-component
  [input start until]
  (bind parsed (transform-parsed (cmdline-parser input)))
  (render
    (if-not (insta/failure? parsed)
      (match (fnext parsed)
        [:next-cmd] "Scroll down by one week"
        [:next-cmd n] (str "Scroll down by " n " weeks")
        [:prev-cmd] "Scroll up by one week"
        [:prev-cmd n] (str "Scroll up by " n " weeks")
        [:goto-cmd date] (str "Go to date " (format-date-en-us date))
        [:add-cmd event] (str "Add " (format-event event start until))
        [:rm-cmd name] (str "Remove events named \"" name "\"")
        [:ls-cmd] "List events visible in the current view"
        [:ls-cmd [:ls-all]] "List all events"
        [:ls-cmd [:ls-visible]] "List events visible in the current view"
        [:ls-cmd [:ls-only & exprs]] (str "List events that are "
                                          (format-str-exprs exprs))
        [:help-cmd] "Show help"
        [:config-cmd] "Configure calendar display"
        :else (pr-str parsed)))))

(defn cmdline-input-changed
  [env {:keys [processed-input]}]
  (assoc-in env [:ui :cmdline-input] processed-input))
(sg/reg-event :app :cmdline-input-changed cmdline-input-changed)

(defc cmdline-component
  [start until]
  (bind input (sg/kv-lookup :ui :cmdline-input))
  (bind ^boolean should-autocomplete (sg/kv-lookup :ui :autocomplete))
  (bind ^boolean should-explain (sg/kv-lookup :ui :explain-cmd))
  (bind parsed (cmdline-parser input :total true :unhide :all))
  (bind possible-completion
        (if should-autocomplete
          (if (insta/failure? parsed)
            (or (sg/query #(history-search-current-completion (:ui %)))
                (find-parser-based-completion parsed)))))
  (render
    (<<
      [:div#cmdline
       [:textarea#cmdline-in.cmdline
        {:spell-check "false",
         :autofocus true,
         :value (str cmdline-prompt input),
         :on-input {:e :textarea-change},
         ;; React synthesizes select event for many other event types. See
         ;; https://github.com/facebook/react/blob/7c8e5e7ab8bb63de911637892392c5efd8ce1d0f/packages/react-dom-bindings/src/events/plugins/SelectEventPlugin.js#L150
         :on-select {:e :textarea-select},
         :on-focusin {:e :textarea-select},
         :on-mouseup {:e :textarea-select},
         :on-dragend {:e :textarea-select},
         :on-keydown {:e :textarea-keydown},
         :on-keyup {:e :textarea-select}}]
       [:pre#cmdline-disp.cmdline
        {:aria-hidden "true",
         :class (if (identical? "" input)
                  ""
                  (if (insta/failure? parsed) "failed" "succeeded"))}
        [:code cmdline-prompt
         (if (seq parsed) (cmdline-display-component parsed))
         (if-not (identical? "" input)
           (<< [:span.comment
                (if (insta/failure? parsed)
                  (if (nil? possible-completion)
                    (if should-explain " # Parse Error")
                    (.substring possible-completion (.-length input)))
                  (if should-explain
                    (<< " # "
                        (explain-input-component input start until))))]))]]]))
  (event :textarea-change
         [env _ e]
         ;; We do not support tab characters for now. The
         ;; browser is supposed to interpret the tab character
         ;; as focusing on the next input and should not
         ;; result in any real tab characters.
         (let [val (-> e
                       .-target
                       .-value
                       (.replaceAll "\t" ""))]
           ;; The handling of the prompt is somewhat ad-hoc
           ;; and arbitrary. Basically the <textarea> element
           ;; doesn't have a way to restrict editing to some
           ;; portion of it. So the prompt is included.
           (if
             ;; The user keeps the prompt and appends to it. Happy case.
             (.startsWith val cmdline-prompt)
             (let [input (.substring val cmdline-prompt-length)]
               (if (> (.indexOf input "\n") -1)
                 ;; In Grove, controlled components behave differently from
                 ;; React. We need to manually set the DOM value. This is
                 ;; because the DOM value is changed by the user and yet we
                 ;; want it to stay the same. Since the framework sees that
                 ;; the value is the same as before, it will not set the
                 ;; actual DOM value again. See also a similar issue in
                 ;; Preact:
                 ;; Https://www.jovidecroock.com/blog/controlled-inputs.
                 ;; This is why we need manual set!.
                 (do (set! (.. e -target -value) cmdline-prompt)
                     (sg/run-tx env
                                {:e :execute-input,
                                 :input (.replaceAll input "\n" "")}))
                 (do (set! (.. e -target -value) (str cmdline-prompt input))
                     (sg/run-tx env
                                {:e :cmdline-input-changed,
                                 :processed-input input}))))
             ;; The user is a bit mischievous.
             (let [input (-> val
                             (remove-subsequence cmdline-prompt)
                             (.replaceAll "\n" ""))]
               (set! (.. e -target -value) (str cmdline-prompt input))
               (sg/run-tx env
                          {:e :cmdline-input-changed,
                           :processed-input input})))))
  (event :textarea-select
         [env _ e]
         (let [start (-> e
                         .-target
                         .-selectionStart)
               end (-> e
                       .-target
                       .-selectionEnd)
               correct-start (max start cmdline-prompt-length)
               correct-end (max end cmdline-prompt-length)]
           (when-not (and (== start correct-start) (== end correct-end))
             (.setSelectionRange (.-target e) correct-start correct-end)))
         env)
  (event :textarea-keydown
         [env _ e]
         (when-not (let [^boolean composing (.-isComposing e)]
                     (or composing (== 229 (.-keyCode e))))
           (case (.-code e)
             "ArrowUp" (do (.preventDefault e)
                           (.stopPropagation e)
                           (sg/run-tx env {:e :cmdline-arrow-up}))
             "ArrowDown" (do (.preventDefault e)
                             (.stopPropagation e)
                             (sg/run-tx env {:e :cmdline-arrow-down}))
             "ArrowRight" (when (== (-> e
                                        .-target
                                        .-selectionStart)
                                    (-> e
                                        .-target
                                        .-selectionEnd)
                                    (+ cmdline-prompt-length (.-length input)))
                            (.preventDefault e)
                            (.stopPropagation e)
                            (sg/run-tx env
                                       {:e :cmdline-arrow-right,
                                        :current-completion
                                          possible-completion}))
             "ArrowLeft" nil
             (sg/run-tx env {:e :cmdline-history-finish})))))

(defc cmdline-output-component
  []
  (bind o (sg/kv-lookup :ui :cmdline-output))
  (render (<< [:div#cmdline-out
               (if (seq o) (<< [:pre [:code (.trimEnd o)]]))])))

(defc modal-component
  []
  (bind modal-content (sg/kv-lookup :ui :modal-content))
  (bind modal-shown (sg/kv-lookup :ui :modal-shown))
  (bind modal-class (if modal-shown "" "hidden"))
  (render (<< [:div#modal {:class modal-class}
               (match modal-content
                 nil nil
                 :help (help-modal-component)
                 :config (config-modal-component)
                 :ls-visible (ls-modal-component false nil)
                 :ls-all (ls-modal-component true nil)
                 [:ls-only selected] (ls-modal-component true selected))])))

(defc calendar-component
  []
  (bind weeks-to-show (sg/kv-lookup :ui :weeks-to-show))
  (bind ^boolean should-alt-color (sg/kv-lookup :ui :alt-color))
  (bind start (actual-start (sg/kv-lookup :ui :start-date)))
  (bind until (day-num-to-date (+ (* 7 weeks-to-show) (:daynum start))))
  (bind days-with-events
        (sg/query (fn [{:keys [events]}]
                    (get-days-with-events start until (vals events)))))
  (render (<< [:div#cal (modal-component)
               [:div#table {:on-click {:e :hide-modal}} [:div.td.th "Sun"]
                [:div.td.th "Mon"] [:div.td.th "Tue"] [:div.td.th "Wed"]
                [:div.td.th "Thu"] [:div.td.th "Fri"] [:div.td.th "Sat"]
                (sg/keyed-seq
                  (mapv day-num-to-date (range (:daynum start) (:daynum until)))
                  :daynum
                  (fn [date]
                    (day-component date
                                   should-alt-color
                                   (== (:daynum start) (:daynum date))
                                   (get days-with-events date))))]
               [:div#cmdline-inout (cmdline-output-component)
                (cmdline-component start until)]])))

;; -------------------------
;; Initialize and hot reload

(defn render!
  []
  (sg/render rt-ref (.getElementById js/document "app") (calendar-component)))

(defn init! [] (init-ui-state!) (init-events-state!) (render!))

(defn ^:dev/after-load reload! [] (render!))
