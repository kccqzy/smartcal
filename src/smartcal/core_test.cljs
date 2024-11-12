(ns smartcal.core-test
  (:require [cljs.test :refer (deftest is)]
            [instaparse.core :as insta]
            [smartcal.core :as c]))

(deftest to-day-num
  (is (= (c/ymd-to-day-num 1600 0 1) 0))
  (is (= (c/ymd-to-day-num 2000 0 1) 146097))
  (is (= (c/ymd-to-day-num 2024 0 1) 154863))
  (is (= (c/ymd-to-day-num 2024 1 1) 154894))
  (is (= (c/ymd-to-day-num 2024 2 1) 154923))
  (is (= (c/ymd-to-day-num 2024 3 1) 154954))
  (is (= (c/ymd-to-day-num 2024 9 1) 155137))
  (is (= (c/ymd-to-day-num 2024 9 100) 155236))
  (is (= (c/ymd-to-day-num 2399 11 31) 292193)))

(deftest from-day-num
  (is (= (c/day-num-to-date 0) (c/Date. 1600 0 1 6 0)))
  (is (= (c/day-num-to-date 154863) (c/Date. 2024 0 1 1 154863)))
  (is (= (c/day-num-to-date 154891) (c/Date. 2024 0 29 1 154891)))
  (is (= (c/day-num-to-date 154892) (c/Date. 2024 0 30 2 154892)))
  (is (= (c/day-num-to-date 154893) (c/Date. 2024 0 31 3 154893)))
  (is (= (c/day-num-to-date 154894) (c/Date. 2024 1 1 4 154894)))
  (is (= (c/day-num-to-date 154923) (c/Date. 2024 2 1 5 154923)))
  (is (= (c/day-num-to-date 154954) (c/Date. 2024 3 1 1 154954)))
  (is (= (c/day-num-to-date 155137) (c/Date. 2024 9 1 2 155137))))

(deftest ymd-map-to-date-checked
  (is (= (c/ymd-map-to-date-checked {:y 1900, :m 0, :d 1})
         (c/ymd-to-date 1900 0 1)))
  (is (thrown? (type {}) (c/ymd-map-to-date-checked {:y 1899, :m 11, :d 31})))
  (is (thrown? (type {}) (c/ymd-map-to-date-checked {:y 1900, :m 0, :d 0})))
  (is (thrown? (type {}) (c/ymd-map-to-date-checked {:y 2100, :m 0, :d 1}))))

(deftest month-num
  (is (= (c/month-num (c/ymd-to-date 1600 0 1)) 0))
  (is (= (c/month-num (c/ymd-to-date 1600 1 1)) 1))
  (is (= (c/month-num (c/ymd-to-date 1601 0 1)) 12))
  (is (= (c/month-num (c/ymd-to-date 1602 1 1)) 25)))

(deftest today
  ;; Please don't run this test near midnight.
  (is (= (:y (c/today)) (.getFullYear (js/Date.))))
  (is (= (:m (c/today)) (.getMonth (js/Date.))))
  (is (= (:d (c/today)) (.getDate (js/Date.))))
  (is (= (:dow (c/today)) (.getDay (js/Date.)))))

(deftest actual-start
  (is (= (c/actual-start (c/ymd-to-date 2024 9 11)) (c/ymd-to-date 2024 9 6)))
  (is (= (c/actual-start (c/ymd-to-date 2024 0 1)) (c/ymd-to-date 2023 11 31))))

(deftest modulo-remainder-seq
  (is (= (c/modulo-remainder-seq 7 0 0 0) []))
  (is (= (c/modulo-remainder-seq 7 71 0 10) [1 8]) "val outside range")
  (is (= (c/modulo-remainder-seq 7 -6 0 10) [1 8]) "val negative")
  (is (= (c/modulo-remainder-seq 7 6 6 6) []) "from = to")
  (is (= (c/modulo-remainder-seq 7 0 1 0) []) "from > to")
  (is (= (c/modulo-remainder-seq 7 6 6 7) [6]))
  (is (= (c/modulo-remainder-seq 7 6 0 21) [6 13 20]))
  (is (= (c/modulo-remainder-seq 2 3 4 8) [5 7]))
  (is (= (c/modulo-remainder-seq 1 3 4 8) [4 5 6 7]))
  (is (chunked-seq? (c/modulo-remainder-seq 7 6 0 21))))

(deftest all-nd-weekday-of-month
  (is (= (c/all-nd-weekdays-of-month [0] 0 9 2024) [(c/ymd-to-date 2024 9 6)]))
  (is (= (c/all-nd-weekdays-of-month [0] 1 9 2024) [(c/ymd-to-date 2024 9 7)]))
  (is (= (c/all-nd-weekdays-of-month [0] 2 9 2024) [(c/ymd-to-date 2024 9 1)]))
  (is (= (c/all-nd-weekdays-of-month [0] 3 9 2024) [(c/ymd-to-date 2024 9 2)]))
  (is (= (c/all-nd-weekdays-of-month [0] 4 9 2024) [(c/ymd-to-date 2024 9 3)]))
  (is (= (c/all-nd-weekdays-of-month [0] 5 9 2024) [(c/ymd-to-date 2024 9 4)]))
  (is (= (c/all-nd-weekdays-of-month [0] 6 9 2024) [(c/ymd-to-date 2024 9 5)]))
  (is (= (c/all-nd-weekdays-of-month [1] 0 9 2024) [(c/ymd-to-date 2024 9 13)]))
  (is (= (c/all-nd-weekdays-of-month [1] 1 9 2024) [(c/ymd-to-date 2024 9 14)]))
  (is (= (c/all-nd-weekdays-of-month [1] 2 9 2024) [(c/ymd-to-date 2024 9 8)]))
  (is (= (c/all-nd-weekdays-of-month [2] 2 9 2024) [(c/ymd-to-date 2024 9 15)]))
  (is (= (c/all-nd-weekdays-of-month [3] 2 9 2024) [(c/ymd-to-date 2024 9 22)]))
  (is (= (c/all-nd-weekdays-of-month [4] 2 9 2024) [(c/ymd-to-date 2024 9 29)]))
  (is (empty? (c/all-nd-weekdays-of-month [5] 2 9 2024))
      "occurrence out of bounds")
  (is (= (c/all-nd-weekdays-of-month [4] 9 9 2024) [(c/ymd-to-date 2024 9 29)]))
  (is (= (c/all-nd-weekdays-of-month [-1] 2 9 2024)
         [(c/ymd-to-date 2024 9 29)]))
  (is (= (c/all-nd-weekdays-of-month [-2] 2 9 2024)
         [(c/ymd-to-date 2024 9 22)]))
  (is (= (c/all-nd-weekdays-of-month [-3] 2 9 2024)
         [(c/ymd-to-date 2024 9 15)]))
  (is (= (c/all-nd-weekdays-of-month [-4] 2 9 2024) [(c/ymd-to-date 2024 9 8)]))
  (is (= (c/all-nd-weekdays-of-month [-5] 2 9 2024) [(c/ymd-to-date 2024 9 1)]))
  (is (empty? (c/all-nd-weekdays-of-month [-6] 2 9 2024))
      "negative occurrence out of bounds"))

(deftest load-state
  (is (= (c/load-state {}) {}))
  (is (= (c/load-state {:weeks-to-show 1}) {:weeks-to-show 1}))
  (is (= (c/load-state {:weeks-to-show -1}) {}))
  (is (= (c/load-state {:weeks-to-show 2, :start-date nil}) {:weeks-to-show 2}))
  (is (= (c/load-state {:weeks-to-show 2, :start-date {:xx 8}})
         {:weeks-to-show 2}))
  (is (= (c/load-state {:weeks-to-show 2, :start-date {:y 1999}})
         {:weeks-to-show 2}))
  (is (= (c/load-state {:weeks-to-show 2, :start-date {:y 1999, :m 9, :d 9}})
         {:weeks-to-show 2, :start-date (c/ymd-to-date 1999 9 9)}))
  (is (= (c/load-state {:weeks-to-show 2, :start-date {:y 1, :m 1, :d 1}})
         {:weeks-to-show 2}))
  (is (= (c/load-state {:weeks-to-show 2, :start-date {:y 1999, :m 12, :d 1}})
         {:weeks-to-show 2})))

(defn parses [s] (c/transform-parsed (insta/parses c/cmdline-parser s)))

(deftest control-language
  (is (= (parses "goto 20241001")
         [[:cmd [:goto-cmd (c/ymd-to-date 2024 9 1)]]]))
  (is (= (parses "goto 2024-10-01")
         [[:cmd [:goto-cmd (c/ymd-to-date 2024 9 1)]]]))
  (is (= (parses "goto 2024  10       01")
         [[:cmd [:goto-cmd (c/ymd-to-date 2024 9 1)]]]))
  (is (= (parses "goto Oct 1, 2024")
         [[:cmd [:goto-cmd (c/ymd-to-date 2024 9 1)]]]))
  (is (= (parses "goto Oct 12, 2024")
         [[:cmd [:goto-cmd (c/ymd-to-date 2024 9 12)]]]))
  (is (= (parses "goto 01 Oct 2024")
         [[:cmd [:goto-cmd (c/ymd-to-date 2024 9 1)]]]))
  (is (= (parses "next") [[:cmd [:next-cmd]]]))
  (is (= (parses "next 2") [[:cmd [:next-cmd 2]]]))
  (is (= (parses "next(2)") [[:cmd [:next-cmd 2]]]))
  (is (= (parses "next (2)") [[:cmd [:next-cmd 2]]]))
  (is (= (parses "next ((2))") [[:cmd [:next-cmd 2]]]))
  (is (= (parses "add \"x\" on 20241001")
         [[:cmd
           [:add-cmd (c/event-from-single-occ "x" (c/ymd-to-date 2024 9 1))]]]))
  (is (= (parses "add \"x\" on Dec 1, 2024")
         [[:cmd
           [:add-cmd
            (c/event-from-single-occ "x" (c/ymd-to-date 2024 11 1))]]]))
  (is (= (parses "add \"x\" every day")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x" {:recur-type :day, :freq 1})]]]))
  (is (= (parses "add \"grocery shopping\" every 3 days")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "grocery shopping"
                                     {:recur-type :day, :freq 3})]]]))
  (is (= (parses "add \"grocery shopping\" every (3) days")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "grocery shopping"
                                     {:recur-type :day, :freq 3})]]]))
  (is (= (parses "add \"grocery shopping\" every (1+1*2) days")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "grocery shopping"
                                     {:recur-type :day, :freq 3})]]]))
  (is (= (parses "add \"grocery shopping\" every(3)days")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "grocery shopping"
                                     {:recur-type :day, :freq 3})]]]))
  (is (= (parses "add \"x\" every week on Mon")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec
              "x"
              {:recur-type :week, :freq 1, :dow #{1}})]]]))
  (is (= (parses "add \"x\" every week on Monday")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec
              "x"
              {:recur-type :week, :freq 1, :dow #{1}})]]]))
  (is (= (parses "add \"x\" every Monday")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec
              "x"
              {:recur-type :week, :freq 1, :dow #{1}})]]]))
  (is (= (parses "add \"x\" every 3 weeks on Monday")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec
              "x"
              {:recur-type :week, :freq 3, :dow #{1}})]]]))
  (is (= (parses "add \"x\" every(3)weeks on Monday")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec
              "x"
              {:recur-type :week, :freq 3, :dow #{1}})]]]))
  (is (= (parses "add \"x\" every week on Monday, Friday")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec
              "x"
              {:recur-type :week, :freq 1, :dow #{1 5}})]]]))
  (is (= (parses "add \"x\" every week on Monday  Wednesday Friday")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec
              "x"
              {:recur-type :week, :freq 1, :dow #{1 3 5}})]]]))
  (is (= (parses "add \"x\" every week on Monday, Wednesday Friday") [])
      "cannot mix comma-separated and space-separated values")
  (is (= (parses "add \"x\" every Monday, Friday")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec
              "x"
              {:recur-type :week, :freq 1, :dow #{1 5}})]]]))
  (is (= (parses "add \"x\" every 2 weeks on Monday , Fri")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec
              "x"
              {:recur-type :week, :freq 2, :dow #{1 5}})]]]))
  (is (= (parses "add \"x\" every month on 18th")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec
              "x"
              {:recur-type :month, :freq 1, :day-selection :d, :d #{18}})]]]))
  (is (= (parses "add \"x\" every month on the 18th")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec
              "x"
              {:recur-type :month, :freq 1, :day-selection :d, :d #{18}})]]]))
  (is
    (= (parses "add \"x\" every month on the 18th, 22nd")
       [[:cmd
         [:add-cmd
          (c/event-from-single-rec
            "x"
            {:recur-type :month, :freq 1, :day-selection :d, :d #{18 22}})]]]))
  (is
    (= (parses "add \"x\" every month on the 18th 22nd")
       [[:cmd
         [:add-cmd
          (c/event-from-single-rec
            "x"
            {:recur-type :month, :freq 1, :day-selection :d, :d #{18 22}})]]]))
  (is (= (parses "add \"x\" every 2 months on the 18th")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec
              "x"
              {:recur-type :month, :freq 2, :day-selection :d, :d #{18}})]]]))
  (is (= (parses "add \"x\" every(2) months on the 18th")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec
              "x"
              {:recur-type :month, :freq 2, :day-selection :d, :d #{18}})]]]))
  (is (= (parses "add \"x\" every 18th of the month")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec
              "x"
              {:recur-type :month, :freq 1, :day-selection :d, :d #{18}})]]]))
  (is (= (parses "add \"x\" every first Monday of the month")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :month,
                                      :freq 1,
                                      :day-selection :dow,
                                      :dow 1,
                                      :occ #{0}})]]]))
  (is (= (parses "add \"x\" every first Monday of each month")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :month,
                                      :freq 1,
                                      :day-selection :dow,
                                      :dow 1,
                                      :occ #{0}})]]]))
  (is (= (parses "add \"x\" every first, third Monday of each month")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :month,
                                      :freq 1,
                                      :day-selection :dow,
                                      :dow 1,
                                      :occ #{0 2}})]]]))
  (is (= (parses "add \"x\" every month on the first Monday")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :month,
                                      :freq 1,
                                      :day-selection :dow,
                                      :dow 1,
                                      :occ #{0}})]]]))
  (is (= (parses "add \"x\" every month on the last Monday")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :month,
                                      :freq 1,
                                      :day-selection :dow,
                                      :dow 1,
                                      :occ #{-1}})]]]))
  (is (= (parses "add \"x\" every 2 months on the last Monday")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :month,
                                      :freq 2,
                                      :day-selection :dow,
                                      :dow 1,
                                      :occ #{-1}})]]]))
  (is
    (= (parses "add \"x\" every year on Dec 26")
       [[:cmd
         [:add-cmd
          (c/event-from-single-rec
            "x"
            {:recur-type :year, :freq 1, :day-selection :md, :m 11, :d 26})]]]))
  (is
    (= (parses "add \"x\" every year on 26 Dec")
       [[:cmd
         [:add-cmd
          (c/event-from-single-rec
            "x"
            {:recur-type :year, :freq 1, :day-selection :md, :m 11, :d 26})]]]))
  (is
    (= (parses "add \"x\" every 2 years on 26 Dec")
       [[:cmd
         [:add-cmd
          (c/event-from-single-rec
            "x"
            {:recur-type :year, :freq 2, :day-selection :md, :m 11, :d 26})]]]))
  (is
    (= (parses "add \"x\" every (2)years on 26 Dec")
       [[:cmd
         [:add-cmd
          (c/event-from-single-rec
            "x"
            {:recur-type :year, :freq 2, :day-selection :md, :m 11, :d 26})]]]))
  (is (= (parses "add \"x\" every year on last Friday of June")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :year,
                                      :freq 1,
                                      :day-selection :occ-dow-month,
                                      :occ #{-1},
                                      :m #{5},
                                      :dow 5})]]]))
  (is (=
        (parses
          "add \"fut exp\" every year on third Friday of March, June, Sep, Dec")
        [[:cmd
          [:add-cmd
           (c/event-from-single-rec "fut exp"
                                    {:recur-type :year,
                                     :freq 1,
                                     :day-selection :occ-dow-month,
                                     :occ #{2},
                                     :m #{2 5 8 11},
                                     :dow 5})]]]))
  (is (= (parses "add \"x\" every year on the second Monday of Jan")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :year,
                                      :freq 1,
                                      :day-selection :occ-dow-month,
                                      :occ #{1},
                                      :m #{0},
                                      :dow 1})]]]))
  (is (= (parses
           "add \"x\" every year on the first, third Wednesday of Jan, Jul")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :year,
                                      :freq 1,
                                      :day-selection :occ-dow-month,
                                      :occ #{0 2},
                                      :m #{0 6},
                                      :dow 3})]]]))
  (is (= (parses "add \"x\" every 4 years on the last Monday of February")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :year,
                                      :freq 4,
                                      :day-selection :occ-dow-month,
                                      :occ #{-1},
                                      :m #{1},
                                      :dow 1})]]]))
  (is (= (parses "add \"x\" every week on Mon from 20200101 ")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :week,
                                      :freq 1,
                                      :dow #{1},
                                      :recur-start
                                        (c/ymd-to-date 2020 0 1)})]]]))
  (is (= (parses "add \"x\" every week on Mon until 20200101 ")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :week,
                                      :freq 1,
                                      :dow #{1},
                                      :recur-end (c/ymd-to-date 2020 0 1)})]]]))
  (is (= (parses "add \"x\" every week on Mon from 20200101 until Feb 1, 2020")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :week,
                                      :freq 1,
                                      :dow #{1},
                                      :recur-start (c/ymd-to-date 2020 0 1),
                                      :recur-end (c/ymd-to-date 2020 1 1)})]]]))
  (is (= (parses "add \"x\" on plus(Dec 1, 2024, d(12))")
         [[:cmd
           [:add-cmd
            (c/event-from-single-occ "x" (c/ymd-to-date 2024 11 13))]]]))
  (is (= (parses "add \"x\" on plus(Dec 1, 2024, d(31))")
         [[:cmd
           [:add-cmd (c/event-from-single-occ "x" (c/ymd-to-date 2025 0 1))]]]))
  (is (= (parses "add \"x\" on plus(Dec 1, 2024, d(1+2))")
         [[:cmd
           [:add-cmd
            (c/event-from-single-occ "x" (c/ymd-to-date 2024 11 4))]]]))
  (is (= (parses "add \"x\" on plus(Dec 1, 2024, d(2*7))")
         [[:cmd
           [:add-cmd
            (c/event-from-single-occ "x" (c/ymd-to-date 2024 11 15))]]]))
  (is (= (parses "add \"x\" on plus(Dec 1, 2024, d(7/2))")
         [[:cmd
           [:add-cmd
            (c/event-from-single-occ "x" (c/ymd-to-date 2024 11 4))]]]))
  (is (= (parses "add \"x\" on plus(Dec 1, 2024, d(7%2))")
         [[:cmd
           [:add-cmd
            (c/event-from-single-occ "x" (c/ymd-to-date 2024 11 2))]]]))
  (is (= (parses "add \"x\" on plus(Dec 1, 2024, d(1-2/(3-4)+5*6))")
         [[:cmd
           [:add-cmd (c/event-from-single-occ "x" (c/ymd-to-date 2025 0 3))]]]))
  (is (= (parses "add \"x\" on plus(Dec 1, 2024, 12d)")
         [[:cmd
           [:add-cmd
            (c/event-from-single-occ "x" (c/ymd-to-date 2024 11 13))]]])
      "add date by day literals")
  (is (= (parses "add \"x\" on plus(Dec 1, 2024, m(1))")
         [[:cmd
           [:add-cmd (c/event-from-single-occ "x" (c/ymd-to-date 2025 0 1))]]]))
  (is (= (parses "add \"x\" on plus(Dec 1, 2024, 1m)")
         [[:cmd
           [:add-cmd (c/event-from-single-occ "x" (c/ymd-to-date 2025 0 1))]]]))
  (is (= (parses "add \"x\" on plus(Dec 1, 2024, m(0-1))")
         [[:cmd
           [:add-cmd
            (c/event-from-single-occ "x" (c/ymd-to-date 2024 10 1))]]]))
  (is (= (parses "add \"x\" on plus(Dec 1, 2024, 1y)")
         [[:cmd
           [:add-cmd
            (c/event-from-single-occ "x" (c/ymd-to-date 2025 11 1))]]]))
  (is (= (parses "add \"x\" on plus(Dec 1, 2024, 10y)")
         [[:cmd
           [:add-cmd
            (c/event-from-single-occ "x" (c/ymd-to-date 2034 11 1))]]])))

(deftest recurrent-event-occurrences
  (is (= (c/recurrent-event-occurrences {:recur-type :day, :freq 1}
                                        (c/ymd-to-date 2024 9 18)
                                        (c/ymd-to-date 2024 9 18)
                                        (c/ymd-to-date 2024 9 25))
         [(c/ymd-to-date 2024 9 18) (c/ymd-to-date 2024 9 19)
          (c/ymd-to-date 2024 9 20) (c/ymd-to-date 2024 9 21)
          (c/ymd-to-date 2024 9 22) (c/ymd-to-date 2024 9 23)
          (c/ymd-to-date 2024 9 24)]))
  (is (= (c/recurrent-event-occurrences {:recur-type :day, :freq 3}
                                        (c/ymd-to-date 2024 9 18)
                                        (c/ymd-to-date 2024 9 18)
                                        (c/ymd-to-date 2024 10 3))
         [(c/ymd-to-date 2024 9 18) (c/ymd-to-date 2024 9 21)
          (c/ymd-to-date 2024 9 24) (c/ymd-to-date 2024 9 27)
          (c/ymd-to-date 2024 9 30) (c/ymd-to-date 2024 10 2)]))
  (is (= (c/recurrent-event-occurrences {:recur-type :day, :freq 30}
                                        (c/ymd-to-date 2024 0 1)
                                        (c/ymd-to-date 2023 0 1)
                                        (c/ymd-to-date 2024 4 2))
         [(c/ymd-to-date 2024 0 1) (c/ymd-to-date 2024 0 31)
          (c/ymd-to-date 2024 2 1) (c/ymd-to-date 2024 2 31)
          (c/ymd-to-date 2024 3 30)]))
  (is (= (c/recurrent-event-occurrences {:recur-type :week, :freq 1, :dow #{1}}
                                        (c/ymd-to-date 2024 0 1)
                                        (c/ymd-to-date 2024 0 1)
                                        (c/ymd-to-date 2024 1 1))
         [(c/ymd-to-date 2024 0 1) (c/ymd-to-date 2024 0 8)
          (c/ymd-to-date 2024 0 15) (c/ymd-to-date 2024 0 22)
          (c/ymd-to-date 2024 0 29)]))
  (is (= (c/recurrent-event-occurrences {:recur-type :week, :freq 1, :dow #{2}}
                                        (c/ymd-to-date 2024 0 1)
                                        (c/ymd-to-date 2024 0 1)
                                        (c/ymd-to-date 2024 1 1))
         [(c/ymd-to-date 2024 0 2) (c/ymd-to-date 2024 0 9)
          (c/ymd-to-date 2024 0 16) (c/ymd-to-date 2024 0 23)
          (c/ymd-to-date 2024 0 30)]))
  (is (= (c/recurrent-event-occurrences {:recur-type :week, :freq 1, :dow #{2}}
                                        ;; Recurrence started long before
                                        ;; query window
                                        (c/ymd-to-date 2020 0 1)
                                        (c/ymd-to-date 2024 0 1)
                                        (c/ymd-to-date 2024 1 1))
         [(c/ymd-to-date 2024 0 2) (c/ymd-to-date 2024 0 9)
          (c/ymd-to-date 2024 0 16) (c/ymd-to-date 2024 0 23)
          (c/ymd-to-date 2024 0 30)]))
  (is (= (c/recurrent-event-occurrences
           {:recur-type :week, :freq 1, :dow #{1 5}}
           (c/ymd-to-date 2024 0 5)
           ;; Query window before recurrence start.
           (c/ymd-to-date 2020 0 1)
           (c/ymd-to-date 2024 0 20))
         [(c/ymd-to-date 2024 0 5) (c/ymd-to-date 2024 0 8)
          (c/ymd-to-date 2024 0 12) (c/ymd-to-date 2024 0 15)
          (c/ymd-to-date 2024 0 19)]))
  (is (= (c/recurrent-event-occurrences {:recur-type :week, :freq 2, :dow #{2}}
                                        (c/ymd-to-date 2024 0 1)
                                        (c/ymd-to-date 2024 0 1)
                                        (c/ymd-to-date 2024 1 1))
         [(c/ymd-to-date 2024 0 2) (c/ymd-to-date 2024 0 16)
          (c/ymd-to-date 2024 0 30)]))
  (is (= (c/recurrent-event-occurrences {:recur-type :week, :freq 4, :dow #{1}}
                                        (c/ymd-to-date 2024 0 5)
                                        (c/ymd-to-date 2024 0 1)
                                        (c/ymd-to-date 2024 2 20))
         [(c/ymd-to-date 2024 0 8) (c/ymd-to-date 2024 1 5)
          (c/ymd-to-date 2024 2 4)]))
  (is (= (c/recurrent-event-occurrences
           {:recur-type :week, :freq 2, :dow #{2 6}}
           (c/ymd-to-date 2024 0 1)
           (c/ymd-to-date 2024 0 1)
           (c/ymd-to-date 2024 1 1))
         [(c/ymd-to-date 2024 0 2) (c/ymd-to-date 2024 0 6)
          (c/ymd-to-date 2024 0 16) (c/ymd-to-date 2024 0 20)
          (c/ymd-to-date 2024 0 30)]))
  (is (= (c/recurrent-event-occurrences
           {:recur-type :week, :freq 2, :dow #{1 5}}
           (c/ymd-to-date 2024 0 5)
           (c/ymd-to-date 2020 0 1)
           (c/ymd-to-date 2024 0 20))
         [(c/ymd-to-date 2024 0 5) (c/ymd-to-date 2024 0 8)
          (c/ymd-to-date 2024 0 19)]))
  (is (= (c/recurrent-event-occurrences
           {:recur-type :month, :freq 1, :day-selection :d, :d #{1}}
           (c/ymd-to-date 2024 0 1)
           (c/ymd-to-date 2020 0 1)
           (c/ymd-to-date 2024 5 1))
         [(c/ymd-to-date 2024 0 1) (c/ymd-to-date 2024 1 1)
          (c/ymd-to-date 2024 2 1) (c/ymd-to-date 2024 3 1)
          (c/ymd-to-date 2024 4 1)]))
  (is (= (c/recurrent-event-occurrences
           {:recur-type :month, :freq 2, :day-selection :d, :d #{1}}
           (c/ymd-to-date 2024 0 1)
           (c/ymd-to-date 2020 0 1)
           (c/ymd-to-date 2024 5 2))
         [(c/ymd-to-date 2024 0 1) (c/ymd-to-date 2024 2 1)
          (c/ymd-to-date 2024 4 1)]))
  (is (= (c/recurrent-event-occurrences
           {:recur-type :month, :freq 3, :day-selection :d, :d #{8 18 28}}
           (c/ymd-to-date 2024 0 1)
           (c/ymd-to-date 2020 0 1)
           (c/ymd-to-date 2024 7 1))
         [(c/ymd-to-date 2024 0 8) (c/ymd-to-date 2024 0 18)
          (c/ymd-to-date 2024 0 28) (c/ymd-to-date 2024 3 8)
          (c/ymd-to-date 2024 3 18) (c/ymd-to-date 2024 3 28)
          (c/ymd-to-date 2024 6 8) (c/ymd-to-date 2024 6 18)
          (c/ymd-to-date 2024 6 28)]))
  (is (= (c/recurrent-event-occurrences
           {:recur-type :month, :freq 1, :day-selection :d, :d #{31}}
           (c/ymd-to-date 2024 0 1)
           (c/ymd-to-date 2020 0 1)
           (c/ymd-to-date 2025 0 1))
         [(c/ymd-to-date 2024 0 31) (c/ymd-to-date 2024 2 31)
          (c/ymd-to-date 2024 4 31) (c/ymd-to-date 2024 6 31)
          (c/ymd-to-date 2024 7 31) (c/ymd-to-date 2024 9 31)
          (c/ymd-to-date 2024 11 31)])
      "skips over non-existent 31st days")
  (is (= (c/recurrent-event-occurrences
           {:recur-type :month, :freq 2, :day-selection :d, :d #{31}}
           (c/ymd-to-date 2024 0 1)
           (c/ymd-to-date 2020 0 1)
           (c/ymd-to-date 2025 0 1))
         [(c/ymd-to-date 2024 0 31) (c/ymd-to-date 2024 2 31)
          (c/ymd-to-date 2024 4 31) (c/ymd-to-date 2024 6 31)])
      "skips over non-existent 31st days and also every second month")
  (is (= (c/select-dates-from-month-recur
           {:day-selection :dow, :dow 1, :occ #{0}}
           (c/month-num {:y 2024, :m 0}))
         [(c/ymd-to-date 2024 0 1)]))
  (is (= (c/recurrent-event-occurrences
           {:recur-type :month, :freq 1, :day-selection :dow, :dow 1, :occ #{0}}
           (c/ymd-to-date 2024 0 1)
           (c/ymd-to-date 2020 0 1)
           (c/ymd-to-date 2024 6 1))
         [(c/ymd-to-date 2024 0 1) (c/ymd-to-date 2024 1 5)
          (c/ymd-to-date 2024 2 4) (c/ymd-to-date 2024 3 1)
          (c/ymd-to-date 2024 4 6) (c/ymd-to-date 2024 5 3)])
      "finds first Monday of each month")
  (is (= (c/recurrent-event-occurrences {:recur-type :month,
                                         :freq 1,
                                         :day-selection :dow,
                                         :dow 1,
                                         :occ #{0 -1}}
                                        (c/ymd-to-date 2024 0 1)
                                        (c/ymd-to-date 2020 0 1)
                                        (c/ymd-to-date 2024 3 1))
         [(c/ymd-to-date 2024 0 1) (c/ymd-to-date 2024 0 29)
          (c/ymd-to-date 2024 1 5) (c/ymd-to-date 2024 1 26)
          (c/ymd-to-date 2024 2 4) (c/ymd-to-date 2024 2 25)])
      "finds first and last Monday of each month")
  (is
    (= (c/recurrent-event-occurrences
         {:recur-type :month, :freq 1, :day-selection :dow, :dow 1, :occ #{0 4}}
         (c/ymd-to-date 2024 0 1)
         (c/ymd-to-date 2020 0 1)
         (c/ymd-to-date 2024 4 1))
       [(c/ymd-to-date 2024 0 1) (c/ymd-to-date 2024 0 29)
        (c/ymd-to-date 2024 1 5) (c/ymd-to-date 2024 2 4)
        (c/ymd-to-date 2024 3 1) (c/ymd-to-date 2024 3 29)])
    "finds first and fifth Monday of each month")
  (is
    (= (c/recurrent-event-occurrences
         {:recur-type :month, :freq 2, :day-selection :dow, :dow 1, :occ #{0 4}}
         (c/ymd-to-date 2024 0 1)
         (c/ymd-to-date 2020 0 1)
         (c/ymd-to-date 2024 6 30))
       [(c/ymd-to-date 2024 0 1) (c/ymd-to-date 2024 0 29)
        (c/ymd-to-date 2024 2 4) (c/ymd-to-date 2024 4 6)
        (c/ymd-to-date 2024 6 1) (c/ymd-to-date 2024 6 29)])
    "finds first and fifth Monday of every 2 months")
  (is
    (= (c/recurrent-event-occurrences
         {:recur-type :month, :freq 13, :day-selection :dow, :dow 0, :occ #{-1}}
         (c/ymd-to-date 2020 0 1)
         (c/ymd-to-date 2020 0 1)
         (c/ymd-to-date 2024 6 30))
       [(c/ymd-to-date 2020 0 26) (c/ymd-to-date 2021 1 28)
        (c/ymd-to-date 2022 2 27) (c/ymd-to-date 2023 3 30)
        (c/ymd-to-date 2024 4 26)])
    "finds last Sunday every 13 months")
  (is (= (c/recurrent-event-occurrences
           {:recur-type :year, :freq 1, :day-selection :md, :m 11, :d 25}
           (c/ymd-to-date 2020 0 1)
           (c/ymd-to-date 2019 0 1)
           (c/ymd-to-date 2025 0 1))
         [(c/ymd-to-date 2020 11 25) (c/ymd-to-date 2021 11 25)
          (c/ymd-to-date 2022 11 25) (c/ymd-to-date 2023 11 25)
          (c/ymd-to-date 2024 11 25)])
      "finds fixed date every year")
  (is (= (c/recurrent-event-occurrences
           {:recur-type :year, :freq 1, :day-selection :md, :m 1, :d 29}
           (c/ymd-to-date 1995 0 1)
           (c/ymd-to-date 1995 0 1)
           (c/ymd-to-date 2010 0 1))
         [(c/ymd-to-date 1996 1 29) (c/ymd-to-date 2000 1 29)
          (c/ymd-to-date 2004 1 29) (c/ymd-to-date 2008 1 29)])
      "finds Feb 29 every four years")
  (is (= (c/recurrent-event-occurrences
           {:recur-type :year, :freq 5, :day-selection :md, :m 6, :d 30}
           (c/ymd-to-date 1995 0 1)
           (c/ymd-to-date 1995 0 1)
           (c/ymd-to-date 2011 0 1))
         [(c/ymd-to-date 1995 6 30) (c/ymd-to-date 2000 6 30)
          (c/ymd-to-date 2005 6 30) (c/ymd-to-date 2010 6 30)])
      "finds a date every 5 years")
  (is (= (c/recurrent-event-occurrences {:recur-type :year,
                                         :freq 1,
                                         :day-selection :occ-dow-month,
                                         :occ #{0},
                                         :dow 0,
                                         :m #{0 1}}
                                        (c/ymd-to-date 2020 0 1)
                                        (c/ymd-to-date 2020 0 1)
                                        (c/ymd-to-date 2022 0 1))
         [(c/ymd-to-date 2020 0 5) (c/ymd-to-date 2020 1 2)
          (c/ymd-to-date 2021 0 3) (c/ymd-to-date 2021 1 7)])))

(def example-events
  [(c/event-from-single-occ "2023 New Year" (c/ymd-to-date 2023 0 1))
   (c/event-from-single-occ "2024 New Year" (c/ymd-to-date 2024 0 1))
   (c/event-from-single-rec
     "New Year"
     {:recur-type :year, :freq 1, :day-selection :md, :m 0, :d 1})
   (c/event-from-single-rec
     "Good Day"
     {:recur-type :year, :freq 1, :day-selection :md, :m 1, :d 1})
   (c/event-from-single-rec "Very Good Day"
                            {:recur-type :year,
                             :freq 1,
                             :day-selection :occ-dow-month,
                             :m #{0},
                             :dow 0,
                             :occ #{1 3}})
   (c/merge-event (c/event-from-single-occ "Hello" (c/ymd-to-date 2020 8 1))
                  (c/event-from-single-occ "Hello" (c/ymd-to-date 2020 8 3)))])

(deftest event-to-dates
  (is
    (= (c/event-to-dates
         (c/ymd-to-date 2024 0 1)
         (c/ymd-to-date 2025 0 1)
         (-> (c/event-from-single-occ "x" (c/ymd-to-date 2024 2 1))
             (c/merge-event (c/event-from-single-occ "x"
                                                     (c/ymd-to-date 2024 2 6)))
             (c/merge-event (c/event-from-single-occ "x"
                                                     (c/ymd-to-date 2024 2 30)))
             (c/merge-event (c/event-from-single-rec
                              "x"
                              {:recur-type :month,
                               :freq 3,
                               :day-selection :d,
                               :d #{6},
                               :recur-start (c/ymd-to-date 2024 2 1)}))))
       [(c/ymd-to-date 2024 2 1) (c/ymd-to-date 2024 2 6)
        (c/ymd-to-date 2024 2 30) (c/ymd-to-date 2024 5 6)
        (c/ymd-to-date 2024 8 6) (c/ymd-to-date 2024 11 6)])))

(deftest get-visible-events
  (is (= (c/get-visible-events (c/ymd-to-date 2024 0 1)
                               (c/ymd-to-date 2024 1 1)
                               example-events)
         [{:date (c/ymd-to-date 2024 0 1), :event (get example-events 1)}
          {:date (c/ymd-to-date 2024 0 1), :event (get example-events 2)}
          {:date (c/ymd-to-date 2024 0 14), :event (get example-events 4)}
          {:date (c/ymd-to-date 2024 0 28), :event (get example-events 4)}]))
  (is (= (c/get-visible-events (c/ymd-to-date 2020 8 1)
                               (c/ymd-to-date 2020 8 3)
                               example-events)
         [{:date (c/ymd-to-date 2020 8 1), :event (get example-events 5)}]))
  (is (= (c/get-visible-events (c/ymd-to-date 2020 8 1)
                               (c/ymd-to-date 2020 8 4)
                               example-events)
         [{:date (c/ymd-to-date 2020 8 1), :event (get example-events 5)}
          {:date (c/ymd-to-date 2020 8 3), :event (get example-events 5)}]))
  (is (= (c/get-visible-events (c/ymd-to-date 2020 8 2)
                               (c/ymd-to-date 2020 8 4)
                               example-events)
         [{:date (c/ymd-to-date 2020 8 3), :event (get example-events 5)}])))

(deftest get-days-with-events
  (is (= (c/get-days-with-events (c/ymd-to-date 2024 0 1)
                                 (c/ymd-to-date 2024 1 1)
                                 example-events)
         {(c/ymd-to-date 2024 0 1) [(get example-events 1)
                                    (get example-events 2)],
          (c/ymd-to-date 2024 0 14) [(get example-events 4)],
          (c/ymd-to-date 2024 0 28) [(get example-events 4)]})))

(deftest format-recur-pat
  (is (= (c/format-recur-pat {:recur-type :day, :freq 1}) "every day"))
  (is (= (c/format-recur-pat {:recur-type :day, :freq 3}) "every 3 days"))
  (is (= (c/format-recur-pat {:recur-type :day, :freq 300}) "every 300 days"))
  (is (= (c/format-recur-pat {:recur-type :day,
                              :freq 300,
                              :recur-start (c/ymd-to-date 2020 10 1)})
         "every 300 days from Nov 1, 2020"))
  (is (= (c/format-recur-pat
           {:recur-type :day, :freq 300, :recur-start c/epoch})
         "every 300 days"))
  (is (= (c/format-recur-pat {:recur-type :week, :freq 1, :dow #{1}})
         "every week on Mon"))
  (is (= (c/format-recur-pat {:recur-type :week, :freq 1, :dow #{1 3 5}})
         "every week on Mon, Wed, Fri"))
  (is (= (c/format-recur-pat {:recur-type :week, :freq 1, :dow #{1 3 5}})
         "every week on Mon, Wed, Fri"))
  (is (= (c/format-recur-pat
           {:recur-type :month, :freq 1, :day-selection :d, :d #{1 3 5}})
         "every month on the 1st, 3rd, 5th"))
  (is (= (c/format-recur-pat
           {:recur-type :month, :freq 2, :day-selection :d, :d #{1 11 21 31}})
         "every 2 months on the 1st, 11th, 21st, 31st"))
  (is (=
        (c/format-recur-pat
          {:recur-type :month, :freq 2, :day-selection :dow, :dow 1, :occ #{0}})
        "every 2 months on the first Mon"))
  (is
    (=
      (c/format-recur-pat
        {:recur-type :month, :freq 2, :day-selection :dow, :dow 1, :occ #{0 2}})
      "every 2 months on the first, third Mon"))
  (is (= (c/format-recur-pat
           {:recur-type :year, :freq 1, :day-selection :md, :m 1, :d 2})
         "every year on Feb 2"))
  (is (= (c/format-recur-pat {:recur-type :year,
                              :freq 1,
                              :day-selection :occ-dow-month,
                              :occ #{-1},
                              :m #{5},
                              :dow 5})
         "every year on the last Fri of Jun")))

(deftest format-event
  (is (= (c/format-event (c/event-from-single-occ "x" (c/ymd-to-date 2010 1 1))
                         nil
                         nil)
         "an event named \"x\" on Feb 1, 2010"))
  (is (= (c/format-event (c/event-from-single-rec "x"
                                                  {:recur-type :year,
                                                   :freq 1,
                                                   :day-selection
                                                     :occ-dow-month,
                                                   :occ #{-1},
                                                   :m #{5},
                                                   :dow 5})
                         nil
                         nil)
         "an event named \"x\" repeating every year on the last Fri of Jun")))

(deftest simplified-glob-to-regex
  (is (= (c/simplified-glob-to-regex "abc*def") "abc.*def"))
  (is (= (c/simplified-glob-to-regex "abc*") "abc.*"))
  (is (= (c/simplified-glob-to-regex "*") ".*"))
  (is (= (c/simplified-glob-to-regex "?????") "....."))
  (is (= (c/simplified-glob-to-regex "abc?") "abc."))
  (is (= (c/simplified-glob-to-regex "abc.*") "abc\\..*"))
  (is (= (c/simplified-glob-to-regex "abc$*") "abc\\$.*"))
  (is (= (c/simplified-glob-to-regex "abc\\*") "abc\\*"))
  (is (= (c/simplified-glob-to-regex "abc+*") "abc\\+.*")))

(deftest eval-str-exprs
  (is (= (c/eval-str-exprs ["x" "y"] ["a" "x" "y" "z"]) #{"x" "y"}))
  (is (= (c/eval-str-exprs ["x" {:str-glob-fun "*y"}]
                           ["a" "x" "xy" "y" "yz" "z"])
         #{"x" "y" "xy"}))
  (is (= (c/eval-str-exprs [{:str-glob-fun "?"}] ["😄"]) #{"😄"}))
  (is (= (c/eval-str-exprs ["x" "y" "z"] ["Gray"]) #{})))

(deftest remove-subsequence
  (is (= (c/remove-subsequence "" ">>> ") ""))
  (is (= (c/remove-subsequence ">>> " ">>> ") ""))
  (is (= (c/remove-subsequence ">>> a" ">>> ") "a"))
  (is (= (c/remove-subsequence "a>>> " ">>> ") "a"))
  (is (= (c/remove-subsequence ">p>> " ">>> ") "p"))
  (is (= (c/remove-subsequence "a>b>>c " ">>> ") "abc"))
  (is (= (c/remove-subsequence "ab>>c " ">>> ") "abc ")))

(deftest history-add
  (is (= (c/history-add c/history-initial-state "abcd")
         (assoc c/history-initial-state :hist-entries ["abcd"])))
  (is (= (-> c/history-initial-state
             (c/history-add "abcd")
             (c/history-add "efgh"))
         (assoc c/history-initial-state :hist-entries ["abcd" "efgh"]))
      "skips when the new history is the same as the last")
  (is (= (-> c/history-initial-state
             (c/history-add "abcd")
             (c/history-add "efgh")
             (c/history-add "efgh")
             (c/history-add "abcd"))
         (assoc c/history-initial-state :hist-entries ["abcd" "efgh" "abcd"]))
      "does not skip when the new history is the same as the first")
  (is (= (let [truncated-history (reduce c/history-add
                                   c/history-initial-state
                                   (range (* 2 c/history-limit)))]
           (:hist-entries truncated-history))
         (range c/history-limit (* 2 c/history-limit)))
      "truncates from the front when exceeding twice the limit"))

(deftest history-search
  (is (= (:hist-cur-idx (c/history-search c/history-initial-state "abc" true))
         nil))
  (is (= (:hist-cur-idx (c/history-search (c/history-add c/history-initial-state
                                                         "abc")
                                          "abc"
                                          true))
         0))
  (is (thrown? js/Error
               (:hist-cur-idx (c/history-search
                                (c/history-add c/history-initial-state "abc")
                                "abc"
                                false))))
  (is (= (:hist-cur-idx (-> c/history-initial-state
                            (c/history-add "abc")
                            (c/history-add "def")
                            (c/history-add "abc")
                            (c/history-search "def" true)))
         1))
  (is (= (:hist-cur-idx (-> c/history-initial-state
                            (c/history-add "abc")
                            (c/history-add "def")
                            (c/history-add "abc")
                            (c/history-search "xxx" true)))
         nil)
      "search not found")
  (is (= (:hist-cur-idx (-> c/history-initial-state
                            (c/history-add "abc")
                            (c/history-add "def")
                            (c/history-add "abc")
                            (c/history-search "abc" true)))
         2))
  (is (= (:hist-cur-idx (-> c/history-initial-state
                            (c/history-add "abc")
                            (c/history-add "agh")
                            (c/history-add "abc")
                            (c/history-search "a" true)))
         2))
  (is (= (:hist-cur-idx (-> c/history-initial-state
                            (c/history-add "aeg")
                            (c/history-add "xxx")
                            (c/history-add "abc")
                            (c/history-search "a" true)
                            (c/history-search "a" true)))
         0))
  (is (= (:hist-cur-idx (-> c/history-initial-state
                            (c/history-add "aeg")
                            (c/history-add "xxx")
                            (c/history-add "abc")
                            (c/history-search "a" true)
                            (c/history-search "a" true)
                            (c/history-search "a" true)))
         0))
  (is (= (:hist-cur-idx (-> c/history-initial-state
                            (c/history-add "abc")
                            (c/history-add "xxx")
                            (c/history-add "abc")
                            (c/history-search "a" true)
                            (c/history-search "a" true)
                            (c/history-search "a" false)))
         2))
  (is (thrown? js/Error
               (-> c/history-initial-state
                   (c/history-add "abc")
                   (c/history-add "xxx")
                   (c/history-add "abc")
                   (c/history-search "a" true)
                   (c/history-search "x" true))))
  (is (= (:hist-cur-idx (-> c/history-initial-state
                            (c/history-add "abc")
                            (c/history-add "xxx")
                            (c/history-add "abc")
                            (c/history-search "a" true)
                            (c/history-search "a" false)
                            (c/history-search "x" true)))
         1))
  (is (= (:hist-cur-idx (-> (reduce c/history-add
                                    (c/history-add c/history-initial-state "a")
                                    (map #(str "b" %) (range c/history-limit)))
                            ;; "a" "b0" ... "b499"
                            (c/history-search "a" true)))
         nil)
      "cannot find history beyond the limit")
  (is (= (:hist-cur-idx (-> (reduce c/history-add
                                    (c/history-add c/history-initial-state "a")
                                    (map #(str "b" %) (range c/history-limit)))
                            (c/history-search "b0" true)))
         1)
      "can find history just within limit")
  (is (= (:hist-cur-idx (-> (reduce c/history-add
                                    (c/history-add c/history-initial-state "a")
                                    (map #(str "b" %)
                                      (range (dec c/history-limit))))
                            (c/history-add "b0")
                            ;; "a" "b0" ... "b498" "b0"
                            (c/history-search "b0" true)))
         c/history-limit)
      "hist-cur-idx can equal limit")
  (is (= (:hist-cur-idx (-> (reduce c/history-add
                                    (c/history-add c/history-initial-state "a")
                                    (map #(str "b" %)
                                      (range (dec c/history-limit))))
                            (c/history-add "b0499")
                            (c/history-add "b0")
                            ;; "a" "b0" ... "b498" "b0" "b0"
                            (c/history-search "b0" true)))
         (inc c/history-limit))
      "hist-cur-idx can exceed limit")
  (is (= (:hist-cur-idx (-> (reduce c/history-add
                                    (c/history-add c/history-initial-state "a")
                                    (map #(str "b" %)
                                      (range (dec c/history-limit))))
                            (c/history-add "b0")
                            ;; "a" "b0" ... "b498" "b0"
                            (c/history-search "b0" true)
                            (c/history-search "b0" true)))
         1)
      "can find subsequent history just within limit")
  (is (= (:hist-cur-idx (-> (reduce c/history-add
                                    (c/history-add c/history-initial-state "a")
                                    (map #(str "b" %)
                                      (range (dec c/history-limit))))
                            (c/history-add "b0")
                            ;; "a" "b0" ... "b498" "b0"
                            (c/history-search "b0" true)
                            (c/history-search "b0" true)
                            (c/history-search "b0" true)))
         1)
      "cannot find subsequent history beyond limit"))

(def fake-history ["abc" "aac" "bbc" "ccd" "zzz"])

(def fake-history-state
  (assoc c/history-initial-state :hist-entries fake-history))

(deftest history-search-current-completion
  (is (= (c/history-search-current-completion (assoc fake-history-state
                                                :cmdline-input ""))
         nil)
      "no search when input empty")
  (is (= (c/history-search-current-completion (assoc fake-history-state
                                                :cmdline-input "z"))
         "zzz"))
  (is (= (c/history-search-current-completion (assoc fake-history-state
                                                :cmdline-input "c"))
         "ccd"))
  (is (= (c/history-search-current-completion (assoc fake-history-state
                                                :cmdline-input "a"))
         "aac")))

(deftest history-search-navigate
  (is (= (-> (assoc fake-history-state :cmdline-input "")
             (c/history-search-navigate true))
         (-> fake-history-state
             (assoc :cmdline-input "zzz")
             (assoc :hist-cur-idx 4)
             (assoc :hist-cur-prefix ""))))
  (is (= (-> (assoc fake-history-state :cmdline-input "z")
             (c/history-search-navigate true))
         (-> fake-history-state
             (assoc :cmdline-input "zzz")
             (assoc :hist-cur-idx 4)
             (assoc :hist-cur-prefix "z"))))
  (is (= (-> (assoc fake-history-state :cmdline-input "z")
             ;; Navigate up twice. No more matches.
             (c/history-search-navigate true)
             (c/history-search-navigate true))
         (-> fake-history-state
             (assoc :cmdline-input "zzz")
             (assoc :hist-cur-idx 4)
             (assoc :hist-cur-prefix "z"))))
  (is (= (-> (assoc fake-history-state :cmdline-input "z")
             ;; Navigate up twice. No more matches.
             (c/history-search-navigate true)
             (c/history-search-navigate true))
         (-> fake-history-state
             (assoc :cmdline-input "zzz")
             (assoc :hist-cur-idx 4)
             (assoc :hist-cur-prefix "z"))))
  (is (= (-> (assoc fake-history-state :cmdline-input "a")
             (c/history-search-navigate true))
         (-> fake-history-state
             (assoc :cmdline-input "aac")
             (assoc :hist-cur-idx 1)
             (assoc :hist-cur-prefix "a"))))
  (is (= (-> (assoc fake-history-state :cmdline-input "a")
             (c/history-search-navigate true)
             (c/history-search-navigate true))
         (-> fake-history-state
             (assoc :cmdline-input "abc")
             (assoc :hist-cur-idx 0)
             (assoc :hist-cur-prefix "a"))))
  (is (= (-> (assoc fake-history-state :cmdline-input "a")
             (c/history-search-navigate true)
             (c/history-search-navigate true)
             (c/history-search-navigate true))
         (-> fake-history-state
             (assoc :cmdline-input "abc")
             (assoc :hist-cur-idx 0)
             (assoc :hist-cur-prefix "a"))))
  (is (= (-> (assoc fake-history-state :cmdline-input "a")
             (c/history-search-navigate true)
             (c/history-search-navigate false))
         (assoc fake-history-state :cmdline-input "a")))
  (is (= (-> (assoc fake-history-state :cmdline-input "a")
             (c/history-search-navigate true)
             (c/history-search-navigate true)
             (c/history-search-navigate false))
         (-> fake-history-state
             (assoc :cmdline-input "aac")
             (assoc :hist-cur-idx 1)
             (assoc :hist-cur-prefix "a"))))
  (is (= (-> (assoc fake-history-state :cmdline-input "aa")
             (c/history-search-navigate true))
         (-> fake-history-state
             (assoc :cmdline-input "aac")
             (assoc :hist-cur-idx 1)
             (assoc :hist-cur-prefix "aa")))))

(deftest align-sorted-seqs
  (is (= (c/align-sorted-seqs [] []) []))
  (is (= (c/align-sorted-seqs [1 2] []) [[1 nil] [2 nil]]))
  (is (= (c/align-sorted-seqs [] [1 2]) [[nil 1] [nil 2]]))
  (is (= (c/align-sorted-seqs [1 2 3] [1 2 4]) [[1 1] [2 2] [3 nil] [nil 4]]))
  (is (= (c/align-sorted-seqs [1 2] [0 3]) [[nil 0] [1 nil] [2 nil] [nil 3]])))
