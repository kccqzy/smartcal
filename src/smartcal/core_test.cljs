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
  (is (= (c/ymd-to-day-num 2024 9 100) 155236)))

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
  (is (= (:weekday (c/today)) (.getDay (js/Date.)))))

(deftest actual-start
  (is (= (c/actual-start (c/ymd-to-date 2024 9 11)) (c/ymd-to-date 2024 9 6)))
  (is (= (c/actual-start (c/ymd-to-date 2024 0 1)) (c/ymd-to-date 2023 11 31))))

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

(defn parses [s] (c/transform-parsed-dates (insta/parses c/cmdline-parser s)))

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
  (is (= (parses "add \"x\" on 20241001")
         [[:cmd [:add-cmd "x" [:single-occ (c/ymd-to-date 2024 9 1)]]]]))
  (is (= (parses "add \"x\" on Dec 1, 2024")
         [[:cmd [:add-cmd "x" [:single-occ (c/ymd-to-date 2024 11 1)]]]]))
  (is (= (parses "add \"x\" every day")
         [[:cmd [:add-cmd "x" [:recurring {:recur-type :day, :freq 1}]]]]))
  (is (= (parses "add \"grocery shopping\" every 3 days")
         [[:cmd
           [:add-cmd "grocery shopping"
            [:recurring {:recur-type :day, :freq 3}]]]]))
  (is (= (parses "add \"x\" every week on Mon")
         [[:cmd
           [:add-cmd "x"
            [:recurring {:recur-type :week, :freq 1, :dow #{1}}]]]]))
  (is (= (parses "add \"x\" every week on Monday")
         [[:cmd
           [:add-cmd "x"
            [:recurring {:recur-type :week, :freq 1, :dow #{1}}]]]]))
  (is (= (parses "add \"x\" every Monday")
         [[:cmd
           [:add-cmd "x"
            [:recurring {:recur-type :week, :freq 1, :dow #{1}}]]]]))
  (is (= (parses "add \"x\" every 3 weeks on Monday")
         [[:cmd
           [:add-cmd "x"
            [:recurring {:recur-type :week, :freq 3, :dow #{1}}]]]]))
  (is (= (parses "add \"x\" every week on Monday, Friday")
         [[:cmd
           [:add-cmd "x"
            [:recurring {:recur-type :week, :freq 1, :dow #{1 5}}]]]]))
  (is (= (parses "add \"x\" every Monday, Friday")
         [[:cmd
           [:add-cmd "x"
            [:recurring {:recur-type :week, :freq 1, :dow #{1 5}}]]]]))
  (is (= (parses "add \"x\" every 2 weeks on Monday , Fri")
         [[:cmd
           [:add-cmd "x"
            [:recurring {:recur-type :week, :freq 2, :dow #{1 5}}]]]]))
  (is (= (parses "add \"x\" every month on 18th")
         [[:cmd
           [:add-cmd "x"
            [:recurring
             {:recur-type :month, :freq 1, :day-selection :d, :d #{18}}]]]]))
  (is (= (parses "add \"x\" every month on the 18th")
         [[:cmd
           [:add-cmd "x"
            [:recurring
             {:recur-type :month, :freq 1, :day-selection :d, :d #{18}}]]]]))
  (is (= (parses "add \"x\" every month on the 18th, 22nd")
         [[:cmd
           [:add-cmd "x"
            [:recurring
             {:recur-type :month, :freq 1, :day-selection :d, :d #{18 22}}]]]]))
  (is (= (parses "add \"x\" every 2 months on the 18th")
         [[:cmd
           [:add-cmd "x"
            [:recurring
             {:recur-type :month, :freq 2, :day-selection :d, :d #{18}}]]]]))
  (is (= (parses "add \"x\" every 18th of the month")
         [[:cmd
           [:add-cmd "x"
            [:recurring
             {:recur-type :month, :freq 1, :day-selection :d, :d #{18}}]]]]))
  (is (= (parses "add \"x\" every first Monday of the month")
         [[:cmd
           [:add-cmd "x"
            [:recurring
             {:recur-type :month,
              :freq 1,
              :day-selection :dow,
              :dow 1,
              :occ #{0}}]]]]))
  (is (= (parses "add \"x\" every first Monday of each month")
         [[:cmd
           [:add-cmd "x"
            [:recurring
             {:recur-type :month,
              :freq 1,
              :day-selection :dow,
              :dow 1,
              :occ #{0}}]]]]))
  (is (= (parses "add \"x\" every first, third Monday of each month")
         [[:cmd
           [:add-cmd "x"
            [:recurring
             {:recur-type :month,
              :freq 1,
              :day-selection :dow,
              :dow 1,
              :occ #{0 2}}]]]]))
  (is (= (parses "add \"x\" every month on the first Monday")
         [[:cmd
           [:add-cmd "x"
            [:recurring
             {:recur-type :month,
              :freq 1,
              :day-selection :dow,
              :dow 1,
              :occ #{0}}]]]]))
  (is (= (parses "add \"x\" every month on the last Monday")
         [[:cmd
           [:add-cmd "x"
            [:recurring
             {:recur-type :month,
              :freq 1,
              :day-selection :dow,
              :dow 1,
              :occ #{-1}}]]]]))
  (is (= (parses "add \"x\" every 2 months on the last Monday")
         [[:cmd
           [:add-cmd "x"
            [:recurring
             {:recur-type :month,
              :freq 2,
              :day-selection :dow,
              :dow 1,
              :occ #{-1}}]]]]))
  (is (=
        (parses "add \"x\" every year on Dec 26")
        [[:cmd
          [:add-cmd "x"
           [:recurring
            {:recur-type :year, :freq 1, :day-selection :md, :m 11, :d 26}]]]]))
  (is (=
        (parses "add \"x\" every year on 26 Dec")
        [[:cmd
          [:add-cmd "x"
           [:recurring
            {:recur-type :year, :freq 1, :day-selection :md, :m 11, :d 26}]]]]))
  (is (=
        (parses "add \"x\" every year on 1226")
        [[:cmd
          [:add-cmd "x"
           [:recurring
            {:recur-type :year, :freq 1, :day-selection :md, :m 11, :d 26}]]]]))
  (is (=
        (parses "add \"x\" every 2 years on 26 Dec")
        [[:cmd
          [:add-cmd "x"
           [:recurring
            {:recur-type :year, :freq 2, :day-selection :md, :m 11, :d 26}]]]]))
  (is (= (parses "add \"x\" every year on last Friday of June")
         [[:cmd
           [:add-cmd "x"
            [:recurring
             {:recur-type :year,
              :freq 1,
              :day-selection :occ-dow-month,
              :occ #{-1},
              :m #{5},
              :dow 5}]]]]))
  (is (=
        (parses
          "add \"fut exp\" every year on third Friday of March, June, Sep, Dec")
        [[:cmd
          [:add-cmd "fut exp"
           [:recurring
            {:recur-type :year,
             :freq 1,
             :day-selection :occ-dow-month,
             :occ #{2},
             :m #{2 5 8 11},
             :dow 5}]]]]))
  (is (= (parses "add \"x\" every year on the second Monday of Jan")
         [[:cmd
           [:add-cmd "x"
            [:recurring
             {:recur-type :year,
              :freq 1,
              :day-selection :occ-dow-month,
              :occ #{1},
              :m #{0},
              :dow 1}]]]]))
  (is (= (parses
           "add \"x\" every year on the first, third Wednesday of Jan, Jul")
         [[:cmd
           [:add-cmd "x"
            [:recurring
             {:recur-type :year,
              :freq 1,
              :day-selection :occ-dow-month,
              :occ #{0 2},
              :m #{0 6},
              :dow 3}]]]]))
  (is (= (parses "add \"x\" every 4 years on the last Monday of February")
         [[:cmd
           [:add-cmd "x"
            [:recurring
             {:recur-type :year,
              :freq 4,
              :day-selection :occ-dow-month,
              :occ #{-1},
              :m #{1},
              :dow 1}]]]]))
  (is (= (parses "add \"x\" every week on Mon from 20200101 ")
         [[:cmd
           [:add-cmd "x"
            [:recurring
             {:recur-type :week,
              :freq 1,
              :dow #{1},
              :recur-start (c/ymd-to-date 2020 0 1)}]]]]))
  (is (= (parses "add \"x\" every week on Mon until 20200101 ")
         [[:cmd
           [:add-cmd "x"
            [:recurring
             {:recur-type :week,
              :freq 1,
              :dow #{1},
              :recur-end (c/ymd-to-date 2020 0 1)}]]]]))
  (is (= (parses "add \"x\" every week on Mon from 20200101 until Feb 1, 2020")
         [[:cmd
           [:add-cmd "x"
            [:recurring
             {:recur-type :week,
              :freq 1,
              :dow #{1},
              :recur-start (c/ymd-to-date 2020 0 1),
              :recur-end (c/ymd-to-date 2020 1 1)}]]]])))

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
  [{:name "2023 New Year", :single-occ (c/ymd-to-date 2023 0 1)}
   {:name "2024 New Year", :single-occ (c/ymd-to-date 2024 0 1)}
   {:name "New Year",
    :recurring {:recur-type :year, :freq 1, :day-selection :md, :m 0, :d 1}}
   {:name "Good Day",
    :recurring {:recur-type :year, :freq 1, :day-selection :md, :m 1, :d 1}}
   {:name "Very Good Day",
    :recurring {:recur-type :year,
                :freq 1,
                :day-selection :occ-dow-month,
                :m #{0},
                :dow 0,
                :occ #{1 3}}}])

(deftest get-visible-events
  (is (= (c/get-visible-events (c/ymd-to-date 2024 0 1)
                               (c/ymd-to-date 2024 1 1)
                               example-events)
         [[(c/ymd-to-date 2024 0 1) "2024 New Year"]
          [(c/ymd-to-date 2024 0 1) "New Year"]
          [(c/ymd-to-date 2024 0 14) "Very Good Day"]
          [(c/ymd-to-date 2024 0 28) "Very Good Day"]])))

(deftest get-days-with-events
  (is (= (c/get-days-with-events (c/ymd-to-date 2024 0 1)
                                 (c/ymd-to-date 2024 1 1)
                                 example-events)
         {(c/ymd-to-date 2024 0 1) ["2024 New Year" "New Year"],
          (c/ymd-to-date 2024 0 14) ["Very Good Day"],
          (c/ymd-to-date 2024 0 28) ["Very Good Day"]})))
