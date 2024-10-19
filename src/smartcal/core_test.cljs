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

(deftest today
  ;; Please don't run this test near midnight.
  (is (= (:y (c/today)) (.getFullYear (js/Date.))))
  (is (= (:m (c/today)) (.getMonth (js/Date.))))
  (is (= (:d (c/today)) (.getDate (js/Date.))))
  (is (= (:weekday (c/today)) (.getDay (js/Date.)))))

(deftest actual-start
  (is (= (c/actual-start (c/ymd-to-date 2024 9 11))
         (c/ymd-to-date 2024 9 6)))
  (is (= (c/actual-start (c/ymd-to-date 2024 0 1))
         (c/ymd-to-date 2023 11 31))))

(deftest nd-weekday-of-month
  (is (= (c/nd-weekday-of-month 0 0 9 2024) 6))
  (is (= (c/nd-weekday-of-month 0 1 9 2024) 7))
  (is (= (c/nd-weekday-of-month 0 2 9 2024) 1))
  (is (= (c/nd-weekday-of-month 0 3 9 2024) 2))
  (is (= (c/nd-weekday-of-month 0 4 9 2024) 3))
  (is (= (c/nd-weekday-of-month 0 5 9 2024) 4))
  (is (= (c/nd-weekday-of-month 0 6 9 2024) 5))
  (is (= (c/nd-weekday-of-month 1 0 9 2024) 13))
  (is (= (c/nd-weekday-of-month 1 1 9 2024) 14))
  (is (= (c/nd-weekday-of-month 1 2 9 2024) 8))
  (is (= (c/nd-weekday-of-month 2 2 9 2024) 15))
  (is (= (c/nd-weekday-of-month 3 2 9 2024) 22))
  (is (= (c/nd-weekday-of-month 4 2 9 2024) 29))
  (is (thrown? js/Error (c/nd-weekday-of-month 5 2 9 2024))
      "occurrence out of bounds")
  (is (= (c/nd-weekday-of-month 4 9 9 2024) 29))
  (is (= (c/nd-weekday-of-month -1 2 9 2024) 29))
  (is (= (c/nd-weekday-of-month -2 2 9 2024) 22))
  (is (= (c/nd-weekday-of-month -3 2 9 2024) 15))
  (is (= (c/nd-weekday-of-month -4 2 9 2024) 8))
  (is (= (c/nd-weekday-of-month -5 2 9 2024) 1))
  (is (thrown? js/Error (c/nd-weekday-of-month -6 2 9 2024))
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
            [:recurring {:recur-type :month, :freq 1, :d #{18}}]]]]))
  (is (= (parses "add \"x\" every month on the 18th")
         [[:cmd
           [:add-cmd "x"
            [:recurring {:recur-type :month, :freq 1, :d #{18}}]]]]))
  (is (= (parses "add \"x\" every 2 months on the 18th")
         [[:cmd
           [:add-cmd "x"
            [:recurring {:recur-type :month, :freq 2, :d #{18}}]]]]))
  (is (= (parses "add \"x\" every first Monday of the month")
         [[:cmd
           [:add-cmd "x"
            [:recurring {:recur-type :month, :freq 1, :dow 1, :occ 0}]]]]))
  (is (= (parses "add \"x\" every first Monday of each month")
         [[:cmd
           [:add-cmd "x"
            [:recurring {:recur-type :month, :freq 1, :dow 1, :occ 0}]]]]))
  (is (= (parses "add \"x\" every month on the first Monday")
         [[:cmd
           [:add-cmd "x"
            [:recurring {:recur-type :month, :freq 1, :dow 1, :occ 0}]]]]))
  (is (= (parses "add \"x\" every month on the last Monday")
         [[:cmd
           [:add-cmd "x"
            [:recurring {:recur-type :month, :freq 1, :dow 1, :occ -1}]]]]))
  (is (= (parses "add \"x\" every 2 months on the last Monday")
         [[:cmd
           [:add-cmd "x"
            [:recurring {:recur-type :month, :freq 2, :dow 1, :occ -1}]]]]))
  (is (= (parses "add \"x\" every year on Dec 26")
         [[:cmd
           [:add-cmd "x"
            [:recurring {:recur-type :year, :freq 1, :m 11, :d 26}]]]]))
  (is (= (parses "add \"x\" every year on 26 Dec")
         [[:cmd
           [:add-cmd "x"
            [:recurring {:recur-type :year, :freq 1, :m 11, :d 26}]]]]))
  (is (= (parses "add \"x\" every year on 1226")
         [[:cmd
           [:add-cmd "x"
            [:recurring {:recur-type :year, :freq 1, :m 11, :d 26}]]]]))
  (is (= (parses "add \"x\" every 2 years on 26 Dec")
         [[:cmd
           [:add-cmd "x"
            [:recurring {:recur-type :year, :freq 2, :m 11, :d 26}]]]]))
  (is (= (parses "add \"x\" every year on last Friday of June")
         [[:cmd
           [:add-cmd "x"
            [:recurring
             {:recur-type :year, :freq 1, :occ -1, :m #{5}, :dow 5}]]]]))
  (is (=
        (parses
          "add \"fut exp\" every year on third Friday of March, June, Sep, Dec")
        [[:cmd
          [:add-cmd "fut exp"
           [:recurring
            {:recur-type :year, :freq 1, :occ 2, :m #{2 5 8 11}, :dow 5}]]]]))
  (is (= (parses "add \"x\" every year on the second Monday of Jan")
         [[:cmd
           [:add-cmd "x"
            [:recurring
             {:recur-type :year, :freq 1, :occ 1, :m #{0}, :dow 1}]]]]))
  (is (= (parses "add \"x\" every 4 years on the last Monday of February")
         [[:cmd
           [:add-cmd "x"
            [:recurring
             {:recur-type :year, :freq 4, :occ -1, :m #{1}, :dow 1}]]]]))
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
