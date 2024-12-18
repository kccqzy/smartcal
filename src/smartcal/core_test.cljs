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

(deftest week-num
  (is (= (c/week-num c/epoch) 0))
  (is (= (c/week-num (c/ymd-to-date 1600 0 2)) 1))
  (is (= (c/week-num (c/ymd-to-date 1600 0 3)) 1))
  (is (= (c/week-num (c/ymd-to-date 1600 0 8)) 1))
  (is (= (c/week-num (c/ymd-to-date 1600 0 9)) 2)))

(deftest week-num-day-to-date
  (is (= (c/week-num-day-to-date 0 6) c/epoch))
  (is (= (c/week-num-day-to-date 1 0) (c/ymd-to-date 1600 0 2)))
  (is (= (c/week-num-day-to-date 1 6) (c/ymd-to-date 1600 0 8)))
  (is (let [date (c/ymd-to-date 2024 10 10)]
        (= (c/week-num-day-to-date (c/week-num date) (:dow date)) date))))

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
  (is (= (c/modulo-remainder-seq 7 0 0 21) [0 7 14]))
  (is (= (c/modulo-remainder-seq 7 0 0 22) [0 7 14 21]))
  (is (= (c/modulo-remainder-seq 7 6 0 20) [6 13]))
  (is (= (c/modulo-remainder-seq 7 6 0 21) [6 13 20]))
  (is (= (c/modulo-remainder-seq 2 3 4 8) [5 7]))
  (is (= (c/modulo-remainder-seq 1 3 4 8) [4 5 6 7]))
  (is (chunked-seq? (c/modulo-remainder-seq 7 6 0 21))))

(deftest modulo-remainder-rseq
  (is (= (c/modulo-remainder-rseq 7 0 0 0) []))
  (is (= (c/modulo-remainder-rseq 7 71 0 10) [8 1]) "val outside range")
  (is (= (c/modulo-remainder-rseq 7 -6 0 10) [8 1]) "val negative")
  (is (= (c/modulo-remainder-rseq 7 6 6 6) []) "from = to")
  (is (= (c/modulo-remainder-rseq 7 0 1 0) []) "from > to")
  (is (= (c/modulo-remainder-rseq 7 6 6 7) [6]))
  (is (= (c/modulo-remainder-rseq 7 0 0 21) [14 7 0]))
  (is (= (c/modulo-remainder-rseq 7 0 0 22) [21 14 7 0]))
  (is (= (c/modulo-remainder-rseq 7 6 0 20) [13 6]))
  (is (= (c/modulo-remainder-rseq 7 6 0 21) [20 13 6]))
  (is (= (c/modulo-remainder-rseq 2 3 4 8) [7 5]))
  (is (= (c/modulo-remainder-rseq 1 3 4 8) [7 6 5 4]))
  (is (chunked-seq? (c/modulo-remainder-rseq 7 6 0 21))))

(deftest all-nd-weekday-of-month
  (is (= (c/all-nd-weekdays-of-month #{0} 0 9 2024) [(c/ymd-to-date 2024 9 6)]))
  (is (= (c/all-nd-weekdays-of-month #{0} 1 9 2024) [(c/ymd-to-date 2024 9 7)]))
  (is (= (c/all-nd-weekdays-of-month #{0} 2 9 2024) [(c/ymd-to-date 2024 9 1)]))
  (is (= (c/all-nd-weekdays-of-month #{0} 3 9 2024) [(c/ymd-to-date 2024 9 2)]))
  (is (= (c/all-nd-weekdays-of-month #{0} 4 9 2024) [(c/ymd-to-date 2024 9 3)]))
  (is (= (c/all-nd-weekdays-of-month #{0} 5 9 2024) [(c/ymd-to-date 2024 9 4)]))
  (is (= (c/all-nd-weekdays-of-month #{0} 6 9 2024) [(c/ymd-to-date 2024 9 5)]))
  (is (= (c/all-nd-weekdays-of-month #{1} 0 9 2024)
         [(c/ymd-to-date 2024 9 13)]))
  (is (= (c/all-nd-weekdays-of-month #{1} 1 9 2024)
         [(c/ymd-to-date 2024 9 14)]))
  (is (= (c/all-nd-weekdays-of-month #{1} 2 9 2024) [(c/ymd-to-date 2024 9 8)]))
  (is (= (c/all-nd-weekdays-of-month #{2} 2 9 2024)
         [(c/ymd-to-date 2024 9 15)]))
  (is (= (c/all-nd-weekdays-of-month #{3} 2 9 2024)
         [(c/ymd-to-date 2024 9 22)]))
  (is (= (c/all-nd-weekdays-of-month #{4} 2 9 2024)
         [(c/ymd-to-date 2024 9 29)]))
  (is (empty? (c/all-nd-weekdays-of-month #{5} 2 9 2024))
      "occurrence out of bounds")
  (is (= (c/all-nd-weekdays-of-month #{4} 9 9 2024)
         [(c/ymd-to-date 2024 9 29)]))
  (is (= (c/all-nd-weekdays-of-month #{-1} 2 9 2024)
         [(c/ymd-to-date 2024 9 29)]))
  (is (= (c/all-nd-weekdays-of-month #{-2} 2 9 2024)
         [(c/ymd-to-date 2024 9 22)]))
  (is (= (c/all-nd-weekdays-of-month #{-3} 2 9 2024)
         [(c/ymd-to-date 2024 9 15)]))
  (is (= (c/all-nd-weekdays-of-month #{-4} 2 9 2024)
         [(c/ymd-to-date 2024 9 8)]))
  (is (= (c/all-nd-weekdays-of-month #{-5} 2 9 2024)
         [(c/ymd-to-date 2024 9 1)]))
  (is (empty? (c/all-nd-weekdays-of-month #{-6} 2 9 2024))
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

(def unit-test-today (c/ymd-to-date 2024 11 1))

(defn- parses
  ([s] (parses s unit-test-today))
  ([s today]
   (c/transform-parsed-with-specific-today (insta/parses c/cmdline-parser s)
                                           today)))

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
  (is (= (parses "add \"x\" on today()")
         [[:cmd [:add-cmd (c/event-from-single-occ "x" unit-test-today)]]]))
  (is (= (parses "add \"x\" every day")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec
              "x"
              {:recur-type :day, :freq 1, :recur-start unit-test-today})]]]))
  (is (= (parses "add \"grocery shopping\" every 3 days")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec
              "grocery shopping"
              {:recur-type :day, :freq 3, :recur-start unit-test-today})]]]))
  (is (= (parses "add \"grocery shopping\" every (3) days")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec
              "grocery shopping"
              {:recur-type :day, :freq 3, :recur-start unit-test-today})]]]))
  (is (= (parses "add \"grocery shopping\" every (1+1*2) days")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec
              "grocery shopping"
              {:recur-type :day, :freq 3, :recur-start unit-test-today})]]]))
  (is (= (parses "add \"grocery shopping\" every(3)days")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec
              "grocery shopping"
              {:recur-type :day, :freq 3, :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every week on Mon")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :week,
                                      :freq 1,
                                      :dow #{1},
                                      :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every week on Monday")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :week,
                                      :freq 1,
                                      :dow #{1},
                                      :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every Monday")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :week,
                                      :freq 1,
                                      :dow #{1},
                                      :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every 3 weeks on Monday")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :week,
                                      :freq 3,
                                      :dow #{1},
                                      :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every(3)weeks on Monday")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :week,
                                      :freq 3,
                                      :dow #{1},
                                      :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every week on Monday, Friday")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :week,
                                      :freq 1,
                                      :dow #{1 5},
                                      :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every week on Monday  Wednesday Friday")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :week,
                                      :freq 1,
                                      :dow #{1 3 5},
                                      :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every week on Monday, Wednesday Friday") [])
      "cannot mix comma-separated and space-separated values")
  (is (= (parses "add \"x\" every Monday, Friday")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :week,
                                      :freq 1,
                                      :dow #{1 5},
                                      :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every 2 weeks on Monday , Fri")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :week,
                                      :freq 2,
                                      :dow #{1 5},
                                      :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every month on 18th")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :month,
                                      :freq 1,
                                      :day-selection :d,
                                      :d #{18},
                                      :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every month on the 18th")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :month,
                                      :freq 1,
                                      :day-selection :d,
                                      :d #{18},
                                      :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every month on the 18th, 22nd")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :month,
                                      :freq 1,
                                      :day-selection :d,
                                      :d #{18 22},
                                      :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every month on the 18th 22nd")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :month,
                                      :freq 1,
                                      :day-selection :d,
                                      :d #{18 22},
                                      :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every 2 months on the 18th")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :month,
                                      :freq 2,
                                      :day-selection :d,
                                      :d #{18},
                                      :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every(2) months on the 18th")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :month,
                                      :freq 2,
                                      :day-selection :d,
                                      :d #{18},
                                      :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every 18th of the month")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :month,
                                      :freq 1,
                                      :day-selection :d,
                                      :d #{18},
                                      :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every first Monday of the month")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :month,
                                      :freq 1,
                                      :day-selection :dow,
                                      :dow 1,
                                      :occ #{0},
                                      :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every first Monday of each month")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :month,
                                      :freq 1,
                                      :day-selection :dow,
                                      :dow 1,
                                      :occ #{0},
                                      :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every first, third Monday of each month")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :month,
                                      :freq 1,
                                      :day-selection :dow,
                                      :dow 1,
                                      :occ #{0 2},
                                      :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every month on the first Monday")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :month,
                                      :freq 1,
                                      :day-selection :dow,
                                      :dow 1,
                                      :occ #{0},
                                      :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every month on the last Monday")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :month,
                                      :freq 1,
                                      :day-selection :dow,
                                      :dow 1,
                                      :occ #{-1},
                                      :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every 2 months on the last Monday")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :month,
                                      :freq 2,
                                      :day-selection :dow,
                                      :dow 1,
                                      :occ #{-1},
                                      :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every year on Dec 26")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :year,
                                      :freq 1,
                                      :day-selection :md,
                                      :m 11,
                                      :d 26,
                                      :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every year on 26 Dec")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :year,
                                      :freq 1,
                                      :day-selection :md,
                                      :m 11,
                                      :d 26,
                                      :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every 2 years on 26 Dec")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :year,
                                      :freq 2,
                                      :day-selection :md,
                                      :m 11,
                                      :d 26,
                                      :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every (2)years on 26 Dec")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :year,
                                      :freq 2,
                                      :day-selection :md,
                                      :m 11,
                                      :d 26,
                                      :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every year on last Friday of June")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :year,
                                      :freq 1,
                                      :day-selection :occ-dow-month,
                                      :occ #{-1},
                                      :m #{5},
                                      :dow 5,
                                      :recur-start unit-test-today})]]]))
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
                                     :dow 5,
                                     :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every year on the second Monday of Jan")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :year,
                                      :freq 1,
                                      :day-selection :occ-dow-month,
                                      :occ #{1},
                                      :m #{0},
                                      :dow 1,
                                      :recur-start unit-test-today})]]]))
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
                                      :dow 3,
                                      :recur-start unit-test-today})]]]))
  (is (= (parses "add \"x\" every 4 years on the last Monday of February")
         [[:cmd
           [:add-cmd
            (c/event-from-single-rec "x"
                                     {:recur-type :year,
                                      :freq 4,
                                      :day-selection :occ-dow-month,
                                      :occ #{-1},
                                      :m #{1},
                                      :dow 1,
                                      :recur-start unit-test-today})]]]))
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
                                      :recur-start unit-test-today,
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
  (is (= (c/recurrent-event-occurrences
           {:recur-type :day, :freq 1, :recur-start (c/ymd-to-date 2024 9 18)}
           (c/ymd-to-date 2024 9 18)
           (c/ymd-to-date 2024 9 25))
         [(c/ymd-to-date 2024 9 18) (c/ymd-to-date 2024 9 19)
          (c/ymd-to-date 2024 9 20) (c/ymd-to-date 2024 9 21)
          (c/ymd-to-date 2024 9 22) (c/ymd-to-date 2024 9 23)
          (c/ymd-to-date 2024 9 24)]))
  (is (= (c/recurrent-event-occurrences
           {:recur-type :day, :freq 3, :recur-start (c/ymd-to-date 2024 9 18)}
           (c/ymd-to-date 2024 9 18)
           (c/ymd-to-date 2024 10 3))
         [(c/ymd-to-date 2024 9 18) (c/ymd-to-date 2024 9 21)
          (c/ymd-to-date 2024 9 24) (c/ymd-to-date 2024 9 27)
          (c/ymd-to-date 2024 9 30) (c/ymd-to-date 2024 10 2)]))
  (is (= (c/recurrent-event-occurrences
           {:recur-type :day, :freq 30, :recur-start (c/ymd-to-date 2024 0 1)}
           (c/ymd-to-date 2023 0 1)
           (c/ymd-to-date 2024 4 2))
         [(c/ymd-to-date 2024 0 1) (c/ymd-to-date 2024 0 31)
          (c/ymd-to-date 2024 2 1) (c/ymd-to-date 2024 2 31)
          (c/ymd-to-date 2024 3 30)]))
  (is (= (c/recurrent-event-occurrences {:recur-type :week,
                                         :freq 1,
                                         :dow #{1},
                                         :recur-start (c/ymd-to-date 2024 0 1)}
                                        (c/ymd-to-date 2024 0 1)
                                        (c/ymd-to-date 2024 1 1))
         [(c/ymd-to-date 2024 0 1) (c/ymd-to-date 2024 0 8)
          (c/ymd-to-date 2024 0 15) (c/ymd-to-date 2024 0 22)
          (c/ymd-to-date 2024 0 29)]))
  (is (= (c/recurrent-event-occurrences {:recur-type :week,
                                         :freq 1,
                                         :dow #{2},
                                         :recur-start (c/ymd-to-date 2024 0 1)}
                                        (c/ymd-to-date 2024 0 1)
                                        (c/ymd-to-date 2024 1 1))
         [(c/ymd-to-date 2024 0 2) (c/ymd-to-date 2024 0 9)
          (c/ymd-to-date 2024 0 16) (c/ymd-to-date 2024 0 23)
          (c/ymd-to-date 2024 0 30)]))
  (is (= (c/recurrent-event-occurrences {:recur-type :week,
                                         :freq 1,
                                         :dow #{2},
                                         :recur-start
                                           ;; Recurrence started long
                                           ;; before query window
                                           (c/ymd-to-date 2020 0 1)}
                                        (c/ymd-to-date 2024 0 1)
                                        (c/ymd-to-date 2024 1 1))
         [(c/ymd-to-date 2024 0 2) (c/ymd-to-date 2024 0 9)
          (c/ymd-to-date 2024 0 16) (c/ymd-to-date 2024 0 23)
          (c/ymd-to-date 2024 0 30)]))
  (is (= (c/recurrent-event-occurrences {:recur-type :week,
                                         :freq 1,
                                         :dow #{1 5},
                                         :recur-start (c/ymd-to-date 2024 0 5)}
                                        ;; Query window before recurrence
                                        ;; start.
                                        (c/ymd-to-date 2020 0 1)
                                        (c/ymd-to-date 2024 0 20))
         [(c/ymd-to-date 2024 0 5) (c/ymd-to-date 2024 0 8)
          (c/ymd-to-date 2024 0 12) (c/ymd-to-date 2024 0 15)
          (c/ymd-to-date 2024 0 19)]))
  (is (= (c/recurrent-event-occurrences {:recur-type :week,
                                         :freq 2,
                                         :dow #{2},
                                         :recur-start (c/ymd-to-date 2024 0 1)}
                                        (c/ymd-to-date 2024 0 1)
                                        (c/ymd-to-date 2024 1 1))
         [(c/ymd-to-date 2024 0 2) (c/ymd-to-date 2024 0 16)
          (c/ymd-to-date 2024 0 30)]))
  (is (= (c/recurrent-event-occurrences {:recur-type :week,
                                         :freq 4,
                                         :dow #{1},
                                         :recur-start (c/ymd-to-date 2024 0 5)}
                                        (c/ymd-to-date 2024 0 1)
                                        (c/ymd-to-date 2024 1 28))
         [(c/ymd-to-date 2024 0 29) (c/ymd-to-date 2024 1 26)])
      "first week has no more occurrences")
  (is (= (c/recurrent-event-occurrences {:recur-type :week,
                                         :freq 2,
                                         :dow #{2 6},
                                         :recur-start (c/ymd-to-date 2024 0 1)}
                                        (c/ymd-to-date 2024 0 1)
                                        (c/ymd-to-date 2024 1 1))
         [(c/ymd-to-date 2024 0 2) (c/ymd-to-date 2024 0 6)
          (c/ymd-to-date 2024 0 16) (c/ymd-to-date 2024 0 20)
          (c/ymd-to-date 2024 0 30)]))
  (is (= (c/recurrent-event-occurrences {:recur-type :week,
                                         :freq 2,
                                         :dow #{1 5},
                                         :recur-start (c/ymd-to-date 2024 0 5)}
                                        (c/ymd-to-date 2020 0 1)
                                        (c/ymd-to-date 2024 0 20))
         [(c/ymd-to-date 2024 0 5) (c/ymd-to-date 2024 0 15)
          (c/ymd-to-date 2024 0 19)]))
  (is (= (c/recurrent-event-occurrences {:recur-type :month,
                                         :freq 1,
                                         :day-selection :d,
                                         :d #{1},
                                         :recur-start (c/ymd-to-date 2024 0 1)}
                                        (c/ymd-to-date 2020 0 1)
                                        (c/ymd-to-date 2024 5 1))
         [(c/ymd-to-date 2024 0 1) (c/ymd-to-date 2024 1 1)
          (c/ymd-to-date 2024 2 1) (c/ymd-to-date 2024 3 1)
          (c/ymd-to-date 2024 4 1)]))
  (is (= (c/recurrent-event-occurrences {:recur-type :month,
                                         :freq 2,
                                         :day-selection :d,
                                         :d #{1},
                                         :recur-start (c/ymd-to-date 2024 0 1)}
                                        (c/ymd-to-date 2020 0 1)
                                        (c/ymd-to-date 2024 5 2))
         [(c/ymd-to-date 2024 0 1) (c/ymd-to-date 2024 2 1)
          (c/ymd-to-date 2024 4 1)]))
  (is (= (c/recurrent-event-occurrences {:recur-type :month,
                                         :freq 3,
                                         :day-selection :d,
                                         :d #{8 18 28},
                                         :recur-start (c/ymd-to-date 2024 0 1)}
                                        (c/ymd-to-date 2020 0 1)
                                        (c/ymd-to-date 2024 7 1))
         [(c/ymd-to-date 2024 0 8) (c/ymd-to-date 2024 0 18)
          (c/ymd-to-date 2024 0 28) (c/ymd-to-date 2024 3 8)
          (c/ymd-to-date 2024 3 18) (c/ymd-to-date 2024 3 28)
          (c/ymd-to-date 2024 6 8) (c/ymd-to-date 2024 6 18)
          (c/ymd-to-date 2024 6 28)]))
  (is (= (c/recurrent-event-occurrences {:recur-type :month,
                                         :freq 1,
                                         :day-selection :d,
                                         :d #{31},
                                         :recur-start (c/ymd-to-date 2024 0 1)}
                                        (c/ymd-to-date 2020 0 1)
                                        (c/ymd-to-date 2025 0 1))
         [(c/ymd-to-date 2024 0 31) (c/ymd-to-date 2024 2 31)
          (c/ymd-to-date 2024 4 31) (c/ymd-to-date 2024 6 31)
          (c/ymd-to-date 2024 7 31) (c/ymd-to-date 2024 9 31)
          (c/ymd-to-date 2024 11 31)])
      "skips over non-existent 31st days")
  (is (= (c/recurrent-event-occurrences {:recur-type :month,
                                         :freq 2,
                                         :day-selection :d,
                                         :d #{31},
                                         :recur-start (c/ymd-to-date 2024 0 1)}
                                        (c/ymd-to-date 2020 0 1)
                                        (c/ymd-to-date 2025 0 1))
         [(c/ymd-to-date 2024 0 31) (c/ymd-to-date 2024 2 31)
          (c/ymd-to-date 2024 4 31) (c/ymd-to-date 2024 6 31)])
      "skips over non-existent 31st days and also every second month")
  (is (= (c/select-dates-from-month-recur
           {:recur-type :month, :day-selection :dow, :dow 1, :occ #{0}}
           [(c/month-num {:y 2024, :m 0})])
         [(c/ymd-to-date 2024 0 1)]))
  (is (= (c/recurrent-event-occurrences {:recur-type :month,
                                         :freq 1,
                                         :day-selection :dow,
                                         :dow 1,
                                         :occ #{0},
                                         :recur-start (c/ymd-to-date 2024 0 1)}
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
                                         :occ #{0 -1},
                                         :recur-start (c/ymd-to-date 2024 0 1)}
                                        (c/ymd-to-date 2020 0 1)
                                        (c/ymd-to-date 2024 3 1))
         [(c/ymd-to-date 2024 0 1) (c/ymd-to-date 2024 0 29)
          (c/ymd-to-date 2024 1 5) (c/ymd-to-date 2024 1 26)
          (c/ymd-to-date 2024 2 4) (c/ymd-to-date 2024 2 25)])
      "finds first and last Monday of each month")
  (is (= (c/recurrent-event-occurrences {:recur-type :month,
                                         :freq 1,
                                         :day-selection :dow,
                                         :dow 1,
                                         :occ #{0 4},
                                         :recur-start (c/ymd-to-date 2024 0 1)}
                                        (c/ymd-to-date 2020 0 1)
                                        (c/ymd-to-date 2024 4 1))
         [(c/ymd-to-date 2024 0 1) (c/ymd-to-date 2024 0 29)
          (c/ymd-to-date 2024 1 5) (c/ymd-to-date 2024 2 4)
          (c/ymd-to-date 2024 3 1) (c/ymd-to-date 2024 3 29)])
      "finds first and fifth Monday of each month")
  (is (= (c/recurrent-event-occurrences {:recur-type :month,
                                         :freq 2,
                                         :day-selection :dow,
                                         :dow 1,
                                         :occ #{0 4},
                                         :recur-start (c/ymd-to-date 2024 0 1)}
                                        (c/ymd-to-date 2020 0 1)
                                        (c/ymd-to-date 2024 6 30))
         [(c/ymd-to-date 2024 0 1) (c/ymd-to-date 2024 0 29)
          (c/ymd-to-date 2024 2 4) (c/ymd-to-date 2024 4 6)
          (c/ymd-to-date 2024 6 1) (c/ymd-to-date 2024 6 29)])
      "finds first and fifth Monday of every 2 months")
  (is (= (c/recurrent-event-occurrences {:recur-type :month,
                                         :freq 13,
                                         :day-selection :dow,
                                         :dow 0,
                                         :occ #{-1},
                                         :recur-start (c/ymd-to-date 2020 0 1)}
                                        (c/ymd-to-date 2020 0 1)
                                        (c/ymd-to-date 2024 6 30))
         [(c/ymd-to-date 2020 0 26) (c/ymd-to-date 2021 1 28)
          (c/ymd-to-date 2022 2 27) (c/ymd-to-date 2023 3 30)
          (c/ymd-to-date 2024 4 26)])
      "finds last Sunday every 13 months")
  (is (= (c/recurrent-event-occurrences {:recur-type :year,
                                         :freq 1,
                                         :day-selection :md,
                                         :m 11,
                                         :d 25,
                                         :recur-start (c/ymd-to-date 2020 0 1)}
                                        (c/ymd-to-date 2019 0 1)
                                        (c/ymd-to-date 2025 0 1))
         [(c/ymd-to-date 2020 11 25) (c/ymd-to-date 2021 11 25)
          (c/ymd-to-date 2022 11 25) (c/ymd-to-date 2023 11 25)
          (c/ymd-to-date 2024 11 25)])
      "finds fixed date every year")
  (is (= (c/recurrent-event-occurrences {:recur-type :year,
                                         :freq 1,
                                         :day-selection :md,
                                         :m 1,
                                         :d 29,
                                         :recur-start (c/ymd-to-date 1995 0 1)}
                                        (c/ymd-to-date 1995 0 1)
                                        (c/ymd-to-date 2010 0 1))
         [(c/ymd-to-date 1996 1 29) (c/ymd-to-date 2000 1 29)
          (c/ymd-to-date 2004 1 29) (c/ymd-to-date 2008 1 29)])
      "finds Feb 29 every four years")
  (is (= (c/recurrent-event-occurrences {:recur-type :year,
                                         :freq 5,
                                         :day-selection :md,
                                         :m 6,
                                         :d 30,
                                         :recur-start (c/ymd-to-date 1995 0 1)}
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
                                         :m #{0 1},
                                         :recur-start (c/ymd-to-date 2020 0 1)}
                                        (c/ymd-to-date 2020 0 1)
                                        (c/ymd-to-date 2022 0 1))
         [(c/ymd-to-date 2020 0 5) (c/ymd-to-date 2020 1 2)
          (c/ymd-to-date 2021 0 3) (c/ymd-to-date 2021 1 7)])))

(def example-events
  [(c/event-from-single-occ "2023 New Year" (c/ymd-to-date 2023 0 1))
   (c/event-from-single-occ "2024 New Year" (c/ymd-to-date 2024 0 1))
   (c/event-from-single-rec "New Year"
                            {:recur-type :year,
                             :freq 1,
                             :day-selection :md,
                             :m 0,
                             :d 1,
                             :recur-start c/epoch})
   (c/event-from-single-rec "Good Day"
                            {:recur-type :year,
                             :freq 1,
                             :day-selection :md,
                             :m 1,
                             :d 1,
                             :recur-start c/epoch})
   (c/event-from-single-rec "Very Good Day"
                            {:recur-type :year,
                             :freq 1,
                             :day-selection :occ-dow-month,
                             :m #{0},
                             :dow 0,
                             :occ #{1 3},
                             :recur-start c/epoch})
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
  (is (= (c/format-recur-pat {:recur-type :day, :freq 1})
         "every day from today"))
  (is (= (c/format-recur-pat {:recur-type :day, :freq 3})
         "every 3 days from today"))
  (is (= (c/format-recur-pat {:recur-type :day, :freq 300})
         "every 300 days from today"))
  (is (= (c/format-recur-pat {:recur-type :day,
                              :freq 300,
                              :recur-start (c/ymd-to-date 2020 10 1)})
         "every 300 days from Nov 1, 2020"))
  (is (= (c/format-recur-pat
           {:recur-type :day, :freq 300, :recur-start c/epoch})
         "every 300 days since time immemorial"))
  (is (= (c/format-recur-pat {:recur-type :week, :freq 1, :dow #{1}})
         "every week on Mon from this week"))
  (is (= (c/format-recur-pat {:recur-type :week, :freq 1, :dow #{1 3 5}})
         "every week on Mon, Wed, Fri from this week"))
  (is (= (c/format-recur-pat {:recur-type :week, :freq 1, :dow #{1 3 5}})
         "every week on Mon, Wed, Fri from this week"))
  (is (= (c/format-recur-pat
           {:recur-type :month, :freq 1, :day-selection :d, :d #{1 3 5}})
         "every month on the 1st, 3rd, 5th from this month"))
  (is (= (c/format-recur-pat
           {:recur-type :month, :freq 2, :day-selection :d, :d #{1 11 21 31}})
         "every 2 months on the 1st, 11th, 21st, 31st from this month"))
  (is (=
        (c/format-recur-pat
          {:recur-type :month, :freq 2, :day-selection :dow, :dow 1, :occ #{0}})
        "every 2 months on the first Monday from this month"))
  (is
    (=
      (c/format-recur-pat
        {:recur-type :month, :freq 2, :day-selection :dow, :dow 1, :occ #{0 2}})
      "every 2 months on the first, third Monday from this month"))
  (is (= (c/format-recur-pat
           {:recur-type :year, :freq 1, :day-selection :md, :m 1, :d 2})
         "every year on Feb 2 from this year"))
  (is (= (c/format-recur-pat {:recur-type :year,
                              :freq 1,
                              :day-selection :occ-dow-month,
                              :occ #{-1},
                              :m #{5},
                              :dow 5})
         "every year on the last Friday of June from this year"))
  (is (= (c/format-recur-pat {:recur-type :year,
                              :freq 1,
                              :day-selection :occ-dow-month,
                              :occ #{-1},
                              :m #{5},
                              :dow 5,
                              :recur-start c/epoch})
         "every year on the last Friday of June since time immemorial")))

(deftest format-recur-pat-with-single-period-exception
  (is (= (c/format-recur-pat-with-single-period-exception
           {:recur-type :day,
            :freq 10,
            :recur-start (c/ymd-to-date 2024 11 1),
            :recur-end (c/ymd-to-date 2024 11 5)})
         "On Dec 1, 2024"))
  (is (= (c/format-recur-pat-with-single-period-exception
           {:recur-type :day,
            :freq 10,
            :recur-start (c/ymd-to-date 2024 11 1),
            :recur-end (c/ymd-to-date 2024 11 12)})
         "Repeating every 10 days from Dec 1, 2024 until Dec 12, 2024"))
  (is (= (c/format-recur-pat-with-single-period-exception
           {:recur-type :week,
            :freq 1,
            :dow #{6},
            :recur-start (c/ymd-to-date 2024 11 9),
            :recur-end (c/ymd-to-date 2024 11 16)})
         "During the week of Dec 8, 2024 on Sat (Dec 14, 2024)"))
  (is
    (=
      (c/format-recur-pat-with-single-period-exception
        {:recur-type :week,
         :freq 1,
         :dow #{5 6},
         :recur-start (c/ymd-to-date 2024 11 9),
         :recur-end (c/ymd-to-date 2024 11 18)})
      "During the week of Dec 8, 2024 on Fri (Dec 13, 2024), Sat (Dec 14, 2024)"))
  (is (= (c/format-recur-pat-with-single-period-exception
           {:recur-type :week,
            :freq 1,
            :dow #{6},
            :recur-start (c/ymd-to-date 2024 11 9),
            :recur-end (c/ymd-to-date 2025 11 16)})
         "Repeating every week on Sat from Dec 9, 2024 until Dec 16, 2025"))
  (is (= (c/format-recur-pat-with-single-period-exception
           {:recur-type :month,
            :freq 1,
            :day-selection :d,
            :d #{20 10},
            :recur-start (c/ymd-to-date 2024 11 2),
            :recur-end (c/ymd-to-date 2024 11 25)})
         "During the month of Dec 2024 on the 10th, 20th"))
  (is (= (c/format-recur-pat-with-single-period-exception
           {:recur-type :month,
            :freq 1,
            :day-selection :dow,
            :dow 3,
            :occ #{-1 0 3},
            :recur-start (c/ymd-to-date 2024 11 2),
            :recur-end (c/ymd-to-date 2024 11 25)})
         ;; TODO here last and fourth are redundant; should we handle this?
         "During the month of Dec 2024 on the last, first, fourth Wednesday")))

(deftest format-event
  (is (= (c/format-event (c/event-from-single-occ "x" (c/ymd-to-date 2010 1 1))
                         nil
                         nil)
         "an event named \"x\" on Feb 1, 2010"))
  (is
    (=
      (c/format-event (c/event-from-single-rec "x"
                                               {:recur-type :year,
                                                :freq 1,
                                                :day-selection :occ-dow-month,
                                                :occ #{-1},
                                                :m #{5},
                                                :dow 5})
                      nil
                      nil)
      "an event named \"x\" repeating every year on the last Friday of June from this year")))

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
  (is (= (c/remove-subsequence "" c/cmdline-prompt) ""))
  (is (= (c/remove-subsequence ">" c/cmdline-prompt) ""))
  (is (= (c/remove-subsequence "a>" c/cmdline-prompt) "a"))
  (is (= (c/remove-subsequence ">a" c/cmdline-prompt) "a"))
  (is (= (c/remove-subsequence ">>> " c/cmdline-prompt) ""))
  (is (= (c/remove-subsequence ">>> a" c/cmdline-prompt) "a"))
  (is (= (c/remove-subsequence "a>>> " c/cmdline-prompt) "a"))
  (is (= (c/remove-subsequence "ab>>> " c/cmdline-prompt) "ab"))
  (is (= (c/remove-subsequence ">p>> " c/cmdline-prompt) "p"))
  (is (= (c/remove-subsequence "a>b>>c " c/cmdline-prompt) "abc"))
  (is (= (c/remove-subsequence "ab>>c " c/cmdline-prompt) "abc ")))

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

(deftest recur-month-possibly-feb
  (is (c/recur-month-possibly-feb 1 1) "already Feb")
  (is (c/recur-month-possibly-feb 3 1) "every month")
  (is (not (c/recur-month-possibly-feb 3 3)) "only April July October January")
  (is (not (c/recur-month-possibly-feb 3 4)) "only April August December")
  (is (not (c/recur-month-possibly-feb 3 12)) "only April")
  (is (not (c/recur-month-possibly-feb 3 24)) "only April every other year"))

(deftest find-first-occ
  (is (= (c/find-first-occ
           {:recur-type :day, :freq 1, :recur-start (c/ymd-to-date 2024 9 18)})
         (c/ymd-to-date 2024 9 18)))
  (is (= (c/find-first-occ
           {:recur-type :day, :freq 3, :recur-start (c/ymd-to-date 2024 9 18)})
         (c/ymd-to-date 2024 9 18)))
  (is (= (c/find-first-occ {:recur-type :week,
                            :freq 1,
                            :dow #{1},
                            :recur-start (c/ymd-to-date 2024 0 1)})
         (c/ymd-to-date 2024 0 1)))
  (is (= (c/find-first-occ {:recur-type :week,
                            :freq 1,
                            :dow #{2},
                            :recur-start (c/ymd-to-date 2024 0 1)})
         (c/ymd-to-date 2024 0 2)))
  (is (= (c/find-first-occ {:recur-type :week,
                            :freq 1,
                            :dow #{1 5},
                            :recur-start (c/ymd-to-date 2024 0 1)})
         (c/ymd-to-date 2024 0 1)))
  (is (= (c/find-first-occ {:recur-type :week,
                            :freq 2,
                            :dow #{1},
                            :recur-start (c/ymd-to-date 2024 0 6)})
         (c/ymd-to-date 2024 0 15)))
  (is (= (c/find-first-occ {:recur-type :month,
                            :freq 1,
                            :day-selection :d,
                            :d #{1},
                            :recur-start (c/ymd-to-date 2024 0 1)})
         (c/ymd-to-date 2024 0 1)))
  (is (= (c/find-first-occ {:recur-type :month,
                            :freq 1,
                            :day-selection :d,
                            :d #{8},
                            :recur-start (c/ymd-to-date 2024 0 1)})
         (c/ymd-to-date 2024 0 8)))
  (is (= (c/find-first-occ {:recur-type :month,
                            :freq 3,
                            :day-selection :d,
                            :d #{8 18 28},
                            :recur-start (c/ymd-to-date 2024 0 30)})
         (c/ymd-to-date 2024 3 8)))
  (is (= (c/find-first-occ {:recur-type :month,
                            :freq 1,
                            :day-selection :d,
                            :d #{31},
                            :recur-start (c/ymd-to-date 2024 1 1)})
         (c/ymd-to-date 2024 2 31))
      "skips over non-existent 31st days")
  (is (= (c/find-first-occ {:recur-type :month,
                            :freq 2,
                            :day-selection :d,
                            :d #{31},
                            :recur-start (c/ymd-to-date 2024 1 1)})
         (c/ymd-to-date 2024 7 31))
      "skips over non-existent 31st days and also every second month")
  (is (= (c/find-first-occ {:recur-type :month,
                            :freq 300,
                            :day-selection :d,
                            :d #{29},
                            :recur-start (c/ymd-to-date 2025 1 1)})
         (c/ymd-to-date 2400 1 29))
      "Feb 29th every 25 years => next occ 375 years later")
  (is (nil? (c/find-first-occ {:recur-type :month,
                               :freq 12,
                               :day-selection :d,
                               :d #{31},
                               :recur-start (c/ymd-to-date 2024 3 1)}))
      "cannot adjust start date as no events occur")
  (is (= (c/find-first-occ {:recur-type :month,
                            :freq 1,
                            :day-selection :dow,
                            :dow 1,
                            :occ #{0},
                            :recur-start (c/ymd-to-date 2024 0 1)})
         (c/ymd-to-date 2024 0 1)))
  (is (= (c/find-first-occ {:recur-type :month,
                            :freq 1,
                            :day-selection :dow,
                            :dow 1,
                            :occ #{4},
                            :recur-start (c/ymd-to-date 2024 0 1)})
         (c/ymd-to-date 2024 0 29))
      "adjusts to different day in the same month")
  (is (= (c/find-first-occ {:recur-type :month,
                            :freq 2,
                            :day-selection :dow,
                            :dow 1,
                            :occ #{4},
                            :recur-start (c/ymd-to-date 2024 0 30)})
         (c/ymd-to-date 2024 6 29))
      "adjusts to different month")
  (is (= (c/find-first-occ {:recur-type :month,
                            :freq 12,
                            :day-selection :dow,
                            :dow 1,
                            :occ #{4},
                            :recur-start (c/ymd-to-date 2020 1 1)})
         (c/ymd-to-date 2044 1 29))
      "fifth Monday in February")
  (is (nil? (c/find-first-occ {:recur-type :month,
                               :freq (* 12 25),
                               :day-selection :dow,
                               :dow 1,
                               :occ #{4},
                               :recur-start (c/ymd-to-date 2020 1 1)}))
      "cannot find fifth Monday in February every 25 years")
  (is (= (c/find-first-occ {:recur-type :month,
                            :freq 293,
                            :day-selection :dow,
                            :dow 5,
                            :occ #{4},
                            :recur-start (c/ymd-to-date 2235 7 1)})
         (c/ymd-to-date 13394 0 31))
      "fifth Friday every 293 months")
  (is (= (c/find-first-occ {:recur-type :month,
                            :freq 135,
                            :day-selection :dow,
                            :dow 4,
                            :occ #{4},
                            :recur-start (c/ymd-to-date 2077 2 1)})
         (c/ymd-to-date 3427 2 29))
      "fifth Thursday every 293 months")
  (is (= (c/find-first-occ {:recur-type :year,
                            :freq 1,
                            :day-selection :md,
                            :m 11,
                            :d 25,
                            :recur-start (c/ymd-to-date 2020 0 1)})
         (c/ymd-to-date 2020 11 25))
      "adjusts to later this year")
  (is (= (c/find-first-occ {:recur-type :year,
                            :freq 1,
                            :day-selection :md,
                            :m 1,
                            :d 29,
                            :recur-start (c/ymd-to-date 1995 0 1)})
         (c/ymd-to-date 1996 1 29))
      "finds Feb 29")
  (is (= (c/find-first-occ {:recur-type :year,
                            :freq 5,
                            :day-selection :md,
                            :m 1,
                            :d 29,
                            :recur-start (c/ymd-to-date 1996 10 1)})
         (c/ymd-to-date 2016 1 29))
      "finds Feb 29 every five years (20 years later)")
  (is (= (c/find-first-occ {:recur-type :year,
                            :freq 25,
                            :day-selection :md,
                            :m 1,
                            :d 29,
                            :recur-start (c/ymd-to-date 2025 2 1)})
         (c/ymd-to-date 2400 1 29))
      "finds Feb 29 every 25 years (375 years later)")
  (is (nil? (c/find-first-occ {:recur-type :year,
                               :freq 1,
                               :day-selection :md,
                               :m 3,
                               :d 31,
                               :recur-start (c/ymd-to-date 1995 0 1)}))
      "cannot find April 31")
  (is (= (c/find-first-occ {:recur-type :year,
                            :freq 1,
                            :day-selection :occ-dow-month,
                            :dow 1,
                            :m #{1},
                            :occ #{4},
                            :recur-start (c/ymd-to-date 2016 2 1)})
         (c/ymd-to-date 2044 1 29))
      "fifth Monday in February every year")
  (is (= (c/find-first-occ {:recur-type :year,
                            :freq 8,
                            :day-selection :occ-dow-month,
                            :dow 1,
                            :m #{1},
                            :occ #{4},
                            :recur-start (c/ymd-to-date 2016 2 1)})
         (c/ymd-to-date 2072 1 29))
      "fifth Monday in February every 8 years")
  (is (= (c/find-first-occ {:recur-type :year,
                            :freq 131,
                            :day-selection :occ-dow-month,
                            :dow 1,
                            :m #{1},
                            :occ #{4},
                            :recur-start (c/ymd-to-date 2035 0 1)})
         (c/ymd-to-date 44872 1 29))
      "fifth Monday in February every 131 years")
  (is (= (c/find-first-occ {:recur-type :year,
                            :freq 231,
                            :day-selection :occ-dow-month,
                            :dow 4,
                            :m #{2},
                            :occ #{4},
                            :recur-start (c/ymd-to-date 2154 0 1)})
         (c/ymd-to-date 43272 2 31))
      "fifth Thursday in March every 231 years")
  (is (= (c/find-first-occ {:recur-type :year,
                            :freq 231,
                            :day-selection :occ-dow-month,
                            :dow 6,
                            :m #{3},
                            :occ #{4},
                            :recur-start (c/ymd-to-date 2142 0 1)})
         (c/ymd-to-date 55272 3 30))
      "fifth Saturday in April every 231 years"))

(deftest common-prefix
  (is (= (c/common-prefix "abcd" "abce") "abc"))
  (is (= (c/common-prefix "ab" "ab") "ab"))
  (is (= (c/common-prefix "cd" "ab") ""))
  (is (= (c/common-prefix "" "ab") ""))
  (is (= (c/common-prefix "" "") ""))
  (is (= (c/common-prefix "Jan" "January") "Jan")))

(deftest common-prefixes
  (is (= (c/common-prefixes []) nil))
  (is (= (c/common-prefixes ["Jan"]) "Jan"))
  (is (= (c/common-prefixes ["Jan" "Feb"]) ""))
  (is (= (c/common-prefixes ["Jan" "Janu"]) "Jan"))
  (is (= (c/common-prefixes ["Jan" "Janu" "January"]) "Jan"))
  (is (= (c/common-prefixes ["F" "Janu" "January"]) "")))

(defn parse-and-find-completion
  [input]
  (c/find-parser-based-completion
    (c/cmdline-parser input :total true :unhide :all)))

(deftest find-parser-based-completion
  (is (= (parse-and-find-completion "add \"x\" every 3 days") nil))
  (is (= (parse-and-find-completion "add \"x\" every 3 da")
         "add \"x\" every 3 days"))
  (is (= (parse-and-find-completion "add \"x\" every 3 years")
         "add \"x\" every 3 years "))
  (is (= (parse-and-find-completion "add \"x\" every 3 years ")
         "add \"x\" every 3 years on"))
  (is (= (parse-and-find-completion "add \"x\" every 3 years o")
         "add \"x\" every 3 years on"))
  (is (= (parse-and-find-completion "add \"x\" every 3 years on")
         "add \"x\" every 3 years on "))
  (is (= (parse-and-find-completion "add \"x\" every 3 years on ") nil))
  (is (= (parse-and-find-completion "add \"x\" every 3 years on  ") nil))
  (is (= (parse-and-find-completion "add \"x\" every 3 years on Ja")
         "add \"x\" every 3 years on Jan")))

(deftest day-rec-to-period
  (is (= (c/day-rec-to-period {:freq 1, :recur-start (c/day-num-to-date 100)})
         {:freq 1,
          :recur-period-remainder 0,
          :recur-period-start 100,
          :recur-period-end nil}))
  (is (= (c/day-rec-to-period {:freq 2, :recur-start (c/day-num-to-date 101)})
         {:freq 2,
          :recur-period-remainder 1,
          :recur-period-start 100,
          :recur-period-end nil}))
  (is (= (c/day-rec-to-period {:freq 2,
                               :recur-start (c/day-num-to-date 101),
                               :recur-end (c/day-num-to-date 109)})
         {:freq 2,
          :recur-period-remainder 1,
          :recur-period-start 100,
          :recur-period-end 109}))
  (is (= (c/day-rec-to-period {:freq 2,
                               :recur-start (c/day-num-to-date 101),
                               :recur-end (c/day-num-to-date 110)})
         {:freq 2,
          :recur-period-remainder 1,
          :recur-period-start 100,
          :recur-period-end 111}))
  (is (= (c/day-rec-to-period {:freq 2,
                               :recur-start (c/day-num-to-date 100),
                               :recur-end (c/day-num-to-date 110)})
         {:freq 2,
          :recur-period-remainder 0,
          :recur-period-start 100,
          :recur-period-end 110})))

(deftest week-rec-to-periods
  (is (= (c/week-month-rec-to-periods {:recur-type :week,
                                       :freq 1,
                                       :dow #{1},
                                       :recur-start (c/ymd-to-date 2024 11 8)})
         [{:recur-type :week,
           :freq 1,
           :dow #{1},
           :recur-period-remainder 0,
           :recur-period-start (c/week-num (c/ymd-to-date 2024 11 8)),
           :recur-period-end nil}])
      "no partial weeks")
  (is (= (c/week-month-rec-to-periods {:recur-type :week,
                                       :freq 1,
                                       :dow #{1},
                                       :recur-start (c/ymd-to-date 2024 11 8),
                                       :recur-end (c/ymd-to-date 2024 11 29)})
         [{:recur-type :week,
           :freq 1,
           :dow #{1},
           :recur-period-remainder 0,
           :recur-period-start (c/week-num (c/ymd-to-date 2024 11 8)),
           :recur-period-end (c/week-num (c/ymd-to-date 2024 11 29))}])
      "no partial weeks with end")
  (is (= (c/week-month-rec-to-periods {:recur-type :week,
                                       :freq 1,
                                       :dow #{1 6},
                                       :recur-start (c/ymd-to-date 2024 11 7)})
         [{:recur-type :week,
           :freq 1,
           :dow #{6},
           :recur-period-remainder 0,
           :recur-period-start (dec (c/week-num (c/ymd-to-date 2024 11 8))),
           :recur-period-end (c/week-num (c/ymd-to-date 2024 11 8))}
          {:recur-type :week,
           :freq 1,
           :dow #{1 6},
           :recur-period-remainder 0,
           :recur-period-start (c/week-num (c/ymd-to-date 2024 11 8)),
           :recur-period-end nil}])
      "one partial week at the front")
  (is (= (c/week-month-rec-to-periods {:recur-type :week,
                                       :freq 1,
                                       :dow #{1 5},
                                       :recur-start (c/ymd-to-date 2024 11 14)})
         [{:recur-type :week,
           :freq 1,
           :dow #{1 5},
           :recur-period-remainder 0,
           :recur-period-start (inc (c/week-num (c/ymd-to-date 2024 11 14))),
           :recur-period-end nil}])
      "no partial week at the front because no occ")
  (is (= (c/week-month-rec-to-periods {:recur-type :week,
                                       :freq 2,
                                       :dow #{5 6},
                                       :recur-start (c/ymd-to-date 2024 11 12)})
         [{:recur-type :week,
           :freq 2,
           :dow #{5 6},
           :recur-period-remainder 1,
           :recur-period-start 22172,
           :recur-period-end nil}])
      "no partial week at the front because can be extended backwards")
  (is (= (c/week-month-rec-to-periods {:recur-type :week,
                                       :freq 1,
                                       :dow #{1 6},
                                       :recur-start (c/ymd-to-date 2024 11 8),
                                       :recur-end (c/ymd-to-date 2024 11 31)})
         [{:recur-type :week,
           :freq 1,
           :dow #{1 6},
           :recur-period-remainder 0,
           :recur-period-start (c/week-num (c/ymd-to-date 2024 11 8)),
           :recur-period-end (c/week-num (c/ymd-to-date 2024 11 31))}
          {:recur-type :week,
           :freq 1,
           :dow #{1},
           :recur-period-remainder 0,
           :recur-period-start (c/week-num (c/ymd-to-date 2024 11 31)),
           :recur-period-end (inc (c/week-num (c/ymd-to-date 2024 11 31)))}])
      "one partial week at the end")
  (is (= (c/week-month-rec-to-periods {:recur-type :week,
                                       :freq 1,
                                       :dow #{1 0},
                                       :recur-start (c/ymd-to-date 2024 11 8),
                                       :recur-end (c/ymd-to-date 2024 11 29)})
         [{:recur-type :week,
           :freq 1,
           :dow #{1 0},
           :recur-period-remainder 0,
           :recur-period-start (c/week-num (c/ymd-to-date 2024 11 8)),
           :recur-period-end (c/week-num (c/ymd-to-date 2024 11 29))}])
      "no partial week at the end because no occ")
  (is (= (c/week-month-rec-to-periods {:recur-type :week,
                                       :freq 2,
                                       :dow #{1 0},
                                       :recur-start (c/ymd-to-date 2024 11 15),
                                       :recur-end (c/ymd-to-date 2024 11 31)})
         [{:recur-type :week,
           :freq 2,
           :dow #{1 0},
           :recur-period-remainder 0,
           :recur-period-start (c/week-num (c/ymd-to-date 2024 11 15)),
           :recur-period-end (c/week-num (c/ymd-to-date 2025 0 12))}])
      "no partial week at the end because can be extended")
  (is (= (c/week-month-rec-to-periods {:recur-type :week,
                                       :freq 2,
                                       :dow #{1},
                                       :recur-start (c/ymd-to-date 2024 11 1)})
         [{:recur-type :week,
           :freq 2,
           :dow #{1},
           :recur-period-remainder 0,
           :recur-period-start 22172,
           :recur-period-end nil}])
      "period 2; zero remainder; no partial weeks")
  (is (= (c/week-month-rec-to-periods {:recur-type :week,
                                       :freq 2,
                                       :dow #{1},
                                       :recur-start (c/ymd-to-date 2024 11 8)})
         [{:recur-type :week,
           :freq 2,
           :dow #{1},
           :recur-period-remainder 1,
           :recur-period-start 22172,
           :recur-period-end nil}])
      "period 2; non-zero remainder; no partial weeks")
  (is (= (c/week-month-rec-to-periods {:recur-type :week,
                                       :freq 2,
                                       :dow #{1},
                                       :recur-start (c/ymd-to-date 2024 11 10)})
         [{:recur-type :week,
           :freq 2,
           :dow #{1},
           :recur-period-remainder 1,
           :recur-period-start 22174,
           :recur-period-end nil}])
      "period 2; non-zero remainder; partial week has no occ")
  (is (= (c/week-month-rec-to-periods {:recur-type :week,
                                       :freq 2,
                                       :dow #{1 5},
                                       :recur-start (c/ymd-to-date 2024 11 10)})
         [{:recur-type :week,
           :freq 1,
           :dow #{5},
           :recur-period-remainder 0,
           :recur-period-start 22173,
           :recur-period-end 22174}
          {:recur-type :week,
           :freq 2,
           :dow #{1 5},
           :recur-period-remainder 1,
           :recur-period-start 22174,
           :recur-period-end nil}])
      "period 2; non-zero remainder; partial week at the front")
  (is (= (c/week-month-rec-to-periods {:recur-type :week,
                                       :freq 2,
                                       :dow #{1 5},
                                       :recur-start (c/ymd-to-date 2024 11 10),
                                       :recur-end (c/ymd-to-date 2025 0 22)})
         [{:recur-type :week,
           :freq 1,
           :dow #{5},
           :recur-period-remainder 0,
           :recur-period-start 22173,
           :recur-period-end 22174}
          {:recur-type :week,
           :freq 2,
           :dow #{1 5},
           :recur-period-remainder 1,
           :recur-period-start 22174,
           :recur-period-end 22179}
          {:recur-type :week,
           :freq 1,
           :dow #{1},
           :recur-period-remainder 0,
           :recur-period-start 22179,
           :recur-period-end 22180}])
      "period 2; non-zero remainder; partial week at the front and back"))

(deftest month-rec-to-periods
  (is (= (c/week-month-rec-to-periods {:recur-type :month,
                                       :freq 1,
                                       :day-selection :d,
                                       :d #{1},
                                       :recur-start (c/ymd-to-date 2024 11 1)})
         [{:recur-type :month,
           :freq 1,
           :day-selection :d,
           :d #{1},
           :recur-period-remainder 0,
           :recur-period-start (c/month-num (c/ymd-to-date 2024 11 1)),
           :recur-period-end nil}])
      "no partial months")
  (is (= (c/week-month-rec-to-periods {:recur-type :month,
                                       :freq 1,
                                       :day-selection :d,
                                       :d #{1},
                                       :recur-start (c/ymd-to-date 2024 1 1),
                                       :recur-end (c/ymd-to-date 2024 10 1)})
         [{:recur-type :month,
           :freq 1,
           :day-selection :d,
           :d #{1},
           :recur-period-remainder 0,
           :recur-period-start (c/month-num (c/ymd-to-date 2024 1 1)),
           :recur-period-end (c/month-num (c/ymd-to-date 2024 10 1))}])
      "no partial months with end")
  (is (= (c/week-month-rec-to-periods {:recur-type :month,
                                       :freq 1,
                                       :day-selection :d,
                                       :d #{1 20},
                                       :recur-start (c/ymd-to-date 2024 11 15)})
         [{:recur-type :month,
           :freq 1,
           :day-selection :d,
           :d #{20},
           :recur-period-remainder 0,
           :recur-period-start (c/month-num (c/ymd-to-date 2024 11 1)),
           :recur-period-end (inc (c/month-num (c/ymd-to-date 2024 11 1)))}
          {:recur-type :month,
           :freq 1,
           :day-selection :d,
           :d #{1 20},
           :recur-period-remainder 0,
           :recur-period-start (inc (c/month-num (c/ymd-to-date 2024 11 1))),
           :recur-period-end nil}])
      "one partial month at the front")
  (is (= (c/week-month-rec-to-periods {:recur-type :month,
                                       :freq 1,
                                       :day-selection :d,
                                       :d #{1 5},
                                       :recur-start (c/ymd-to-date 2024 11 14)})
         [{:recur-type :month,
           :freq 1,
           :day-selection :d,
           :d #{1 5},
           :recur-period-remainder 0,
           :recur-period-start (inc (c/month-num (c/ymd-to-date 2024 11 14))),
           :recur-period-end nil}])
      "no partial month at the front because no occ")
  (is (= (c/week-month-rec-to-periods {:recur-type :month,
                                       :freq 2,
                                       :day-selection :d,
                                       :d #{10 20},
                                       :recur-start (c/ymd-to-date 2024 11 5)})
         [{:recur-type :month,
           :freq 2,
           :day-selection :d,
           :d #{10 20},
           :recur-period-remainder 1,
           :recur-period-start 5098,
           :recur-period-end nil}])
      "no partial month at the front because can be extended backwards")
  (is (= (c/week-month-rec-to-periods {:recur-type :month,
                                       :freq 1,
                                       :day-selection :d,
                                       :d #{1 6},
                                       :recur-start (c/ymd-to-date 2024 10 1),
                                       :recur-end (c/ymd-to-date 2024 11 3)})
         [{:recur-type :month,
           :freq 1,
           :day-selection :d,
           :d #{1 6},
           :recur-period-remainder 0,
           :recur-period-start (c/month-num (c/ymd-to-date 2024 10 1)),
           :recur-period-end (c/month-num (c/ymd-to-date 2024 11 1))}
          {:recur-type :month,
           :freq 1,
           :day-selection :d,
           :d #{1},
           :recur-period-remainder 0,
           :recur-period-start (c/month-num (c/ymd-to-date 2024 11 1)),
           :recur-period-end (c/month-num (c/ymd-to-date 2025 0 1))}])
      "one partial month at the end")
  (is (= (c/week-month-rec-to-periods {:recur-type :month,
                                       :freq 1,
                                       :day-selection :d,
                                       :d #{10 20},
                                       :recur-start (c/ymd-to-date 2024 10 1),
                                       :recur-end (c/ymd-to-date 2024 11 5)})
         [{:recur-type :month,
           :freq 1,
           :day-selection :d,
           :d #{10 20},
           :recur-period-remainder 0,
           :recur-period-start (c/month-num (c/ymd-to-date 2024 10 1)),
           :recur-period-end (c/month-num (c/ymd-to-date 2024 11 1))}])
      "no partial month at the end because no occ")
  (is (= (c/week-month-rec-to-periods {:recur-type :month,
                                       :freq 2,
                                       :day-selection :d,
                                       :d #{10 15},
                                       :recur-start (c/ymd-to-date 2024 8 1),
                                       :recur-end (c/ymd-to-date 2024 10 20)})
         [{:recur-type :month,
           :freq 2,
           :day-selection :d,
           :d #{10 15},
           :recur-period-remainder 0,
           :recur-period-start (c/month-num (c/ymd-to-date 2024 8 1)),
           :recur-period-end (c/month-num (c/ymd-to-date 2025 0 1))}])
      "no partial month at the end because can be extended")
  (is (= (c/week-month-rec-to-periods {:recur-type :month,
                                       :freq 2,
                                       :day-selection :d,
                                       :d #{10 20},
                                       :recur-start (c/ymd-to-date 2024 1 15),
                                       :recur-end (c/ymd-to-date 2024 7 15)})
         [{:recur-type :month,
           :freq 1,
           :day-selection :d,
           :d #{20},
           :recur-period-remainder 0,
           :recur-period-start 5089,
           :recur-period-end 5090}
          {:recur-type :month,
           :freq 2,
           :day-selection :d,
           :d #{10 20},
           :recur-period-remainder 1,
           :recur-period-start 5090,
           :recur-period-end 5095}
          {:recur-type :month,
           :freq 1,
           :day-selection :d,
           :d #{10},
           :recur-period-remainder 0,
           :recur-period-start 5095,
           :recur-period-end 5096}])
      "period 2; non-zero remainder; partial month at the front and back")
  (is
    (= (c/week-month-rec-to-periods {:recur-type :month,
                                     :freq 2,
                                     :day-selection :dow,
                                     :dow 3,
                                     :occ #{0 3},
                                     :recur-start (c/ymd-to-date 2024 1 15),
                                     :recur-end (c/ymd-to-date 2024 7 15)})
       [{:recur-type :month,
         :freq 1,
         :day-selection :dow,
         :dow 3,
         :occ #{3},
         :recur-period-remainder 0,
         :recur-period-start 5089,
         :recur-period-end 5090}
        {:recur-type :month,
         :freq 2,
         :day-selection :dow,
         :dow 3,
         :occ #{0 3},
         :recur-period-remainder 1,
         :recur-period-start 5090,
         :recur-period-end 5095}
        {:recur-type :month,
         :freq 1,
         :day-selection :dow,
         :dow 3,
         :occ #{0},
         :recur-period-remainder 0,
         :recur-period-start 5095,
         :recur-period-end 5096}])
    "period 2; non-zero remainder; partial month at the front and back; using :day-selection :dow")
  (is (= (c/week-month-rec-to-periods {:recur-type :month,
                                       :freq 48,
                                       :day-selection :d,
                                       :d #{29},
                                       :recur-start (c/ymd-to-date 1900 1 15)})
         [{:recur-type :month,
           :freq 48,
           :day-selection :d,
           :d #{29},
           :recur-period-remainder 1,
           :recur-period-start (c/month-num (c/ymd-to-date 1904 0 1)),
           :recur-period-end nil}])
      "Feb 29: front period has no occ")
  (is (= (c/week-month-rec-to-periods {:recur-type :month,
                                       :freq 48,
                                       :day-selection :d,
                                       :d #{29},
                                       :recur-start (c/ymd-to-date 1900 1 15),
                                       :recur-end (c/ymd-to-date 2100 1 15)})
         [{:recur-type :month,
           :freq 48,
           :day-selection :d,
           :d #{29},
           :recur-period-remainder 1,
           :recur-period-start (c/month-num (c/ymd-to-date 1904 0 1)),
           :recur-period-end (c/month-num (c/ymd-to-date 2100 1 1))}])
      "Feb 29: front and back periods have no occ"))

(deftest split-recs-without-overlap
  (is (= (c/split-recs-without-overlap
           [{:recur-period-start 200, :recur-period-end 300, :id 0}])
         [{:recur-period-start 200, :recur-period-end 300, :id 0}]))
  (is (= (c/split-recs-without-overlap
           [{:recur-period-start 200, :recur-period-end 300, :id 0}
            {:recur-period-start 100, :recur-period-end 500, :id 1}])
         [{:recur-period-start 200, :recur-period-end 300, :id 0}
          {:recur-period-start 100, :recur-period-end 200, :id 1}
          {:recur-period-start 200, :recur-period-end 300, :id 1}
          {:recur-period-start 300, :recur-period-end 500, :id 1}]))
  (is (= (c/split-recs-without-overlap
           [{:recur-period-start 200, :recur-period-end 300, :id 0}
            {:recur-period-start 100, :recur-period-end nil, :id 1}])
         [{:recur-period-start 200, :recur-period-end 300, :id 0}
          {:recur-period-start 100, :recur-period-end 200, :id 1}
          {:recur-period-start 200, :recur-period-end 300, :id 1}
          {:recur-period-start 300, :recur-period-end nil, :id 1}]))
  (is (= (c/split-recs-without-overlap
           [{:recur-period-start 200, :recur-period-end nil, :id 0}
            {:recur-period-start 100, :recur-period-end nil, :id 1}])
         [{:recur-period-start 200, :recur-period-end nil, :id 0}
          {:recur-period-start 100, :recur-period-end 200, :id 1}
          {:recur-period-start 200, :recur-period-end nil, :id 1}]))
  (is (= (c/split-recs-without-overlap
           [{:recur-period-start 2, :recur-period-end 4, :id 0}
            {:recur-period-start 2, :recur-period-end 6, :id 1}
            {:recur-period-start 3, :recur-period-end 9, :id 2}])
         [{:recur-period-start 2, :recur-period-end 3, :id 0}
          {:recur-period-start 3, :recur-period-end 4, :id 0}
          {:recur-period-start 2, :recur-period-end 3, :id 1}
          {:recur-period-start 3, :recur-period-end 4, :id 1}
          {:recur-period-start 4, :recur-period-end 6, :id 1}
          {:recur-period-start 3, :recur-period-end 4, :id 2}
          {:recur-period-start 4, :recur-period-end 6, :id 2}
          {:recur-period-start 6, :recur-period-end 9, :id 2}])))

(deftest divisors
  (is (= (c/divisors 1) [1]))
  (is (= (c/divisors 2) [1 2]))
  (is (= (c/divisors 4) [1 2 4]))
  (is (= (c/divisors 19) [1 19]))
  (is (= (c/divisors 24) [1 2 3 4 6 8 12 24]))
  (is (= (c/divisors 49) [1 7 49])))

(deftest rec-group-remove-redundant-high-divisors
  (is (= (c/rec-group-remove-redundant-high-divisors
           [{:freq 2, :recur-period-remainder 0}
            {:freq 2, :recur-period-remainder 0}])
         [{:freq 2, :recur-period-remainder 0}]))
  (is (= (c/rec-group-remove-redundant-high-divisors
           [{:freq 4, :recur-period-remainder 0}
            {:freq 2, :recur-period-remainder 0}])
         [{:freq 2, :recur-period-remainder 0}]))
  (is (= (c/rec-group-remove-redundant-high-divisors
           [{:freq 4, :recur-period-remainder 3}
            {:freq 2, :recur-period-remainder 1}])
         [{:freq 2, :recur-period-remainder 1}]))
  (is (= (c/rec-group-remove-redundant-high-divisors
           [{:freq 3, :recur-period-remainder 1}
            {:freq 2, :recur-period-remainder 0}])
         [{:freq 2, :recur-period-remainder 0}
          {:freq 3, :recur-period-remainder 1}])
      "coprime divisors; nothing to do")
  (is (= (c/rec-group-remove-redundant-high-divisors
           [{:freq 6, :recur-period-remainder 1}
            {:freq 12, :recur-period-remainder 7}
            {:freq 24, :recur-period-remainder 21}])
         [{:freq 6, :recur-period-remainder 1}
          {:freq 24, :recur-period-remainder 21}])))

(deftest rec-group-reduce-large-period
  (is (= (c/rec-group-reduce-large-period [{:freq 2, :recur-period-remainder 0}
                                           {:freq 2,
                                            :recur-period-remainder 0}])
         [{:freq 2, :recur-period-remainder 0}]))
  (is (= (c/rec-group-reduce-large-period [{:freq 2, :recur-period-remainder 0}
                                           {:freq 2,
                                            :recur-period-remainder 1}])
         [{:freq 1, :recur-period-remainder 0}]))
  (is (= (c/rec-group-reduce-large-period [{:freq 4, :recur-period-remainder 0}
                                           {:freq 4, :recur-period-remainder 1}
                                           {:freq 4, :recur-period-remainder 2}
                                           {:freq 4,
                                            :recur-period-remainder 3}])
         [{:freq 1, :recur-period-remainder 0}]))
  (is (= (c/rec-group-reduce-large-period [{:freq 6, :recur-period-remainder 0}
                                           {:freq 6, :recur-period-remainder 2}
                                           {:freq 6,
                                            :recur-period-remainder 4}])
         [{:freq 2, :recur-period-remainder 0}]))
  (is (= (c/rec-group-reduce-large-period [{:freq 6, :recur-period-remainder 0}
                                           {:freq 6, :recur-period-remainder 1}
                                           {:freq 6, :recur-period-remainder 2}
                                           {:freq 6,
                                            :recur-period-remainder 4}])
         [{:freq 2, :recur-period-remainder 0}
          {:freq 6, :recur-period-remainder 1}]))
  (is (= (c/rec-group-reduce-large-period [{:freq 6, :recur-period-remainder 1}
                                           {:freq 6, :recur-period-remainder 3}
                                           {:freq 6,
                                            :recur-period-remainder 5}])
         [{:freq 2, :recur-period-remainder 1}]))
  (is (= (c/rec-group-reduce-large-period [{:freq 6, :recur-period-remainder 0}
                                           {:freq 6,
                                            :recur-period-remainder 3}])
         [{:freq 3, :recur-period-remainder 0}]))
  (is (= (c/rec-group-reduce-large-period [{:freq 6, :recur-period-remainder 1}
                                           {:freq 6,
                                            :recur-period-remainder 4}])
         [{:freq 3, :recur-period-remainder 1}]))
  (is (= (c/rec-group-reduce-large-period [{:freq 6, :recur-period-remainder 2}
                                           {:freq 6,
                                            :recur-period-remainder 5}])
         [{:freq 3, :recur-period-remainder 2}]))
  (is (= (c/rec-group-reduce-large-period
           [{:freq 24, :recur-period-remainder 1}
            {:freq 24, :recur-period-remainder 13}
            {:freq 24, :recur-period-remainder 5}
            {:freq 24, :recur-period-remainder 11}
            {:freq 24, :recur-period-remainder 17}
            {:freq 24, :recur-period-remainder 23}])
         [{:freq 6, :recur-period-remainder 5}
          {:freq 12, :recur-period-remainder 1}]))
  (is (= (c/rec-group-reduce-large-period
           [{:freq 24, :recur-period-remainder 1}
            {:freq 24, :recur-period-remainder 2}
            {:freq 24, :recur-period-remainder 13}
            {:freq 24, :recur-period-remainder 14}])
         [{:freq 12, :recur-period-remainder 1}
          {:freq 12, :recur-period-remainder 2}]))
  (is (= (c/rec-group-reduce-large-period
           [{:freq 24, :recur-period-remainder 1}
            {:freq 24, :recur-period-remainder 3}
            {:freq 24, :recur-period-remainder 7}
            {:freq 24, :recur-period-remainder 13}
            {:freq 24, :recur-period-remainder 15}
            {:freq 24, :recur-period-remainder 19}
            {:freq 24, :recur-period-remainder 21}])
         [{:freq 6, :recur-period-remainder 1}
          {:freq 12, :recur-period-remainder 3}
          {:freq 24, :recur-period-remainder 21}]))
  (is (= (c/rec-group-reduce-large-period [{:freq 2, :recur-period-remainder 0}
                                           {:freq 3,
                                            :recur-period-remainder 1}])
         [{:freq 2, :recur-period-remainder 0}
          {:freq 3, :recur-period-remainder 1}])
      "different freq; nothing to do")
  (is (= (c/rec-group-reduce-large-period [{:freq 2, :recur-period-remainder 0}
                                           {:freq 4, :recur-period-remainder 1}
                                           {:freq 4,
                                            :recur-period-remainder 3}])
         [{:freq 1, :recur-period-remainder 0}])
      "multi-step reduction"))

(deftest recombine-periods
  (is (= (c/recombine-periods [{:recur-period-start 0, :recur-period-end 10}
                               {:recur-period-start 10, :recur-period-end 20}
                               {:recur-period-start 20, :recur-period-end 30}
                               {:recur-period-start 40, :recur-period-end 60}
                               {:recur-period-start 60, :recur-period-end nil}])
         [{:recur-period-start 0, :recur-period-end 30}
          {:recur-period-start 40, :recur-period-end nil}])))

(deftest try-get-single-occ-from-rec
  (is (= (c/try-get-single-occ-from-rec {:recur-type :day,
                                         :freq 6,
                                         :recur-period-remainder 5,
                                         :recur-period-start 996,
                                         :recur-period-end 1002})
         1001))
  (is (= (c/try-get-single-occ-from-rec {:recur-type :day,
                                         :freq 6,
                                         :recur-period-remainder 5,
                                         :recur-period-start 996,
                                         :recur-period-end nil})
         nil))
  (is (= (c/try-get-single-occ-from-rec {:recur-type :day,
                                         :freq 6,
                                         :recur-period-remainder 5,
                                         :recur-period-start 996,
                                         :recur-period-end 10000})
         nil)))

(deftest try-absorb-one
  (is (= (c/try-absorb-one #{15}
                           {:recur-period-start 0,
                            :recur-period-end 13,
                            :recur-period-remainder 0,
                            :freq 3})
         [#{}
          {:recur-period-start 0,
           :recur-period-end 18,
           :recur-period-remainder 0,
           :freq 3}])
      "simple forwards")
  (is (= (c/try-absorb-one #{16}
                           {:recur-period-start 0,
                            :recur-period-end 14,
                            :recur-period-remainder 1,
                            :freq 3})
         [#{}
          {:recur-period-start 0,
           :recur-period-end 19,
           :recur-period-remainder 1,
           :freq 3}])
      "non-zero remainder; forwards")
  (is (= (c/try-absorb-one #{15 18 21 22}
                           {:recur-period-start 0,
                            :recur-period-end 13,
                            :recur-period-remainder 0,
                            :freq 3})
         [#{22}
          {:recur-period-start 0,
           :recur-period-end 24,
           :recur-period-remainder 0,
           :freq 3}])
      "multiple forwards")
  (is (= (c/try-absorb-one #{6}
                           {:recur-period-start 9,
                            :recur-period-end 21,
                            :recur-period-remainder 0,
                            :freq 3})
         [#{}
          {:recur-period-start 6,
           :recur-period-end 21,
           :recur-period-remainder 0,
           :freq 3}])
      "simple backwards")
  (is (= (c/try-absorb-one #{7}
                           {:recur-period-start 10,
                            :recur-period-end 22,
                            :recur-period-remainder 1,
                            :freq 3})
         [#{}
          {:recur-period-start 7,
           :recur-period-end 22,
           :recur-period-remainder 1,
           :freq 3}])
      "non-zero remainder; backwards")
  (is (= (c/try-absorb-one #{1001}
                           {:recur-period-start 1002,
                            :recur-period-end nil,
                            :recur-period-remainder 1,
                            :freq 2})
         [#{}
          {:recur-period-start 1001,
           :recur-period-end nil,
           :recur-period-remainder 1,
           :freq 2}])
      "non-zero remainder; backwards")
  (is (= (c/try-absorb-one #{7 4 2}
                           {:recur-period-start 10,
                            :recur-period-end 22,
                            :recur-period-remainder 1,
                            :freq 3})
         [#{2}
          {:recur-period-start 4,
           :recur-period-end 22,
           :recur-period-remainder 1,
           :freq 3}])
      "multiple backwards")
  (is (= (c/try-absorb-one #{7 4 2 22 25 30}
                           {:recur-period-start 10,
                            :recur-period-end 22,
                            :recur-period-remainder 1,
                            :freq 3})
         [#{2 30}
          {:recur-period-start 4,
           :recur-period-end 28,
           :recur-period-remainder 1,
           :freq 3}])
      "multiple forwards and backwards"))

(deftest try-absorb
  (is (= (c/try-absorb #{1001}
                       [{:freq 2,
                         :recur-period-start 1003,
                         :recur-period-end nil,
                         :recur-period-remainder 1}])
         [{:freq 2,
           :recur-period-start 1001,
           :recur-period-end nil,
           :recur-period-remainder 1}]))
  (is (= (c/try-absorb #{1000}
                       [{:recur-type :day,
                         :freq 2,
                         :recur-period-start 1003,
                         :recur-period-end nil,
                         :recur-period-remainder 1}])
         [{:recur-type :day,
           :freq 2,
           :recur-period-start 1003,
           :recur-period-end nil,
           :recur-period-remainder 1}
          {:recur-type :day,
           :freq 1,
           :recur-period-start 1000,
           :recur-period-end 1001,
           :recur-period-remainder 0}])))

(defn- day-rec
  ([freq start]
   (c/event-from-single-rec
     "x"
     {:recur-type :day, :freq freq, :recur-start (c/day-num-to-date start)}))
  ([freq start end]
   (c/event-from-single-rec "x"
                            {:recur-type :day,
                             :freq freq,
                             :recur-start (c/day-num-to-date start),
                             :recur-end (c/day-num-to-date end)})))

(defn- week-rec
  ([freq dows start]
   (c/event-from-single-rec "x"
                            {:recur-type :week,
                             :dow (into (hash-set) dows),
                             :freq freq,
                             :recur-start (c/day-num-to-date start)}))
  ([freq dows start end]
   (c/event-from-single-rec "x"
                            {:recur-type :week,
                             :dow (into (hash-set) dows),
                             :freq freq,
                             :recur-start (c/day-num-to-date start),
                             :recur-end (c/day-num-to-date end)})))

(deftest optimize-event
  (is (= (c/optimize-event (c/event-from-single-occ "x"
                                                    (c/ymd-to-date 2024 1 1)))
         (c/event-from-single-occ "x" (c/ymd-to-date 2024 1 1)))
      "no rec")
  (is (= (c/optimize-event (day-rec 2 1001)) (day-rec 2 1001)) "single rec")
  (is (= (c/optimize-event (c/merge-event (day-rec 2 100) (day-rec 2 100)))
         (day-rec 2 100))
      "identical rec")
  (is (= (c/optimize-event (c/merge-event (day-rec 1 100) (day-rec 1 200)))
         (day-rec 1 100))
      "same divisor (1), later start")
  (is (= (c/optimize-event (c/merge-event (day-rec 2 100) (day-rec 2 102)))
         (day-rec 2 100))
      "same divisor (2), later start")
  (is (= (c/optimize-event (c/merge-event (day-rec 1 100 500)
                                          (day-rec 1 200 500)))
         (day-rec 1 100 500))
      "same divisor, later start with end")
  (is (= (c/optimize-event (c/merge-event (day-rec 1 100 200)
                                          (day-rec 1 200 300)))
         (day-rec 1 100 300))
      "divisor 1, perfect overlap")
  (is (= (c/optimize-event (c/merge-event (day-rec 2 100 200)
                                          (day-rec 2 200 300)))
         (day-rec 2 100 300))
      "divisor 2, perfect overlap")
  (is (= (c/optimize-event (c/merge-event (day-rec 2 100 199)
                                          (day-rec 2 200 299)))
         (day-rec 2 100 300))
      "divisor 2, perfect overlap with end adjustment")
  (is (= (c/optimize-event (c/merge-event (day-rec 10 100 155)
                                          (day-rec 10 160 299)))
         (day-rec 10 100 300))
      "more end adjustment")
  (is (= (c/optimize-event (c/merge-event (day-rec 1 100) (day-rec 2 100)))
         (day-rec 1 100))
      "same start, different divisors")
  (is (= (c/optimize-event (c/merge-event (day-rec 3 100) (day-rec 2 100)))
         (c/merge-event (day-rec 2 100) (day-rec 3 100)))
      "same start, different divisors, coprime")
  (is (= (c/optimize-event (c/merge-event (day-rec 2 101) (day-rec 2 102)))
         (day-rec 1 101))
      "all remainders covered 1")
  (is (= (c/optimize-event (c/merge-event (day-rec 2 102) (day-rec 2 103)))
         (day-rec 1 102))
      "all remainders covered 2")
  (is (= (c/optimize-event (c/merge-event (day-rec 2 102) (day-rec 2 100)))
         (day-rec 2 100))
      "same divisor, later start, remainder 0")
  (is (= (c/optimize-event (c/merge-event (day-rec 2 103) (day-rec 2 101)))
         (day-rec 2 101))
      "same divisor, later start, remainder 1")
  (is (= (c/optimize-event (c/merge-event (day-rec 2 100) (day-rec 4 100)))
         (day-rec 2 100))
      "a divisor is a multiple of another")
  (is
    (= (c/optimize-event (c/merge-event (day-rec 2 101) (day-rec 4 103)))
       (day-rec 2 101))
    "a divisor is a multiple of another, remainder mod smaller divisor is the same as the other remainder")
  (is (= (c/optimize-event (reduce c/merge-event
                             [(day-rec 3 501) (day-rec 3 502) (day-rec 3 503)]))
         (day-rec 1 501))
      "divisor 3, all remainders covered")
  (is (= (c/optimize-event (reduce c/merge-event
                             [(day-rec 6 1001) (day-rec 6 1003)
                              (day-rec 6 1005)]))
         (day-rec 2 1001))
      "divisor 6, mod 2 remainders covered")
  (is (= (c/optimize-event (reduce c/merge-event
                             [(day-rec 10 200 399) (day-rec 10 400 996)
                              (day-rec 10 1000 1100)]))
         (day-rec 10 200 1100))
      "three segments")
  (is (= (c/optimize-event (c/merge-event (day-rec 10 100 200)
                                          (day-rec 10 125 245)))
         (reduce c/merge-event
           [(day-rec 10 100 120) (day-rec 5 120 200) (day-rec 10 205 245)]))
      "two recs become 3 recs")
  (is (= (c/optimize-event (c/merge-event (day-rec 1 90 91) (day-rec 10 100)))
         (day-rec 10 90))
      "a single-occ rec can be combined with a rec with a different divisor")
  (is (= (c/optimize-event (c/merge-event (day-rec 7 200 206) (day-rec 10 100)))
         (day-rec 10 100))
      "a single-occ rec is a subset of a rec with a different divisor")
  (is
    (= (c/optimize-event (c/merge-event (day-rec 7 200 206)
                                        (day-rec 10 100 500)))
       (day-rec 10 100 500))
    "a single-occ rec is a subset of a rec with a different divisor; with end")
  (is (= (c/optimize-event (c/merge-event (day-rec 7 200 206)
                                          (day-rec 10 100 200)))
         (day-rec 10 100 210))
      "a single-occ rec is a just after a rec with a different divisor")
  (is (= (c/optimize-event (reduce c/merge-event
                             [(day-rec 10 100) (day-rec 1 90 91)
                              (day-rec 5 80 83)]))
         (day-rec 10 80))
      "multiple single recs can be absorbed")
  (is (= (c/optimize-event (reduce c/merge-event
                             [(day-rec 2 198) (day-rec 4 201) (day-rec 4 199)]))
         (day-rec 1 198))
      "requires multi-step reduction in rec-group-reduce-large-period")
  (is (= (c/optimize-event (reduce c/merge-event
                             [(day-rec 3 100) (day-rec 3 102) (day-rec 3 98)]))
         (c/merge-event (day-rec 1 98 99) (day-rec 1 100))))
  (is (= (c/optimize-event (reduce c/merge-event
                             [(day-rec 3 98 99) (day-rec 3 100 101)
                              (day-rec 3 101 102) (day-rec 1 102)]))
         (c/merge-event (day-rec 1 98 99) (day-rec 1 100))))
  (is
    (= (c/optimize-event (reduce c/merge-event
                           [(day-rec 1 101 102) (day-rec 3 104 105)
                            (day-rec 3 105) (day-rec 1 102 103) (day-rec 3 105)
                            (day-rec 3 103 105) (day-rec 1 105)]))
       (day-rec 1 101))
    "case extracted and simplified from week recs, proving we need to recombine again")
  (is (= (c/optimize-event (c/merge-event (week-rec 1 #{1} 701)
                                          (week-rec 1 #{2} 701)))
         (week-rec 1 #{1 2} 701))
      "combine dow")
  (is (= (c/optimize-event (c/merge-event (week-rec 1 #{1} 701 1401)
                                          (week-rec 1 #{2} 701 2101)))
         (c/merge-event (week-rec 1 #{1 2} 701 1401)
                        (week-rec 1 #{2} 1401 2101)))
      "combine dow on overlapping periods")
  (is (= (c/optimize-event (week-rec 1 #{1 6} 699 1403))
         (reduce c/merge-event
           [(week-rec 1 #{6} 694 701) (week-rec 1 #{1 6} 701 1401)
            (week-rec 1 #{1} 1401 1408)]))
      "split incomplete recurrences")
  (is (= (c/optimize-event (c/merge-event (week-rec 2 #{0} 701)
                                          (week-rec 2 #{0} 708)))
         (week-rec 1 #{0} 701))
      "all remainders covered 1")
  (is (= (c/optimize-event (c/merge-event (week-rec 2 #{0} 708)
                                          (week-rec 2 #{0} 715)))
         (week-rec 1 #{0} 708))
      "all remainders covered 2")
  (is (= (c/optimize-event (c/merge-event (week-rec 2 #{0} 708)
                                          (week-rec 2 #{0} (+ 708 14))))
         (week-rec 2 #{0} 708))
      "same divisor, later start 0")
  (is (= (c/optimize-event (c/merge-event (week-rec 2 #{0} 701)
                                          (week-rec 2 #{0} (+ 701 14))))
         (week-rec 2 #{0} 701))
      "same divisor, later start 1")
  (is (= (c/optimize-event (c/merge-event (week-rec 2 #{1 2 3} 701)
                                          (week-rec 4 #{1 3 2} 701)))
         (week-rec 2 #{1 2 3} 701))
      "a divisor is a multiple of another 0")
  (is (= (c/optimize-event (c/merge-event (week-rec 2 #{1 2 3} 701)
                                          (week-rec 4 #{1 3 2} (+ 701 14))))
         (week-rec 2 #{1 2 3} 701))
      "a divisor is a multiple of another 1")
  (is (= (c/optimize-event (reduce c/merge-event
                             [(week-rec 3 #{6} 701) (week-rec 3 #{6} 708)
                              (week-rec 3 #{6} 715)]))
         (week-rec 1 #{6} 701))
      "divisor 3, all remainders covered")
  (is (= (c/optimize-event (reduce c/merge-event
                             [(week-rec 3 #{6} 702) (week-rec 3 #{6} 709)
                              (week-rec 3 #{6} 715)]))
         (week-rec 1 #{6} 701))
      "divisor 3, all remainders covered; start date from middle of a week")
  (is (= (c/optimize-event (reduce c/merge-event
                             [(week-rec 6 #{3} 701)
                              (week-rec 6 #{3} (+ 701 14 14))
                              (week-rec 6 #{3} (+ 701 14))]))
         (week-rec 2 #{3} 701))
      "divisor 6, mod 2 remainders covered")
  (is (= (c/optimize-event (reduce c/merge-event
                             [(week-rec 6 #{4} (+ 701 4))
                              (week-rec 6 #{4} (+ 701 3 14 14))
                              (week-rec 6 #{4} (+ 701 2 14))]))
         (week-rec 2 #{4} 701))
      "divisor 6, mod 2 remainders covered; start date from middle of a week"))
