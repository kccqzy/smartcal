(ns smartcal.core-test
  (:require [cljs.test :refer (deftest is)]
            [smartcal.core :as c]))

(deftest actual-start
  (is (= (c/actual-start {:y 2024, :m 9, :d 11}) {:y 2024, :m 9, :d 6}))
  (is (= (c/actual-start {:y 2024, :m 0, :d 1}) {:y 2023, :m 11, :d 31})))

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
  (is (thrown? js/Error (c/nd-weekday-of-month 5 2 9 2024)) "occurrence out of bounds")
  (is (= (c/nd-weekday-of-month 4 9 9 2024) 29))
  (is (= (c/nd-weekday-of-month -1 2 9 2024) 29))
  (is (= (c/nd-weekday-of-month -2 2 9 2024) 22))
  (is (= (c/nd-weekday-of-month -3 2 9 2024) 15))
  (is (= (c/nd-weekday-of-month -4 2 9 2024) 8))
  (is (= (c/nd-weekday-of-month -5 2 9 2024) 1))
  (is (thrown? js/Error (c/nd-weekday-of-month -6 2 9 2024)) "negative occurrence out of bounds"))
