(ns smartcal.core-test
  (:require [cljs.test :refer (deftest is)]
            [instaparse.core :as insta]
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
         {:weeks-to-show 2, :start-date {:y 1999, :m 9, :d 9}}))
  (is (= (c/load-state {:weeks-to-show 2, :start-date {:y 1, :m 1, :d 1}})
         {:weeks-to-show 2}))
  (is (= (c/load-state {:weeks-to-show 2, :start-date {:y 1999, :m 12, :d 1}})
         {:weeks-to-show 2})))

(deftest control-language
  (is (= (insta/parses c/cmdline-parser "goto 20241001")
         [[:cmd
           [:goto-cmd
            [:date-lit [:yyyy-lit "2024"] [:mm-lit "10"] [:dd-lit "01"]]]]]))
  (is (= (insta/parses c/cmdline-parser "goto 2024-10-01")
         [[:cmd
           [:goto-cmd
            [:date-lit [:yyyy-lit "2024"] [:mm-lit "10"] [:dd-lit "01"]]]]]))
  (is (= (insta/parses c/cmdline-parser "goto 2024  10       01")
         [[:cmd
           [:goto-cmd
            [:date-lit [:yyyy-lit "2024"] [:mm-lit "10"] [:dd-lit "01"]]]]]))
  (is (= (insta/parses c/cmdline-parser "goto Oct 1, 2024")
         [[:cmd
           [:goto-cmd
            [:date-lit [:mmm-lit "Oct"] [:d-lit "1"] [:yyyy-lit "2024"]]]]]))
  (is (= (insta/parses c/cmdline-parser "goto Oct 12, 2024")
         [[:cmd
           [:goto-cmd
            [:date-lit [:mmm-lit "Oct"] [:d-lit "12"] [:yyyy-lit "2024"]]]]]))
  (is (= (insta/parses c/cmdline-parser "goto 01 Oct 2024")
         [[:cmd
           [:goto-cmd
            [:date-lit [:d-lit "01"] [:mmm-lit "Oct"] [:yyyy-lit "2024"]]]]])))
