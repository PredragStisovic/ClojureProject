(ns main-test
  (:require [clojure.test :refer :all]
            [main]))

(deftest should-run-test
  (testing "returns true when the score is >= 0.5"
    (is (true? (main/should-run {:weather 1 :user 0.6})))))

(deftest should-not-run-test
  (testing "returns false when the score is < 0.5"
    (is (false? (main/should-run {:weather 0.5 :user 0.5})))))
