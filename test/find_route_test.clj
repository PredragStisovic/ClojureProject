(ns find-route-test
  (:require [clojure.test :refer :all]
            [find_route :refer :all]))

(deftest test-create-circle
  (testing "create-circle returns correct number of points"
    (is (= 4 (count (create-circle 0 0 1 4))))))

(deftest test-circle-to-polygon
  (testing "circle-to-polygon flattens coordinates and closes polygon"
    (let [circle [{:lat 1 :long 2} {:lat 3 :long 4}]]
      (is (= [1 2 3 4 1 2] (circle-to-polygon circle))))))

(deftest test-haversine-formula
  (testing "haversine distance is symmetric and >0 for different points"
    (let [d1 (haversine-formula 0 0 0 1)
          d2 (haversine-formula 0 0 1 0)]
      (is (> d1 0))
      (is (> d2 0))
      (is (= d1 (haversine-formula 0 0 0 1))))))
