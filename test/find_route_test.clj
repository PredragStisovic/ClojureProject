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

(deftest test-add-way-to-graph
  (testing "add-way-to-graph adds bidirectional edges"
    (let [way {:geometry [{:lat 0 :lon 0}
                          {:lat 0 :lon 1}]}
          graph (add-way-to-graph {} way)]
      (is (contains? graph [0 0]))
      (is (contains? graph [0 1]))
      (is (= 1 (count (get graph [0 0]))))
      (is (= 1 (count (get graph [0 1])))))))

(deftest test-find-nearest-node
  (testing "find-nearest-node returns closest graph node"
    (let [graph {[0 0] []
                 [0 5] []
                 [5 5] []}]
      (is (= [0 0]
             (find-nearest-node graph [0.1 0.1]))))))

(deftest test-reconstruct-path
  (testing "reconstruct-path rebuilds path correctly"
    (let [came-from {[0 2] [0 1]
                     [0 1] [0 0]}]
      (is (= [[0 0] [0 1] [0 2]]
             (reconstruct-path came-from [0 2]))))))

(deftest test-a-star-basic
  (testing "a-star finds shortest path"
    (let [graph {[0 0] [[[0 1] 1]]
                 [0 1] [[[0 0] 1] [[0 2] 1]]
                 [0 2] [[[0 1] 1]]}
          result (a-star graph [0 0] [0 2] 2)]
      (is (= 2 (:distance result)))
      (is (= [[0 0] [0 1] [0 2]]
             (:nodes result))))))

(deftest test-find-nodes-at-distance
  (testing "find-nodes-at-distance finds nodes within tolerance"
    (let [graph {[0 0] []
                 [0 0.009] []
                 [0 0.018] []}
          start [0 0]
          nodes (find-nodes-at-distance graph start 1 0.2)]
      (is (= [[0 0.009]] nodes)))))

(deftest test-find-routes
  (testing "find-routes returns valid routes"
    (let [graph {[0 0] [[[0 1] 1]]
                 [0 1] [[[0 0] 1] [[0 2] 1]]
                 [0 2] [[[0 1] 1]]}
          routes (find-routes graph [0 0] [[0 2]] 5)]
      (is (= 1 (count routes)))
      (is (= 2 (:distance (first routes)))))))