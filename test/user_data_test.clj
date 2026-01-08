(ns user_data_test
  (:require [clojure.test :refer :all]
            [java-time.api :as jt]
            [user-data :refer [get-user-score]]))

(defn run
  [{:keys [days-ago distance moving-time]}]
  {:distance distance
   :moving_time moving-time
   :start_date (jt/format :iso-instant
                          (jt/minus (jt/instant) (jt/days days-ago)))})


(defn body-with-runs [& runs]
  {:runs runs})


(deftest high-intensity-lowers-score
  (let [low-intensity (get-user-score
                        (body-with-runs
                          (run {:days-ago 2
                                :distance 3000.0
                                :moving-time 1200})))
        high-intensity (get-user-score
                         (body-with-runs
                           (run {:days-ago 2
                                 :distance 15000.0
                                 :moving-time 1800})))]
    (is (> low-intensity high-intensity))))


(deftest more-runs-improve-consistency
  (let [one-run (get-user-score
                  (body-with-runs
                    (run {:days-ago 2 :distance 5000 :moving-time 1800})))

        three-runs (get-user-score
                     (body-with-runs
                       (run {:days-ago 2 :distance 5000.0 :moving-time 1800})
                       (run {:days-ago 4 :distance 4000.0 :moving-time 1600})
                       (run {:days-ago 6 :distance 3000.0 :moving-time 1400})))]
    (is (< one-run three-runs))))

(deftest recency-affects-score
  (let [today (get-user-score
                (body-with-runs
                  (run {:days-ago 0 :distance 5000.0 :moving-time 1800})))
        three-days (get-user-score
                     (body-with-runs
                       (run {:days-ago 3 :distance 5000.0 :moving-time 1800})))]
    (is (< today three-days))))
