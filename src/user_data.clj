(ns user-data
  (:require [malli.core :as m]
            [java-time.api :as jt]))


(def Run
  [:map
   [:distance [:double {:min 0}]]
   [:moving_time [:int {:min 0}]]])

(def ReadyScore
  [:map
   [:score [:double {:min 0.0 :max 1.0}]]
   [:text-description [:enum :go :easy :short :rest]]])


(defn get-user-score [body]
  (let [runs (:runs body)
        latest-run (first runs)]
    (when (m/validate Run latest-run)
      (let [days-since-last-run (jt/time-between (jt/instant (:start_date latest-run)) (jt/instant) :days)
            intensity (* (/ (:moving_time latest-run) 60) (/ (:distance latest-run) 1000))
            num-of-runs (count runs)
            recency-score (cond
              (= days-since-last-run 0) 0.0
              (= days-since-last-run 1) 0.3
              (= days-since-last-run 2) 0.6
              (<= 3 days-since-last-run 5) 1.0
              (= days-since-last-run 6) 0.8
              (= days-since-last-run 7) 0.6
              :else 0.4)
            intensity-score (cond
              (< intensity 200) 1.0
              (< intensity 400) 0.7
              (< intensity 600) 0.4
              :else 0.2)
            consistency-score (cond
            (= num-of-runs 0) 0.3
            (= num-of-runs 1) 0.6
            (<= 2 num-of-runs 3) 1.0
            (<= 4 num-of-runs 5) 0.8
            :else 0.5)


            ]
        (+ (* 0.45 recency-score)
           (* 0.25 intensity-score)
           (* 0.20 consistency-score)
           (* 0.10 0.2))))
    ))
