(ns user-data
    (:require [malli.core :as m]
              [java-time.api :as jt]
              [evaluation_logic]))


(def Run
    [:map
     [:distance [:double {:min 0}]]
     [:moving_time [:int {:min 0}]]
     [:start_date :string]])

(def training-score-rules
  {:recency
   [{:op :=  :threshold 0 :score 0.0}
    {:op :=  :threshold 1 :score 0.3}
    {:op :=  :threshold 2 :score 0.6}
    {:range [3 5] :score 1.0}
    {:op :=  :threshold 6 :score 0.8}
    {:op :=  :threshold 7 :score 0.6}
    {:else true :score 0.4}]

   :intensity
   [{:op :< :threshold 200 :score 1.0}
    {:op :< :threshold 400 :score 0.7}
    {:op :< :threshold 600 :score 0.4}
    {:else true :score 0.2}]

   :consistency
   [{:op := :threshold 1 :score 0.6}
    {:range [2 3] :score 1.0}
    {:range [4 5] :score 0.8}
    {:else true :score 0.5}]})

(def score-weights
  {:recency 0.45
   :intensity 0.25
   :consistency 0.20
   :baseline 0.10})



(defn get-user-score [body]
  (let [runs (:runs body)
        latest-run (first runs)]

    (if (or (empty? runs)
            (not (m/validate Run latest-run)))
      0.0

      (let [days-since-last-run
            (jt/time-between
              (jt/instant (:start_date latest-run))
              (jt/instant)
              :days)

            intensity (* (/ (:moving_time latest-run) 60)
                         (/ (:distance latest-run) 1000))

            num-of-runs (count runs)

            recency-score
            (evaluation_logic/score-from-rules
              days-since-last-run
              (:recency training-score-rules))

            intensity-score
            (evaluation_logic/score-from-rules
              intensity
              (:intensity training-score-rules))

            consistency-score
            (evaluation_logic/score-from-rules
              num-of-runs
              (:consistency training-score-rules))]

        (+ (* (:recency score-weights) recency-score)
           (* (:intensity score-weights) intensity-score)
           (* (:consistency score-weights) consistency-score)
           (* (:baseline score-weights) 0.2))))))


