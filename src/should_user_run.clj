(ns should-user-run
  (:require [user-data]
           [weather]))

(defn should-run [conditions]
  (>= (apply * (map val conditions)) 0.5))

(defn return-run-message [body]
  (let [weather-score 1
        user-score (user-data/get-user-score body)]
    (if (should-run {:weather weather-score
                     :user    user-score})
      "You are in great condition to go for a run"
      "It is recommended that you dont run at this moment")))
