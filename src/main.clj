(ns main
  (:require [user-data]
            [weather]))

(defn should-run [conditions]
  (>= (apply * (map val conditions)) 0.5))

(defn run-message [user-score weather-score]
  (if (should-run {:weather weather-score
                   :user user-score})
    "You are in great condition to go for a run"
    "It is recommended that you dont run at this moment"))

(defn -main []
  (let [user-score    (user-data/get-user-score)
        weather-data  (:current (weather/get-current-weather "Belgrade"))
        weather-score (weather/calculate-running-score weather-data)]
    (println (run-message user-score weather-score))))