(ns main
  (:require [user-data])
  (:require [weather]))

(defn should-run [conditions]
  (>= (apply * (map val conditions)) 0.5))

(def user-score (user-data/get-user-score))
(def weather  (:current (weather/get-current-weather "Belgrade")))
(def weather-score (weather/calculate-running-score weather))

(print (should-run {:weather weather-score :user user-score }))