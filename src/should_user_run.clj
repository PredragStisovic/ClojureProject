(ns should-user-run
  (:require [user-data]
           [weather]))

(defn score-description [score]
  (cond
    (<= score 0.3) "It is not suggested to run today, as both weather conditions and your latest running data show you need some rest"
    (<= score 0.6) "We recommend you go take it easy if you want to run today"
    (<= score 0.8) "It is recommended you go on a short run today"
    :else "You are good to go, the weather is good and you are in an excellent condition to run!"))

(defn return-run-message [body]
  (let [weather-score (weather/calculate-weather-score (:latitude body) (:longitude body))
        user-score (user-data/get-user-score body)
        combined (+ (* weather-score 0.6)
                    (* user-score 0.4))]
    {:score combined
     :text-description (score-description combined)}))
