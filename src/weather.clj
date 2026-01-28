(ns weather
  (:require
    [clj-http.client :as http]
    [aero.core :refer [read-config]]
    [evaluation_logic]))

(defn get-current-weather [latitude longitude]
  (let [config (read-config "config.edn")
        api-key (:api-key config)
        base-url (:base-url config)
        url (str base-url "/current.json")
        response (http/get url
                           {:query-params {:key api-key
                                           :q (str latitude "," longitude)}
                            :as :json})]
    (:body response)))

(def weather-score-rules
  {:temperature
   [{:op :<= :threshold 0  :score 0}
    {:op :>= :threshold 35 :score 0}
    {:op :<= :threshold 10 :score 0.2}
    {:op :>= :threshold 25 :score 0.5}]

   :precipitation
   [{:op :<= :threshold 0.1 :score 1.0}
    {:op :<= :threshold 0.5 :score 0.6}
    {:op :<= :threshold 1.0 :score 0.3}
    {:else true :score 0}]

   :wind
   [{:op :>= :threshold 50 :score 0}
    {:op :>= :threshold 30 :score 0.3}
    {:op :>= :threshold 15 :score 0.6}]

   :visibility
   [{:op :< :threshold 1 :score 0}
    {:op :< :threshold 2 :score 0.5}]})

(defn calculate-weather-score
  [latitude longitude]
  (let [current-weather (:current (get-current-weather latitude longitude))
        feelslike_c (:feelslike_c current-weather)
        precip_mm (:precip_mm current-weather)
        gust_kph (:gust_kph current-weather)
        vis_km (:vis_km current-weather)

        temp-score (evaluation_logic/score-from-rules feelslike_c (:temperature weather-score-rules))
        precipitation-score (evaluation_logic/score-from-rules precip_mm (:precipitation weather-score-rules))
        wind-score (evaluation_logic/score-from-rules gust_kph (:wind weather-score-rules))
        visibility-score (evaluation_logic/score-from-rules vis_km (:visibility weather-score-rules))

        total (/ (+ temp-score precipitation-score wind-score visibility-score) 4.0)]
    total))
