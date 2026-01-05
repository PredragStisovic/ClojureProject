(ns weather
  (:require
    [clj-http.client :as http]
    [aero.core :refer [read-config]]))

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

(defn calculate-weather-score
  [latitude longitude]
  (let [current-weather (:current (get-current-weather latitude longitude))
        feelslike_c (:feelslike_c current-weather)
        precip_mm (:precip_mm current-weather)
        gust_kph (:gust_kph current-weather)
        vis_km (:vis_km current-weather)

        temp-score
        (cond
          (<= feelslike_c 0) 0
          (>= feelslike_c 35) 0
          (<= feelslike_c 10) 0.2
          (>= feelslike_c 25) 0.5
          :else 1)

        precipitation-score
        (cond
          (> precip_mm 2.0) 0
          (> precip_mm 1.0) 0.3
          (> precip_mm 0.5) 0.6
          :else 1)

        wind-score
        (cond
          (>= gust_kph 50) 0
          (>= gust_kph 30) 0.3
          (>= gust_kph 15) 0.6
          :else 1)

        visibility-score
        (cond
          (< vis_km 1) 0
          (< vis_km 2) 0.5
          :else 1)

        total (/ (+ temp-score precipitation-score wind-score visibility-score) 4.0)]
    total))
