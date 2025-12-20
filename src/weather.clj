(ns weather
  (:require [clj-http.client :as http]))
  (require '[aero.core :refer [read-config]])
(def config (read-config "config.edn"))

(def api-key (:api-key config))
(def base-url (:base-url config))

(defn get-current-weather [location]
  (let [url (str base-url "/current.json")
        response (http/get url {:query-params {:key api-key :q location} :as :json})]
    (:body response)))

(defn calculate-running-score
  [{:keys [feelslike_c precip_mm gust_kph vis_km]}]

  (let [temp-score
        (cond
          (>= feelslike_c 35) 0
          (<= feelslike_c 10) 0
          (< feelslike_c 5)   0.5
          (> feelslike_c 25)  0.5
          :else               1)

        precipitation-score
        (cond
          (> precip_mm 1.0) 0
          (> precip_mm 0.5) 0.5
          :else             1)

        wind-score
        (cond
          (>= gust_kph 50) 0
          (>= gust_kph 25) 0.5
          :else            1)

        visibility-score
        (cond
          (< vis_km 1) 0
          (< vis_km 2) 0.5
          :else        1)
        total (+ temp-score precipitation-score wind-score visibility-score)
        weighted-res (cond
          (<= total 1.5) 0
          (<= total 2.5) 0.5
          :else 1)
        ]
    weighted-res
    ))
