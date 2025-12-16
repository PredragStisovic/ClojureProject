(ns weather_test
  (:require [clojure.test :refer :all]
            [weather]
            [clj-http.client :as http]))

(deftest calculate-running-score-perfect-weather
  (testing "Is score equal to one, i.e. are running conditions ideal"
    (is (= 1
           (weather/calculate-running-score
             {:feelslike_c 20
              :precip_mm 0
              :gust_kph 10
              :vis_km 10})))))


(deftest calculate-running-score-bad-weather
  (testing "Is score equal to zero, i.e. very bad running conditions"
    (is (= 0
           (weather/calculate-running-score
             {:feelslike_c 40
              :precip_mm 3.0
              :gust_kph 60
              :vis_km 0.5})))))

(deftest get-current-weather-test
  (testing "Does api call return parsed weather data"
    (with-redefs [http/get (fn [url {}]
                             {:body {:current
                                     {:feelslike_c 20
                                      :precip_mm 0
                                      :gust_kph 5
                                      :vis_km 10}}})]
      (is (= {:current
              {:feelslike_c 20
               :precip_mm 0
               :gust_kph 5
               :vis_km 10}}
             (weather/get-current-weather "Belgrade"))))))


