(ns weather_test
  (:require [clojure.test :refer :all]
            [weather]
            [clj-http.client :as http]))

(deftest calculate-weather-score-perfect-weather
  (testing "Is score equal to one, i.e. are running conditions ideal"
    (with-redefs [weather/get-current-weather
                  (fn [_ _]
                    {:current
                     {:feelslike_c 20
                      :precip_mm 0
                      :gust_kph 10
                      :vis_km 10}})]
      (is (= 1.0
             (weather/calculate-weather-score 44.8 20.4))))))

(deftest calculate-weather-score-bad-weather
  (testing "Is score equal to zero, i.e. very bad running conditions"
    (with-redefs [weather/get-current-weather
                  (fn [_ _]
                    {:current
                     {:feelslike_c 40
                      :precip_mm 3.0
                      :gust_kph 60
                      :vis_km 0.5}})]
      (is (= 0.0
             (weather/calculate-weather-score 44.8 20.4))))))


(deftest get-current-weather-test
  (testing "Does API call return parsed weather data"
    (with-redefs
      [aero.core/read-config
       (fn [_]
         {:api-key "test"
          :base-url "http://test.com"})

       http/get
       (fn [_ _]
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
             (weather/get-current-weather 44.82295 20.45821))))))



