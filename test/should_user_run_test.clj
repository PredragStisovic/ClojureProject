(ns should_user_run_test
  (:require
    [clojure.test :refer :all]
    [should-user-run :as sur]
    [weather]
    [user-data]))

(deftest score-description-test
  (testing "Score <= 0.3"
    (is (= "It is not suggested to run today, as both weather conditions and your latest running data show you need some rest"
           (sur/score-description 0.2))))

  (testing "Score <= 0.6"
    (is (= "We recommend you go take it easy if you want to run today"
           (sur/score-description 0.5))))

  (testing "Score <= 0.8"
    (is (= "It is recommended you go on a short run today"
           (sur/score-description 0.75))))

  (testing "Score > 0.8"
    (is (= "You are good to go, the weather is good and you are in an excellent condition to run!"
           (sur/score-description 0.9)))))


(deftest return-run-message-test
  (testing "Combines weather and user score correctly"
    (with-redefs [weather/calculate-weather-score (fn [_ _] 0.8)
                  user-data/get-user-score          (fn [_] 0.6)]
      (let [body {:latitude 45.0
                  :longitude 19.0}
            result (sur/return-run-message body)]

        (is (= 0.72 (:score result)))
        (is (= "It is recommended you go on a short run today"
               (:text_description result))))))

  (testing "High scores produce 'good to go' message"
    (with-redefs [weather/calculate-weather-score (fn [_ _] 1.0)
                  user-data/get-user-score          (fn [_] 1.0)]
      (let [result (sur/return-run-message {:latitude 0 :longitude 0})]
        (is (= 1.0 (:score result)))
        (is (= "You are good to go, the weather is good and you are in an excellent condition to run!"
               (:text_description result)))))))
