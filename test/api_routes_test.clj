(ns api_routes_test
  (:require [clojure.test :refer :all]
            [ring.mock.request :as mock]
            [cheshire.core :as json]
            [api_routes :refer [app]]))

(deftest post-return-routes-success
  (testing "POST /return-routes returns JSON response"
    (with-redefs [find_route/generate-routes
                  (fn [_]
                    {:start [1 2]
                     :distance 5
                     :routes []})]

      (let [payload {:start [1 2] :distance 5}
            response (app
                       (-> (mock/request :post "/should-i-run-api/return-routes")
                           (mock/json-body payload)))
            body (json/parse-string (:body response) true)]

        (is (= 200 (:status response)))
        (is (.startsWith
              (get-in response [:headers "Content-Type"])
              "application/json"))
        (is (= [] (:routes body)))))))

