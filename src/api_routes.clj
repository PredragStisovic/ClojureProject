(ns api_routes
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.json :as json]
            [ring.util.response :refer [response]]
            [find_route]
            [should-user-run]))

(defroutes app-routes

           (POST "/should-i-run-api/return-routes" {body :json-params}
             (response (find_route/generate-routes body)))

           (POST "/should-i-run-api/should-user-run/calculate" {body :json-params}
             (response (should-user-run/return-run-message body)))

           (route/resources "/")
           (route/not-found "Not Found"))


(defn wrap-fallback-exception
  [handler]
  #(try
     (handler %)
     (catch Exception e
       {:status 500 :body (.getMessage e)})))

(defn wrap-cors [handler]
  (fn [request]
    (let [resp (handler request)
          headers (get resp :headers {})
          new-headers (merge headers
                             {"Access-Control-Allow-Origin" "http://localhost:3000"
                              "Access-Control-Allow-Methods" "GET,POST,PUT,DELETE,OPTIONS"
                              "Access-Control-Allow-Headers" "Content-Type,Authorization"})]
      (assoc resp :headers new-headers))))


(defroutes options-routes
           (OPTIONS "*" []
             {:status 200
              :headers {"Access-Control-Allow-Origin"  "http://localhost:3000"
                        "Access-Control-Allow-Methods" "GET,POST,PUT,DELETE,OPTIONS"
                        "Access-Control-Allow-Headers" "Content-Type,Authorization"}}))




(def app
  (-> (routes options-routes app-routes)
      (json/wrap-json-params {:keywords? true})
      json/wrap-json-response
      wrap-fallback-exception
      wrap-cors))
