(ns api_routes
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.json :as json]
            [ring.util.response :refer [response]]
            [find_route]))

(defroutes app-routes

           (POST "/should-i-run-api/return-routes" {body :json-params}
             (response (find_route/generate-routes body)))

           (route/resources "/")
           (route/not-found "Not Found"))


(defn wrap-fallback-exception
  [handler]
  #(try
     (handler %)
     (catch Exception e
       {:status 500 :body (.getMessage e)})))


(def app
  (-> app-routes
      (json/wrap-json-params {:keywords? true})
      json/wrap-json-response
      wrap-fallback-exception))