(ns main
  (:require [user-data]
            [weather]
            [api_routes]
            [org.httpkit.server :as server]))

(defn should-run [conditions]
  (>= (apply * (map val conditions)) 0.5))

(defn run-message [user-score weather-score]
  (if (should-run {:weather weather-score
                   :user user-score})
    "You are in great condition to go for a run"
    "It is recommended that you dont run at this moment"))

(defn -main [& args]
  (let [port (Integer/parseInt (or (System/getenv "PORT") "8080"))]
    (server/run-server #'api_routes/app {:port port})
    (println (str "Running webserver at http:/127.0.0.1:" port "/"))))