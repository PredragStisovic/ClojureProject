(ns main
  (:require [api_routes]
            [org.httpkit.server :as server]))

(defn -main [& args]
  (let [port (Integer/parseInt (or (System/getenv "PORT") "8080"))]
    (server/run-server #'api_routes/app {:port port})
    (println (str "Running webserver at http:/127.0.0.1:" port "/"))))