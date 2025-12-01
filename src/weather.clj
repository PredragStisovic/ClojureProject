(ns weather
  (:require [clj-http.client :as http]))
  (require '[aero.core :refer [read-config]])
(def config (read-config "config.edn"))

(def api-key (:api-key config))
(def base-url (:base-url config))

(defn get-current-weather [location]
  (let [url (str base-url "/current.json")
        response (http/get url {:query-params {:key api-key :q location}})]
    (println "Response:" (:body response))
    (:body response)))

(get-current-weather "Belgrade")