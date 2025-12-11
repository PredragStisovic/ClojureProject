(ns find_route
  (:require [clojure.string :as str]))

(defn create-circle [latitude longitude radius num-of-points]
  (let [radius-long (* (/ 1 (* 111.319  (Math/cos (* Math/PI (/ latitude 180))))) radius)
         radius-lat (* (/ 1 110.574) radius)
         dTheta (/ (* 2 Math/PI) num-of-points)
         ]
    (reduce (fn [acc e] (let [theta (* dTheta e)
                               new-lat (+ latitude (* radius-lat (Math/sin theta)))
                               new-long (+ longitude (* radius-long (Math/cos theta)))
                               ]
                           (conj acc {:lat new-lat :long new-long}))
                          )
            []
            (range num-of-points)))
  )

(defn circle-to-polygon [circle] (let [coordinates (apply concat (map
                                                       vals circle))
                                       coordinates-closed (concat coordinates (take 2 coordinates))
                                       ]
                                   coordinates-closed))

(def polygon (str/join " " (circle-to-polygon (create-circle 40.5 20.5 10 32))))

(defn fetch-tracks-from-osm [polygon] (let [query (str "[out:json];"
                                                       "way[highway~'footway|path|track|residential|living_street'](poly:\""
                                                       polygon
                                                       "\");out geom;")]
                                        (slurp (str "https://overpass-api.de/api/interpreter?data=" (java.net.URLEncoder/encode query "UTF-8")))))




(println (fetch-tracks-from-osm polygon))