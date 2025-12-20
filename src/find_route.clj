(ns find_route
  (:require [clojure.string :as str]
            [clojure.data.json :as json])
  )


(def PI Math/PI)

(defn create-circle [latitude longitude radius num-of-points]
  (let [radius-long (* (/ 1 (* 111.319  (Math/cos (* PI (/ latitude 180))))) radius)
         radius-lat (* (/ 1 110.574) radius)
         dTheta (/ (* 2 PI) num-of-points)
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

(def polygon (str/join " " (circle-to-polygon (create-circle 40.766479 20.480571 1 16))))

(defn fetch-tracks-from-osm [polygon] (let [query (str "[out:json][timeout:30];"
                                                       "way[highway~'footway|path|track|residential|living_street'](poly:\""
                                                       polygon
                                                       "\");out geom;")]
                                        (slurp (str "https://overpass-api.de/api/interpreter?data=" (java.net.URLEncoder/encode query "UTF-8")))))

(defn haversine-formula [lat1 lat2 long1 long2]
  (let [dlat (/ (* (- lat2 lat1) PI) 180)
        dlong (/ (* (- long2 long1) PI) 180)
        lat1-rad (/ (* lat1 PI) 180)
        lat2-rad (/ (* lat2 PI) 180)
        distance-between-points (+ (Math/pow (Math/sin (/ dlat 2)) 2) (* (Math/pow (Math/sin (/ dlong 2)) 2) (Math/cos lat1-rad) (Math/cos lat2-rad)))
        angular-distance (* 2 (Math/asin (Math/sqrt distance-between-points)))]
    ;6371 is the radius of earth in kilometers
    (* 6371 angular-distance))
  )

(defn add-way-to-graph [graph way]
  (let [geom (:geometry way)]
    (reduce
      (fn [g [p1 p2]]
        (let [c1 [(:lat p1) (:lon p1)]
              c2 [(:lat p2) (:lon p2)]
              distance (haversine-formula (:lat p1) (:lat p2) (:lon p1) (:lon p2))]
          (-> g
              (update c1 (fnil conj []) [c2 distance])
              (update c2 (fnil conj []) [c1 distance]))))
      graph
      (partition 2 1 geom))))

(def osm-json (json/read-str (fetch-tracks-from-osm polygon) :key-fn keyword))

(def ways (:elements osm-json))

(def graph
  (reduce add-way-to-graph {} ways))

(defn find-nearest-node [graph [user-lat user-lon]]
  (apply min-key #(haversine-formula user-lat (first %) user-lon (second %) )
         (keys graph)))

(def start (find-nearest-node graph [40.766479 20.480571]))


(defn find-routes [graph start run-distance]
  (loop [paths {:nodes [start] :distance 0}]

    (let [current-path (first paths)
          final-node (last (:nodes current-path))
          options (get graph final-node [])]
      (let [new-path (for [[option option-distance] options
                           :when (and (not (some (fn [node] (= node option)) (:nodes current-path)))
                                      (<= option-distance (:distance current-path)))
                           ])]))))



(println (find-routes graph start 5))


