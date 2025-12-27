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

(def generated-circle (create-circle 43.84568 20.03683 5 8))


(def polygon (str/join " " (circle-to-polygon generated-circle)))

(defn fetch-tracks-from-osm [polygon] (let [query (str "[out:json][timeout:60];"
                                                       "way[highway~'footway|path|track'](poly:\""
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



(def start (find-nearest-node graph [43.84568 20.03683]))

(def closest-nodes
  (map
    (fn [{:keys [lat long]}]
      (find-nearest-node graph [lat long]))
    (take-nth 2 generated-circle)))

;(defn find-routes [graph start run-distance]
;  (loop [paths [{:nodes [start] :distance 0}]
;         all-routes []]
;
;    (if (empty? paths)
;      all-routes
;      (let [current-path (first paths)
;            rest-paths   (rest paths)
;            final-node   (last (:nodes current-path))
;            options      (get graph final-node [])
;
;            new-paths
;            (for [[option option-distance] options
;                  :let [new-distance (+ (:distance current-path) option-distance)]
;                  :when (and (not (some #{option} (:nodes current-path)))
;                             (<= new-distance run-distance))]
;              {:nodes (conj (:nodes current-path) option)
;               :distance new-distance})
;
;            updated-routes
;            (if (and (> (:distance current-path) 0)
;                     (<= (:distance current-path) run-distance))
;              (if (empty? new-paths)
;                (conj all-routes current-path)
;                all-routes)
;              all-routes)]
;
;        (recur (concat rest-paths new-paths)
;               updated-routes)))))
;
;
;
;(println (find-routes graph start 4))

(defn a-star-path-search [])

(defn routes-to-gpx [routes]
  (str
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<gpx version=\"1.1\" creator=\"find-route\" "
    "xmlns=\"http://www.topografix.com/GPX/1/1\">\n"
    (apply str
           (map-indexed
             (fn [i {:keys [nodes distance]}]
               (str
                 "  <trk>\n"
                 "    <name>Route " (inc i)
                 " (" (format "%.2f" distance) " km)</name>\n"
                 "    <trkseg>\n"
                 (apply str
                        (for [[lat lon] nodes]
                          (format "      <trkpt lat=\"%.7f\" lon=\"%.7f\" />\n"
                                  lat lon)))
                 "    </trkseg>\n"
                 "  </trk>\n"))
             routes))
    "</gpx>\n"))

;(spit "routes.gpx" (routes-to-gpx (find-routes graph start 4)))

