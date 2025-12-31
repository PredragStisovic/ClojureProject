(ns find_route
  (:require [clojure.string :as str]
            [clojure.data.json :as json]))

(def PI Math/PI)

(defn create-circle [latitude longitude radius num-of-points]
  (let [radius-long (* (/ 1 (* 111.319 (Math/cos (* PI (/ latitude 180))))) radius)
        radius-lat (* (/ 1 110.574) radius)
        dTheta (/ (* 2 PI) num-of-points)]
    (reduce (fn [acc e]
              (let [theta (* dTheta e)
                    new-lat (+ latitude (* radius-lat (Math/sin theta)))
                    new-long (+ longitude (* radius-long (Math/cos theta)))]
                (conj acc {:lat new-lat :long new-long})))
            []
            (range num-of-points))))

(defn circle-to-polygon [circle]
  (let [coordinates (apply concat (map vals circle))
        coordinates-closed (concat coordinates (take 2 coordinates))]
    coordinates-closed))

(defn fetch-tracks-from-osm [polygon]
  (let [query (str "[out:json][timeout:60];"
                   "way[highway~'footway|path|track|residential|living_street'](poly:\""
                   polygon
                   "\");out geom;")]
    (slurp (str "https://overpass-api.de/api/interpreter?data="
                (java.net.URLEncoder/encode query "UTF-8")))))

(defn haversine-formula [lat1 lat2 long1 long2]
  (let [dlat (/ (* (- lat2 lat1) PI) 180)
        dlong (/ (* (- long2 long1) PI) 180)
        lat1-rad (/ (* lat1 PI) 180)
        lat2-rad (/ (* lat2 PI) 180)
        distance-between-points (+ (Math/pow (Math/sin (/ dlat 2)) 2)
                                   (* (Math/pow (Math/sin (/ dlong 2)) 2)
                                      (Math/cos lat1-rad)
                                      (Math/cos lat2-rad)))
        angular-distance (* 2 (Math/asin (Math/sqrt distance-between-points)))]
    ;; 6371 is the radius of earth in kilometers
    (* 6371 angular-distance)))

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

(defn find-nearest-node [graph [user-lat user-lon]]
  (apply min-key #(haversine-formula user-lat (first %) user-lon (second %))
         (keys graph)))


(defn reconstruct-path [came-from current]
  (loop [curr current
         path [current]]
    (if-let [prev (came-from curr)]
      (recur prev (conj path prev))
      (reverse path))))

(defn a-star
  [graph start goal distance]
  (let [heuristic (fn [[lat lon]]
                    (haversine-formula lat (first goal)
                                       lon (second goal)))]
    (loop [open-set #{start}
           came-from {}
           g-score {start 0}
           f-score {start (heuristic start)}]

      (if (empty? open-set)
        nil
        (let [current (apply min-key #(get f-score % Double/POSITIVE_INFINITY) open-set)]

          (if (or (= current goal) (>= (g-score current) distance))
            {:nodes (reconstruct-path came-from current)
             :distance (g-score current)}

            (let [open-set-two (disj open-set current)]
              (recur
                (reduce
                  (fn [new-set [neighbor distance]]
                    (let [tentative (+ (g-score current) distance)]
                      (if (< tentative (get g-score neighbor Double/POSITIVE_INFINITY))
                        (conj new-set neighbor)
                        new-set)))
                  open-set-two
                  (get graph current []))

                (reduce
                  (fn [predecessor-map [neighbor distance]]
                    (let [tentative (+ (g-score current) distance)]
                      (if (< tentative (get g-score neighbor Double/POSITIVE_INFINITY))
                        (assoc predecessor-map neighbor current)
                        predecessor-map)))
                  came-from
                  (get graph current []))

                (reduce
                  (fn [cost-so-far [neighbor distance]]
                    (let [tentative (+ (g-score current) distance)]
                      (if (< tentative (get cost-so-far neighbor Double/POSITIVE_INFINITY))
                        (assoc cost-so-far neighbor tentative)
                        cost-so-far)))
                  g-score
                  (get graph current []))

                (reduce
                  (fn [estimated-total-cost [neighbor distance]]
                    (let [tentative (+ (g-score current) distance)]
                      (if (< tentative (get g-score neighbor Double/POSITIVE_INFINITY))
                        (assoc estimated-total-cost neighbor (+ tentative (heuristic neighbor)))
                        estimated-total-cost)))
                  f-score
                  (get graph current []))))))))))

(defn find-nodes-at-distance [graph start target-distance tolerance]
  (->> (keys graph)
       (remove #(= % start))
       (map (fn [node]
              [node (haversine-formula (first start) (first node)
                                       (second start) (second node))]))
       (filter (fn [[_ distance]]
                 (<= (Math/abs (- distance target-distance)) tolerance)))
       (sort-by second)
       (map first)))

(defn find-routes [graph start targets max-distance]
  (->> targets
       (map #(a-star graph start % max-distance))
       (remove nil?)))

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

(defn generate-routes
  [{:keys [lat lon distance]}]

  (let [radius-km distance
        num-points 8
        circle (create-circle lat lon radius-km num-points)
        polygon (str/join " " (circle-to-polygon circle))
        osm-json (json/read-str
                   (fetch-tracks-from-osm polygon)
                   :key-fn keyword)
        ways (:elements osm-json)
        graph (reduce add-way-to-graph {} ways)
        start (find-nearest-node graph [lat lon])
        targets (find-nodes-at-distance
                  graph
                  start
                  distance
                  0.5)
        routes (find-routes graph start (take 10 targets) distance)]


    {:start [lat lon]
     :distance distance
     :routes routes}))
