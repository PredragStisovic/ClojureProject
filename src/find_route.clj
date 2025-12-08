(ns find_route)

(defn create-circle [latitude longitude radius num-of-points]
  (let [radius-long (* (/ 1 (* 111.319  (Math/cos latitude))) radius)
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