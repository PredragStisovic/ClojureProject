(ns main)

(defn should-run [conditions]
  (>= (apply * (map val conditions)) 0.5))


(print (should-run {:weather 1 :health 0.5 :rest 0.25 }))