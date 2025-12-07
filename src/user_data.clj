(ns user-data)

(defn get-user-score []
  (println "How many hours did you sleep last night?")
  (let [hours-slept (parse-long (read-line))
        sleep-score (cond (> hours-slept 8) 1
                          (> hours-slept 6) 0.5
                          :else 0)

        _ (println "How many kilometers did you run last time?")
        kilometers-ran (parse-long (read-line))
        run-score (cond (>= kilometers-ran 15) 0
                        (> kilometers-ran 10) 0.5
                        :else 1)

        _ (println "Do you feel pain in legs, chest or joints? (Yes/No)")
        pain-input (read-line)
        pain-score (if (= pain-input "Yes") 0 1)

        total (+ sleep-score run-score pain-score)]
    (cond
      (> total 2) 1
      (> total 1) 0.5
      :else 0)))
