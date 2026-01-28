(ns evaluation_logic)


(defn evaluate-rule [value {:keys [range op threshold score else]}]
  (cond
    else score

    range (let [[min max] range]
            (when (<= min value max) score))
    (= op :=) (when (= value threshold) score)
    (= op :<)  (when (< value threshold) score)
    (= op :<=) (when (<= value threshold) score)
    (= op :>)  (when (> value threshold) score)
    (= op :>=) (when (>= value threshold) score)))

(defn score-from-rules [value rules]
  (or (some #(evaluate-rule value %) rules) 1))
