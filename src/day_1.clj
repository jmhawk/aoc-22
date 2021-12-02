(ns aoc_22.src)

(defn part-1 
    [data]
    (let
     [results
      (reduce
       (fn [acc next] 
        (if
         (> next (:latest acc))
          {:counter (inc (:counter acc))
           :latest next}
          {:counter (:counter acc)
           :latest next}))
       {:counter 0
        :latest (first data)}
       (rest data))]))

(defn part-2
    []
    false)