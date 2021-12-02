(ns aoc-22.src
    (:require [clojure.string :as str]))

(def measurements 
    (map #(Integer/parseInt %) (-> (slurp "resources/1.txt")
                                   str/split-lines)))

(defn part-1 
    [measurements]
    (let
     [results (reduce
               (fn [acc next]
                 (if
                  (> next (:latest acc))
                   {:counter (inc (:counter acc))
                    :latest next}
                   {:counter (:counter acc)
                    :latest next}))
               {:counter 0
                :latest (first measurements)}
               (rest measurements))]
      (:counter results)))

(defn part-2
    [measurements]
    )

(part-1 measurements)
;; how often is the sum of three in a row greater than the sum of the next three-grouping

;;n0 + n1 + n2
;;n1 + n2 + n3
;;n2 + n3 + n4 

;; ;;n0 + n1 + n2 > ;;n1 + n2 + n3?

;; ;;n1 + n2 + n3 > ;;n2 + n3 + n4 ?