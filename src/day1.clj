(ns day1
    (:require [clojure.string :as str]))

(def measurements 
    (map #(Integer/parseInt %) (-> (slurp "resources/1.txt")
                                   str/split-lines)))

(defn verdict 
    [grouping]
    (> (last grouping) (first grouping)))

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
    (->> measurements
        (partition 4 1)
        (filter verdict)
        count))