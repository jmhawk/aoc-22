(ns aoc-22.src
  (:require [clojure.string :as str]))

(def commands
  (->> (map #(str/split % #" ") (-> (slurp "resources/2.txt")
                                   str/split-lines))
      (map (fn [dis]
             [(keyword (first dis)) (Integer/parseInt (last dis))]))))

(take 10 commands)

;;explode on space
;; cast second half to int

(defn part-one
  [commands]
  (let [grouping (group-by first commands)
        forward-sum (:forward grouping)]
    (take 1 grouping)))

(part-one commands)

;;group by up, down, forward
;; sum forward
;; 