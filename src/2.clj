(ns aoc-22.src
  (:require [clojure.string :as str]))

(def commands
  (->> (map #(str/split % #" ") (-> (slurp "resources/2.txt")
                                   str/split-lines))
      (map (fn [dis]
             [(keyword (first dis)) (Integer/parseInt (last dis))]))))

(take 10 commands)

(defn part-one
  [commands]
  (let [{:keys [forward up down]} (group-by first commands)
        forward-sum (reduce (fn [counter entry] (+ counter (last entry))) 0 forward)
        up-sum (reduce (fn [counter entry] (+ counter (last entry))) 0 up)
        down-sum (reduce (fn [counter entry] (+ counter (last entry))) 0 down)]
    (* forward-sum (- down-sum up-sum))))

(part-one commands)


(:a [[:a [1 2 3]] [:b [8 9 10]]])
;;group by up, down, forward
;; sum forward
;; 