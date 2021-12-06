(ns aoc-22.src
  (:require [clojure.string :as str]))

(def commands
  (->> (map #(str/split % #" ") (-> (slurp "resources/2.txt")
                                   str/split-lines))
      (map (fn [dis]
             [(keyword (first dis)) (Integer/parseInt (last dis))]))))

(defn part-one
  [command-coll]
  (let [{:keys [forward up down]} (group-by first command-coll)
        forward-sum (reduce (fn [counter entry] (+ counter (last entry))) 0 forward)
        up-sum (reduce (fn [counter entry] (+ counter (last entry))) 0 up)
        down-sum (reduce (fn [counter entry] (+ counter (last entry))) 0 down)]
    (* forward-sum (- down-sum up-sum))))

;;;;;;;; Part 2 ;;;;;;;;;

(def starting-state {:depth 0
                     :aim 0
                     :horiz 0})

(defn forward!
  [{:keys [horiz depth aim]} command-num]
  {:depth (+ depth (* aim command-num))
   :horiz (+ horiz command-num)
   :aim aim})

(defn up!
  [state command-num]
  (update state :aim (fn [x] (- x command-num))))

(defn down!
  [state command-num]
  (update state :aim (fn [x] (+ x command-num))))

(defn process-command
  [state command]
  (case (first command)
    :forward (forward! state (last command))
    :up (up! state (last command))
    :down (down! state (last command))))

(defn part2 
  [command-coll]
  (let [{:keys [horiz depth] :as final-state} (reduce
                                               process-command
                                               starting-state
                                               command-coll)]
    (* depth horiz)))
