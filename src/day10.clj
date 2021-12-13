(ns day10
  (:require [clojure.string :refer [split-lines]]
            [clojure.test :as test]))

(def test-data (->> (slurp "resources/day10_test.txt")
                    split-lines))

(def starting-m
  {\< 0
   \[ 0
   \( 0
   \{ 0})

(def scoreboard 0)

(defn get-pts [c]
  (case c
    \) 3
    \] 57
    \} 1197
    \> 25137))

(defn score!
  [c]
  (let [pts (get-pts c)]
    (+ scoreboard pts)))

(def close->open
  {\} \{
   \] \[
   \) \(
   \> \<})

(defn update-map
  [m c]
  (case c
    (\[ \{ \( \<) (update m c inc)
    (\] \} \) \>) (update m (get close->open c) dec)))

(defn check-map
  [m c]
  (< (get m c) 0))

(defn process-check
  [m c]
  (let [boom? (check-map m c)]
    (if boom?
      {:boom? true
       :c c}
      m)))

(defn handle-check
  [m]
  (if (:boom? m)))

  (defn process-symbol
    [m c failures]
    (-> m
        (update-map c)
        (process-check c)
        (handle-check failures)))

(defn process-row
  [row]
  (let [m starting-m]
    (map process-symbol row)))

(test/deftest part1
  (test/is (let [test-m {\[ 0}] (= 1 (-> (update-map test-m \[)
                                         (get \[)))))
  (test/is (let [test-m {\[ 0}] (= -1 (-> (update-map test-m \])
                                          (get \[)))))
  (test/is (let [test-m {\[ 0}] ( check-map test-m \[))))

(comment

  (def sample-line "[{<>}]")
  
  (get close->open \])


;
  )