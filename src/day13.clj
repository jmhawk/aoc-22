(ns day13
  (:require [clojure.test :refer :all]
            [clojure.string :as str]))

;;
;; distance

  ;;(read-points!) => [[6 10] [0 14] ... [x y]]
  ;;  -- receive series of fold instructions
  ;;  (process-fold "fold along y=9") => [[6 8] [0 4] ... [x y]]
  ;; 

  ;; [[6 10] [0 14] [9 10]]
  ;; fold at 7
  ;; transform anything with x > 7

;; complement for inverting a predicate, not "not"

(def test-inputs
  (slurp "resources/day13_test.txt"))

(defn parse-inputs 
  [s]
  (->> s
       split-lines
       (remove empty?)
       (split-with #(re-find #"\d+,\d+" %))))

(defn read-point
  [coord]
  (-> coord
      (str/split #",")
      (->>
       (map (fn [x] (Integer/parseInt x))))))

(defn read-points!
  [coords]
  (map read-point coords))

(defn read-inst-value
  [s]
  {:direction (case (first s)
                \y :horizontal
                \x :vertical)
   :at (Character/getNumericValue (last s))})

(defn read-instruction
  [inst-str]
  (-> inst-str
      (str/split #" ")
      last
      read-inst-value))

(defn read-instructions
  [inst-strs]
  (map read-instruction inst-strs))

(defn package-inputs
  [inputs]
  (let [[coords-str instructions-str] (parse-inputs inputs)]
    {:coords (read-points! coords-str)
     :instructions (read-instructions instructions-str)}))

(defn do-stuff
  [])

(def test-first-fold
  [[0 0] [0 1] [0 3] [1 4] [2 0] [3 0] [3 4] [4 1] [4 3] [6 0] [6 2] [6 4] [8 4] [9 0] [9 4] [10 2] [10 4]])

(defn translate-coord
  [at [x y]]
  (let [diff (- y at)]
    [x (- at diff)]))

(defn fold
  [coords {:keys [_direction at]}]
  ;;{:direction :horizontal, :at 7}
  ;; points on close side of fold don't
  ;; reflect the points beyond the fold over fold axis
  ;; fold axis = direction + value
  ;; 
  (let [above-fold (filter #(< (second %) at) coords)
        below-fold (remove #(< (second %) at) coords)
        new-coords (map (partial translate-coord at) below-fold)]
    (set (concat new-coords above-fold))))


(deftest part1
  (testing "figuring out fold")
  (testing "read-points"
    
    (is (= [[6 10] [6 10]] (read-points! ["6,10" "6,10"])))
    (is (= [6 10] (read-point "6,10")))
    (is (= {:direction :horizontal
            :at 7} (read-instruction "fold along y=7")))
    (is (= [{:direction :horizontal
            :at 7}
            {:direction :horizontal, :at 7}] (read-instructions ["fold along y=7" "fold along y=7"])))
    (is (= {:coords '((6 10) (0 14) (9 10) (0 3) (10 4) (4 11) (6 0) (6 12) (4 1) (0 13) (10 12) (3 4) (3 0) (8 4) (1 10) (2 14) (8 10) (9 0)), :instructions '({:direction :horizontal, :at 7} {:direction :vertical, :at 5})}
           (package-inputs test-inputs)))
    (is (= (set test-first-fold) (fold (:coords (package-inputs test-inputs)) {:direction :horizontal, :at 7})))))