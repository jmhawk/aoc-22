(ns day9
  (:require [clojure.test :as test]
            [clojure.string :refer [split-lines]]))

(def test-inputs (->> (slurp "resources/day9_test.txt")
                      split-lines
                      (map #(map (fn [c] (Character/getNumericValue c)) %))))

(def deez-inputs test-inputs)

(defn height
  [inputs]
  (count inputs))

(defn low-point?
  [[a b c]]
  (and (< b a)
       (< b c)))

(defn mapping-fn
  [index group-of-three]
  (when (low-point? group-of-three) index))

(defn find-low-xs [row]
  (let [padded-row (cons 9 (conj row 9))
        groups-of-3 (partition 3 1 padded-row)]
    (keep-indexed mapping-fn groups-of-3))) ;; coll of low point index

(defn process-x-row
  [index row]
  (map #(vector % index) (find-low-xs row)))

(defn get-above
  [[x y]]
  (when (> y 0) (nth (nth deez-inputs (- y 1)) x)))

(defn get-below
  [[x y]]
  (when (< y (- (height deez-inputs) 1)) (nth (nth deez-inputs (+ y 1)) x)))

(defn low-y? [[x y]]
  (let [above (get-above [x y])
        below (get-below [x y])
        current (nth (nth deez-inputs y) x)]
    (cond
      (nil? above) (< current below)
      (nil? below) (< current above)
      (and above below) (and (< current below)
                             (< current above)))))

(defn coordinate->value
  [coordinate]
  (nth (nth deez-inputs (last coordinate)) (first coordinate)))

(defn coordinates->values
  [coordinates]
  (map coordinate->value coordinates))

(defn process-rows
  [rows]
  (->> (map-indexed process-x-row rows)
       (reduce concat)
       (filter low-y?)
       coordinates->values
       (map inc)
       (apply +)))

(defn part1 
  [input]
  (process-rows input))

(part1 using-inputs)

(test/deftest part1
  (test/is (= [1] (find-low-xs [1 0 1])))
  (test/is (= [0 2] (find-low-xs [1 2 0 1])))
  (test/is (= [6 6] (process-rows deez-inputs)))
  (test/is (= 1 (get-above [1 1])))
  (test/is (nil? (get-above [9 0])))
  (test/is (nil? (get-above [1 0])))
  (test/is (= 8 (get-below [1 1])))
  (test/is (= 1 (get-below [9 0])))
  (test/is (nil? (get-below [1 4])))
  (test/is (= 1 (height [[1 1 1]])))
  (test/is (low-y? [0 1]))
  (test/is (low-y? [2 2]))
  (test/is (not (low-y? [2 3])))
  (test/is (low-y? [9 0])))

(comment)

