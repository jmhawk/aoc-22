(ns day11
  (:require [clojure.string :refer [split-lines]]
            [clojure.test :as test]))

(def test-inputs (->> (slurp "resources/day11_test.txt")
                      split-lines
                      (map #(map (fn [x] (Character/getNumericValue x)) %))))

(defn increase-row!
  [row]
  (map inc row))

(defn increase-rows!
  [rows]
  (map increase-row! rows))

(defn >9?
  [row]
  (keep-indexed (fn [i x] (when (> x 9) i)) row))

(defn rows->coords-over-9
  [rows]
  (keep-indexed (fn [i row] [i (>9? row)]) rows))

(rows->coords-over-9 [[9 9 9 10] [10 9 9 9] [1 1 1 1]])

;; what?
;; I want to grab the index of values over ten, put them in a coord vector
;; so I also need index of the row that is being mapped

(defn flash-neighbors!
  [flashed-coords]
  ;; take coords, map over them and flash neighbors (adds to vector if unflashed)
  )

(defn flash-dance! 
  [graph]
  (let [>9-coords (rows->coords-over-9 graph)]))

(defn step!
  []
  (-> (increase-rows!)
      (flash-dance!)))

(defn coord->neighbor-coords
  [coords]
  ;; filter out coords on the flashed vector 
  )

;; each step:
;; increase each value by one
;; any that are (> x 9) flash!
;; (defn flash! [coord] 
;;   inc surrounding digits (diag included))
;; check all for 10 or more
;; if unflashed results, flash again and repeat
;; once you can scan and see no more unique 10+s, count resulting flashes
;; next step!


;; CURRENT APPROACH
;; increase all values
;;go row by row and find coords of over 10
;; this gives me a big set of coordinates
;; then run some (f-inc-neighbors) which raises all adjacent to last flash
;;

;; need to know who has already flashed in a given step (list of coords)

;; then refresh those flashes on the next step
;;    -- count the coords from previous step and inc some global counter
;;    -- start anew with empty vector on next step



(test/deftest part1
  (test/is (= '(6 5 9 4 2 5 4 3 3 4)  (increase-row! (first test-inputs))))
  (test/is (= 10 (count (increase-rows! test-inputs))))
  (test/is (= [6 3 9 4 8 6 2 6 3 7] (last (increase-rows! test-inputs))))
  (test/is (= [2 4] (>9? [0 0 10 7 284828])))
  (test/is (= [2 4] (>9? [0 0 10 7 284828]))))





(comment

  ((fn [] (dotimes [n 3] (inc counter) (prn counter)))) ;; why does this print 0 three times? 

; 
  )