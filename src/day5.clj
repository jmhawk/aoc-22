(ns day5
  (:require [clojure.string :refer [trim split split-lines]]
            [clojure.test :as test]
            [clojure.edn :as edn]))

(def test-coords (-> (slurp "resources/day5_test.txt")
                     split-lines))

(defn str-coord->int-coords
  [coord-str]
  (map #(Integer/parseInt %) (split coord-str #",")))

(defn row->str-coords
  [row]
  (split row #" -> "))

(defn coord-pair-str->clj
  [coord-pair-str] ;; [["0,1" "0,2"]]
  (map str-coord->int-coords coord-pair-str))
;; ["0,1" "0,2"] arg to mapping function

(defn coords->clj
  [coords]
  (map str-coord->int-coords (map row->str-coords coords)))

;; "0,1 -> 3,1" =>  [[0 1] [3 1]]

(defn graph-point
  [coord current-map]
  (str-coord->int-coords coord))

(map str-coord->int-coords ["0,1" "0,2"])

(map #( apply edn/read-string %) [["hey"] ["buddy"]])

(test/deftest part1
  (test/is (= ["0,1" "0,2"] (row->str-coords "0,1 -> 0,2")))
  (test/is (= [["0,1" "0,2"] ["1,1" "1,2"]] (map row->str-coords ["0,1 -> 0,2" "1,1 -> 1,2"])))
  (test/is (= [[0 1] [0 2]] (coord-pair-str->clj [["0,1" "0,2"]])))
  (test/is (= [[0 1] [0 2]] (coords->clj ["0,1 -> 0,2"])))
  (test/is (= [[0 1]] (graph-point "0,1" [])))
  (test/is (= [[0 1 1]] (graph-point "0,2" []))))

(comment 
  
  (coords->clj test-coords)
  )

;; receive row of coordinates
;; ["0,1" "3,1"]
;;
;; 
;; (mark-coord) ;; takes coord and increments its location
;; end up with:   . . . .
;;                1 1 1 1

;; data representation of diagram?
;; [[0 0 0 0] ;; y=0
;;  [1 1 1 1]] ;; y=1