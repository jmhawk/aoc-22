(ns aoc-22.src
  (:require [clojure.string :refer [replace split-lines]]
            [clojure.test :as test]))

;; gamma * epsilon => power consumption
;; each bit of gamma rate == 
;; most common bit in the corresponding position of all numbers in diagnostic
;; input = diagnostic
;; epsilon rate is opposite of gamma

;; turn bit form of each into integer
;; (Integer/parseInt "10110" 2)

(def diagnostics (-> (slurp "resources/3.txt")
                      split-lines))

(defn make-rates
  [diagnostics-coll])

(def col-1 [])
(def col-2 [])
(def col-3 [])
(def col-4 [])
(def col-5 []) ;; uses memory forever
;; (let [col1 []] ...) at least forces 

;; \1 == char literal
;; \space
;; \newline

;; (magic-fn "10010") => [[1] [0] [0]]

;; have five in-memory colls of bits (0s or 1s)
;; after sorting each entry, count in some way
;; make the gamma and epsilon off of that

(defn char->int-vector
  [c]
  [(Character/getNumericValue c)])

(defn vec-maker [s]
  (map char->int-vector s))
;; tests
(defn bigger-vecker
  [coll]
  (apply map concat (map vec-maker coll)))

(defn most-common-value
  [xs]
  (let [fs (frequencies xs)]
    (if (> (get fs 0) (get fs 1))
      0
      1)))

(defn least-common-value
  [xs]
  (let [fs (frequencies xs)]
    (if (> (get fs 0) (get fs 1))
      1
      0)))

(defn get-gamma
  [diagnostics]
  (Integer/parseInt
   (->> diagnostics
        bigger-vecker
        (map most-common-value)
        (map str)
        (apply str))
   2))

(defn get-epsilon
  [diagnostics]
  (Integer/parseInt
   (->> diagnostics
        bigger-vecker
        (map least-common-value)
        (map str)
        (apply str))
   2))


(test/deftest test-string-to-element-vectors
  (test/is (= [[1] [0] [0] [1] [0]] (vec-maker "10010")))
  (test/is (=
            [[1 1] [0 1] [0 0] [1 0] [0 0]]
            (bigger-vecker ["10010" "11000"])))
  (test/is (=
            1
            (most-common-value '(0 1 1 1 1 0 0 1 1 1 0 0))))
  (test/is (=
            [1 0 1 1 0]
            (map most-common-value '((0 1 1 1 1 0 0 1 1 1 0 0) (0 1 0 0 0 1 0 1 0 1 0 1) (1 1 1 1 1 1 1 1 0 0 0 0) (0 1 1 1 0 1 1 0 0 0 1 1) (0 0 0 1 1 1 1 0 0 1 0 0))))))

;; map takes n colls, mapping fn takes n args

;; split the string into units
;; convert text->int
;; wrap digits in vector
;; I was trying to (map vector "10110")
;; trying to handle whole collection and individual element transformation simultaneously
;; better way is to separate transformation into its own step
;; good for unfamiliar operations at different levels
;;

(comment

  (bigger-vecker diagnostics)
  
  (test/run-tests)

  (apply map concat '(([1] [0] [0] [1] [0]) ([1] [1] [0] [0] [0])))
  
  (apply + 1 2 [3 4])
  
  (apply + [1 2 3])
  ;
  )