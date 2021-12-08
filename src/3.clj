(ns aoc-22.src
  (:require [clojure.string :refer [split-lines]]
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

(def test-diagnostics  (-> (slurp "resources/test_3.txt")
                           split-lines))

(def col-5 []) ;; uses memory forever
;; (let [col1 []] ...) at least forces execution stack memory

;; \1 == char literal
;; \space
;; \newline

(defn char->int-vector
  [c]
  [(Character/getNumericValue c)])

(defn bit->vector [s]
  (map char->int-vector s))

(defn inputs->index-vectors
  [coll]
  (->> (map bit->vector coll)
       (apply map concat)))

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
        inputs->index-vectors
        (map most-common-value)
        (map str)
        (apply str))
   2))

(defn get-epsilon
  [diagnostics]
  (Integer/parseInt
   (->> diagnostics
        inputs->index-vectors
        (map least-common-value)
        (map str)
        (apply str))
   2))

(defn part-1 
  [diagnostics]
  (* (get-epsilon diagnostics) (get-gamma diagnostics)))

(test/deftest test-string-to-element-vectors
  (test/is (= [[1] [0] [0] [1] [0]] (bit->vector "10010")))
  (test/is (=
            [[1 1] [0 1] [0 0] [1 0] [0 0]]
            (inputs->index-vectors ["10010" "11000"])))
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

;; PART 2 ;; 

;;hand me a coll
;; group up index 0 vals
;; find oxygen-filter-value
;; filter coll by oxygen-filter-value index 0
;; group up index 1 vals
;; find oxygen-filter-value
;; filter coll by oxygen-filter-value index 1
;; group up index 2 vals
;; find o2-filter-val
;; filter coll by o2-filter fal index 2
;; etc

;; in order to filter, we need diagnostics coll and oxygen filter number and index to filter on
(def indices [0 1 2 3 4])

(defn oxygen-filter-value
  [xs]
  (let [fs (frequencies xs)]
    (cond
      (= (get fs 0) (get fs 1)) 1
      (> (get fs 0) (get fs 1)) 0
      (< (get fs 0) (get fs 1)) 1)))

(defn co2-filter-value
  [xs]
  (let [fs (frequencies xs)]
    (cond
      (= (get fs 0) (get fs 1)) 0
      (> (get fs 0) (get fs 1)) 1
      (< (get fs 0) (get fs 1)) 0)))


(defn o2-filter-val
  [raw-diagnostics index]
  (let [indexed-bits (inputs->index-vectors raw-diagnostics)] 
    (oxygen-filter-value (nth indexed-bits index))))

(defn co2-filter-val
  [raw-diagnostics index]
  (let [indexed-bits (inputs->index-vectors raw-diagnostics)]
    (co2-filter-value (nth indexed-bits index))))

(defn filter-oxygen
  [raw-diagnostics index]
  (if (< 1 (count raw-diagnostics))
    (filter
     #(= (Character/getNumericValue (nth % index)) (o2-filter-val raw-diagnostics index))
     raw-diagnostics)
    raw-diagnostics))

(defn filter-carbon
  [raw-diagnostics index]
  (if (< 1 (count raw-diagnostics))
    (filter
     #(= (Character/getNumericValue (nth % index)) (co2-filter-val raw-diagnostics index))
     raw-diagnostics)
    raw-diagnostics))

(def r1 (filter-carbon test-diagnostics 0))
(def r2 (filter-carbon r1 1))
(def r3 (filter-carbon r2 2))
(def r4 (filter-carbon r3 3))

(reduce filter-oxygen test-diagnostics indices)

(defn part2
  [diagnostics]
  (let [o2 (Integer/parseInt (first (reduce filter-oxygen diagnostics indices)) 2)
        co2 (Integer/parseInt (first (reduce filter-carbon diagnostics indices)) 2)]
    (* o2 co2)))

(part2 diagnostics) ;; not working yet :'(

(test/deftest part-2
              (test/is (= 1 (oxygen-filter-value [1 0])))
              (test/is (= 1 (oxygen-filter-value [1 1 0])))
              (test/is (= 0 (oxygen-filter-value [1 0 0])))
              (test/is (= 1 (o2-filter-val test-diagnostics 0)))
              (test/is (= ["10111"] (reduce filter-oxygen test-diagnostics indices)))
              )


(comment

  (inputs->index-vectors test-diagnostics)
  
  (test/run-tests)

  (apply map concat '(([1] [0] [0] [1] [0]) ([1] [1] [0] [0] [0])))
  
  (apply + 1 2 [3 4])
  
  (apply + [1 2 3])
  ;
  )