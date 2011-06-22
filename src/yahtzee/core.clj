(ns yahtzee.core
  (:use midje.sweet))

(def sides [6 5 4 3 2 1])

(unfinished )

(defn occ [die dice]
  (filter #{die} dice))

(fact
 (occ 6 [1 2 3 4 5]) => []
 (occ 1 [1 2 3 4 5]) => [1]
 (occ 1 [1 2 1 2 1]) => [1 1 1])

(defn sum [dice]
  (reduce + (flatten dice)))

(fact
 (sum [1 1 1]) => 3
 (sum [[1 1] [2 2]]) => 6)

(defn sixes [throw]
  (sum (occ 6 throw)))
(defn fives [throw]
  (sum (occ 5 throw)))

(defn make-occ-func [die]
  (fn [throw]
    (* ((frequencies throw) die 0) die)))

(def fours (make-occ-func 4))
(def threes (make-occ-func 3))
(def twos (make-occ-func 2))
(def ones (make-occ-func 1))

(fact
 (sixes [6 1 6 1 6])  => 18
 (fives [1 2 3 4 5])  => 5
 (fours [1 2 3 4 5])  => 4
 (threes [1 2 3 4 3]) => 6)

(defn pairs [throw]
  (let [freq (frequencies throw)]
    (map first (filter (fn [[_ frequency]] (= 2 frequency) ) freq))))

(fact
 (pairs [1 2 3 4 5]) => []
 (pairs [1 2 3 2 5]) => [2]
 (pairs [1 2 3 2 1]) => (just [2 1] :in-any-order)
 (pairs [3 3 2 6 6]) => (just [6 3] :in-any-order))


(defn pair [throw]
  (let [candidates (pairs throw)]
    (if (empty? candidates)
      0
      (* 2 (last (sort candidates))))))

(fact
 (pair ...throw...) => 0
 (provided
  (pairs ...throw...) => [])
 (pair ...throw...) => 2
 (provided
  (pairs ...throw...) => [1])
 (pair ...throw...) => 6
 (provided
  (pairs ...throw...) => [1 3]))


(defn two-pairs [throw]
  (sum (pairs throw))) ;; Duplication

(fact
 (two-pairs ...throw...) => 12
 (provided
  (pairs ...throw...) => [[6 6]])
 (two-pairs ...throw...) => 6
 (provided
  (pairs ...throw...) => [[2 2] [1 1]])
 (two-pairs ...throw...) => 0
 (provided
  (pairs ...throw...) => []))


(defn triplets [throw]
  (filter #(= 3 (count %))
          (map #(occ % throw) sides))) ;; Duplication...

(fact
 (triplets [1 2 3 4 5]) => []
 (triplets [1 2 3 2 2]) => [[2 2 2]])

(defn three-of-a-kind [throw]
  (sum (triplets throw))) ;; Duplication...

(fact
 (three-of-a-kind ...throw...) => 12
 (provided
  (triplets ...throw...) => [[4 4 4]])
 (three-of-a-kind ...throw...) => 0
 (provided
  (triplets ...throw...) => []))