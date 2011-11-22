(ns yahtzee.core
  (:use midje.sweet))

(def sides [6 5 4 3 2 1])

(unfinished )

(defn sum [dice]
  (reduce + (flatten dice)))

(fact
 (sum [1 1 1]) => 3
 (sum [[1 1] [2 2]]) => 6)

(defn make-occ-func [die]
  (fn [throw]
    (* ((frequencies throw) die 0) die)))

(def sixes (make-occ-func 6))
(def fives (make-occ-func 5))
(def fours (make-occ-func 4))
(def threes (make-occ-func 3))
(def twos (make-occ-func 2))
(def ones (make-occ-func 1))

(fact
 (sixes [6 1 6 1 6])  => 18
 (fives [1 2 3 4 5])  => 5
 (fours [1 2 3 4 5])  => 4
 (threes [1 2 3 4 3]) => 6)

(defn recurring [times throw]
  (let [freq (frequencies throw)]
    (map first (filter #(= times (second %)) freq))))

(defn pair [throw]
  (let [pairs (recurring 2 throw)]
    (if (seq pairs)
      (* 2 (last pairs))
      0)))

(fact
 (pair ...throw...) => 0
 (provided
  (recurring 2 ...throw...) => [])
 (pair ...throw...) => 2
 (provided
  (recurring 2 ...throw...) => [1])
 (pair ...throw...) => 6
 (provided
  (recurring 2 ...throw...) => [1 3]))

(defn two-pairs [throw]
  (let [pairs (recurring 2 throw)
        pair-of-pairs (if (= 2 (count pairs))
                        pairs
                        [])]
   (sum (map #(* 2 %) pair-of-pairs)))) ;; Duplication

(fact
 (two-pairs ...throw...) => 22
 (provided
  (recurring 2 ...throw...) => [6 5])
 (two-pairs ...throw...) => 6
 (provided
  (recurring 2 ...throw...) => [2 1])
 (two-pairs ...throw...) => 0
 (provided
  (recurring 2 ...throw...) => [2])
 (two-pairs ...throw...) => 0
 (provided
  (recurring 2 ...throw...) => []))

(defn three-of-a-kind [throw]
  (sum (map #(* 3 %) (recurring 3 throw)))) ;; Duplication...

(fact
 (three-of-a-kind ...throw...) => 12
 (provided
  (recurring 3 ...throw...) => [4])
 (three-of-a-kind ...throw...) => 0
 (provided
  (recurring 3 ...throw...) => []))

(defn four-of-a-kind [throw]
  (sum (map #(* 4 %) (recurring 4 throw)))) ;; Duplication...

(fact
 (four-of-a-kind [1 4 4 4 4]) => 16
 (four-of-a-kind [4 2 3 4 4]) => 0)
