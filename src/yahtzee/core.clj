(ns yahtzee.core
  (:use midje.sweet))

(defn sum [dice]
  (reduce + 0 (flatten dice)))

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
  (let [occurences (frequencies throw)]
    (for [[die occurs] occurences :when (<= times occurs)]
      die)))

(defn total-for-recurs [times recurs]
  (sum (map (partial * times) recurs)))

(defn pair [throw]
  (let [pairs (recurring 2 throw)]
    (if (seq pairs)
      (* 2 (last (sort pairs)))
      0)))

(fact
 (pair [1 2 3 4 5]) => 0
 (pair [1 1 2 3 4]) => 2
 (pair [3 3 2 1 1]) => 6)

(defn two-pairs [throw]
  (let [pairs (recurring 2 throw)
        pair-of-pairs (if (= 2 (count pairs))
                        pairs
                        [])]
    (total-for-recurs 2 pair-of-pairs)))

(fact
 (two-pairs [6 5 6 5 1]) => 22
 (two-pairs [2 1 2 1 4]) => 6
 (two-pairs [2 2 1 5 6]) => 0
 (two-pairs [1 2 3 4 5]) => 0
 (two-pairs [1 1 1 2 2]) => 6)

(defn three-of-a-kind [throw]
  (total-for-recurs 3 (recurring 3 throw))) ;; Duplication...

(fact
 (three-of-a-kind [1 3 2 3 3]) => 9
 (three-of-a-kind [1 3 3 3 3]) => 9
 (three-of-a-kind [2 2 1 1 3]) => 0)

(defn four-of-a-kind [throw]
  (total-for-recurs 4 (recurring 4 throw))) ;; Duplication...

(fact
 (four-of-a-kind [1 4 4 4 4]) => 16
 (four-of-a-kind [4 2 3 4 4]) => 0)

(defn yahtzee [throw]
  (if (seq (recurring 5 throw))
    50
    0))

(fact
 (yahtzee [1 1 1 1 1]) => 50
 (yahtzee [1 1 2 1 1]) => 0)

(defn full-house [throw]
  (let [triplet (recurring 3 throw)
        ex-triplet (filter (complement (set triplet)) throw)
        pair (recurring 2 ex-triplet)]
    (if (and (seq pair) (seq triplet))
      (sum [ (total-for-recurs 2 pair)
             (total-for-recurs 3 triplet)])
      0)))

(fact
 (full-house [2 2 3 3 3]) => 13
 (full-house [2 2 2 2 2]) => 0
 (full-house [1 1 2 2 3]) => 0
 (full-house [1 2 2 2 3]) => 0)

(defn small-straight [throw]
  (if (= #{1 2 3 4 5} (set throw))
    (sum throw)
    0))

(fact
 (small-straight [1 2 3 4 5]) => 15
 (small-straight [1 2 3 4 6]) => 0)

(defn large-straight [throw]
  (if (= #{2 3 4 5 6} (set throw))
    (sum throw)
    0))

(fact
 (large-straight [1 2 3 4 5]) => 0
 (large-straight [1 2 3 4 6]) => 0
 (large-straight [2 3 4 5 6]) => 20)

(defn chance [throw]
  (sum throw))

(fact
 (chance ...throw...) => ...result...
 (provided
  (sum ...throw...) => ...result...))

