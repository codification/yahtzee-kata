(ns yahtzee.core
  (:use midje.sweet))

(def *sides* [6 5 4 3 2 1])

(unfinished )

(defn occ [die dice]
  (filter #{die} dice))

(fact
 (occ 6 [1 2 3 4 5]) => [])

(defn sum [dice]
  (reduce + dice))

(fact
 (sum []) => 0)


(defn sixes [throw]
  (sum (occ 6 throw)))
(defn fives [throw]
  (sum (occ 5 throw)))

(fact
 (sixes [6 1 6 1 6])  => 18
 (fives [1 2 3 4 5])  => 5)

(defn pairs [throw]
  (filter #(= 2 (count %)) (map #(occ % throw) *sides*)))

(fact
 (pairs [1 2 3 4 5]) => []
 (pairs [1 2 3 2 5]) => [[2 2]]
 (pairs [1 2 3 2 1]) => [[2 2] [1 1]]
 (pairs [3 3 2 6 6]) => [[6 6] [3 3]])


(defn pair [throw]
  (let [candidates (pairs throw)
        scores (map sum candidates)]
    (if (empty? scores)
      0
      (first (sort > scores)))))

(fact
 (pair ...throw...) => 0
 (provided
  (pairs ...throw...) => [])
 (pair ...throw...) => 2
 (provided
  (pairs ...throw...) => [[1 1]])
 (pair ...throw...) => 6
 (provided
  (pairs ...throw...) => [[1 1] [3 3]]))


(defn two-pairs [throw]
  (reduce + (map #(reduce + %) (pairs throw))))


;.;. Intellectual 'work' is misnamed; it is a pleasure, a dissipation, and
;.;.                                ; is its own highest reward. -- Twain
(fact
 (two-pairs ...throw...) => 6
 (provided
  (pairs ...throw...) => [[2 2] [1 1]])
 (two-pairs ...throw...) => 0
 (provided
  (pairs ...throw...) => []))
