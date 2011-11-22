(ns yahtzee.print
  (use yahtzee.core))

(def categories
  `[;; Encased in var's
    ;; to be able to get their name from the metadata
    ones twos threes fours fives sixes
    pair two-pairs three-of-a-kind four-of-a-kind
    full-house small-straight large-straight
    yahtzee chance])

(defn print-results [throw]
  (doseq [category-func categories]
    (let [func (find-var category-func)
          name (:name (meta func))]
      (println (str name) ":" (func throw)))))
