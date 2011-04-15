(ns clj-diff.other-ld
  "Other Levenshtein Disitance functions for use as a comparison.")

(defn- new-row
  [prev-row row-elem t]
  (reduce (fn [row [d-1 d e]]
            (conj row
                  (if (= row-elem e)
                    d-1
                    (inc (min (peek row) d d-1)))))
          [(inc (first prev-row))]
          (map vector prev-row (next prev-row) t)))

(defn laurent-levenshtein
  "Very compact implementation from Laurent PETIT."
  [s t]
  (peek (reduce (fn [prev-row s-elem]
                  (new-row prev-row s-elem t))
                (range (inc (count t)))
                s)))
