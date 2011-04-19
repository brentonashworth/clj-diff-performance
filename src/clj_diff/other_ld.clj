(ns clj-diff.other-ld
  "Other Levenshtein distance functions for use as a comparison.")

;; A very compact implementation from Laurent Petit. This algorithm is also
;; faster then the typical implementation. If you can make an algorithm
;; faster than this, you are doing well.

;; One thing you may want to investigate is using this algorithm with
;; the data that you get from ses.

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
  [s t]
  (peek (reduce (fn [prev-row s-elem]
                  (new-row prev-row s-elem t))
                (range (inc (count t)))
                s)))
