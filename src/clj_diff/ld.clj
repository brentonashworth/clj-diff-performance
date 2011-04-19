(ns clj-diff.ld
  "Fast implementation of Levenshtein Distance."
  (:require [clj-diff [optimizations :as opt]]
            [clj-diff [miller :as m]]))

(defn vectorize [x]
  (vec (cons nil (seq x))))

(defn ld [a b]
  (let [[a b] (map vectorize [a b])]
    (m/ses a b)))

(defn edit-dist [delta p]
  (+ delta (* 2 p)))

;; Ugly but promising implementation. This is Laurent's code with an
;; added sliding window based on calculating the p values using the
;; existing ses function. It is twice as fast when there is only 10%
;; change and equally as fast with 50% change. Half the time it gives
;; an answer that is off by 1. There is a lot of additional complexity
;; caused by trying to fit this in with Laurent's code. I think a
;; fresh implementation will work much better.

(defn sliding-window [s t delta p]
  (let [window (inc (edit-dist delta p))
        offset (inc (- p window))]
    (map #(vector (max % 0)
                  (min (dec (+ window %)) (count t)))
         (map #(+ % offset) (range (inc (count s)))))))

(defn- new-row
  [prev-row row-elem t [begin end] max-dist]
  (reduce (fn [row [d-1 d e]]
            (conj row
                  (if (= row-elem e)
                    d-1
                    (inc (min (or (peek row) max-dist) d d-1)))))
          (if (zero? begin) [(inc (first prev-row))] [])
          (map vector prev-row (next prev-row) (drop begin (take end t)))))

(defn new-levenshtein
  ([s t]
     (let [[s t] (if (> (count s) (count t)) [s t] [t s])]
       (apply new-levenshtein s t (take 2 (ld s t)))))
  ([s t p delta]
     (let [max-dist (edit-dist delta p)]
       (peek (reduce (fn [prev-row [s-elem window]]
                       (new-row (conj prev-row max-dist max-dist)
                                s-elem
                                t
                                window
                                max-dist))
                     (vec (range (inc p)))
                     (map vector
                          s
                          (drop 1 (sliding-window s t delta p))))))))
