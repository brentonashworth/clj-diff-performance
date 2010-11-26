(ns clj-diff.performance
  "Measuring performance of different diff algorithms."
  (:use [incanter core charts datasets])
  (:require [clj-diff [core :as core]]
            [clj-diff [myers :as myers]]
            [clj-diff [miller :as miller]]
            [incanter [stats :as stats]])
  (:import name.fraser.neil.plaintext.diff_match_patch))

;;
;; Diff functions for performance tests.
;;
;; Each function will compute a diff and return it.
;;

(defn myers-diff [a b]
  (myers/diff a b))

(defn miller-diff [a b]
  (miller/diff a b))

(defn miller-seq-diff [a b]
  (miller/diff (seq a) (seq b)))

(defn fraser-diff [a b]
  (let [dmp (diff_match_patch.)]
    (do (set! (. dmp Diff_Timeout) 0)
        (.diff_main dmp a b))))

(defn fraser-distance [diffs]
  (let [diffs (map #(vector (.toString (.operation %)) (.text %)) (seq diffs))]
    (reduce + (map #(case (first %)
                          "EQUAL" 0
                          (count (last %)))
                   diffs))))

(defprotocol EditDistance
  (edit-distance [diffs]))

(extend-protocol EditDistance
  
  clojure.lang.IPersistentMap
  
  (edit-distance [diffs]
                 (core/edit-distance diffs))

  java.util.LinkedList

  (edit-distance [diffs]
                 (fraser-distance diffs)))

(defn random-between
  "Generate a random number between low and high. Can also be passed
  characters as the bounds."
  [lo hi]
  (let [r (java.util.Random.)
        lo (if (char? lo) (int lo) lo)
        hi (if (char? hi) (int hi) hi)
        n (+ 1 (- hi lo))]
    (+ lo (Math/abs (mod (. r nextInt) n)))))

(defn random-string
  "Generage a random string composed of upper and lower case letters and the
  numbers 0 through 9."
  [size]
  (loop [length (random-between size size)
         v []]
    (if (> length 0)
      (let [j (random-between 1 3)]
        (recur (dec length)
               (conj v
                     (cond (= j 1) (char (random-between \a \z))
                           (= j 2) (char (random-between \A \Z))
                           (= j 3) (char (random-between \0 \9))))))
      (apply str v))))

(defn mutate
  "Make n random mutations to the string s. Mutations will be randomly grouped
  into runs of 1 to g."
  [s n g]
  (let [size (count s)
        s (vec s)
        group-size (random-between 1 (min n g))
        additions (partition group-size
                             (take n
                                   (repeatedly #(char (random-between \a \z)))))
        indecies (take (count additions) (shuffle (range size)))
        indecies (map #(let [c (count %2)]
                         (range %1 (+ c %1)))
                      indecies
                      additions)]
    (apply str
           (flatten
            (reduce (fn [a b]
                      (assoc a (first b) (last b)))
                    s
                    (map #(vector %1 %2)
                         (flatten indecies)
                         (flatten additions)))))))

(defn time*
  "Calculate the time, in milliseconds, to run the passed expression. Returns
  a sequence of maps containing the times and return values of each run."
  ([expr]
     (time* 1 expr identity))
  ([n expr]
     (time* n expr identity))
  ([n expr f]
     (map (fn [_] (let [start (. System (nanoTime))
                        ret (expr)
                        stop (. System (nanoTime))]
                    {:time (/ (double (- stop start)) 1000000.0)
                     :result (f ret)}))
          (range 0 n))))

(defn sample
  "For strings a and b, run each diff algorithm 'total-runs' times and then
  calculate stats based on the fastest 'take-top' runs."
  [fns a b take-top total-runs]
  (map #(let [[alg f] %
              d (take take-top
                      (sort-by :time (time* total-runs
                                            (fn [] (f a b))
                                            edit-distance)))
              times (map :time d)
              distances (distinct (map :result d))
              mean (stats/mean times)
              sd (stats/sd times)]
          {:name alg :mean mean :sd sd
           :distance (apply str (interpose ", " distances))})
       fns))

(defn vary-mutations
  "For a sting on length n, vary the number of mutations that are made to
  the string."
  ([fns n m-range g]
     (vary-mutations fns n m-range g 2 3))
  ([fns n m-range g t r]
     (let [a (random-string n)]
       (flatten
        (map (fn [m] (map #(merge {:mutations m} %)
                          (sample fns a (mutate a m g) (max t 1) (max r 1))))
             m-range)))))

(defn vary-string-length
  ([fns n-range m]
     (vary-string-length fns n-range m 2 3))
  ([fns n-range m t r]
     (flatten
      (map (fn [n] (map #(merge {:size n} %)
                        (let [a (random-string n)]
                          (sample fns a (m a) (max t 1) (max r 1)))))
           n-range))))

(defn visualize [title file-name data]
  (let [d (to-dataset (doall data))]
    (view d)
    (with-data d
      (doto (line-chart :mutations :mean
                        :group-by :name
                        :legend true
                        :title (str "String length = " title)
                        :x-label (str "Mutations")
                        :y-label "Time (ms)")
        (view :width 850)
        (save (str "charts/" file-name ".png") :width 850)))))

(defn visualize-2
  ([title file-name data]
     (visualize-2 title file-name data 850))
  ([title file-name data width]
     (let [d (to-dataset (doall data))
           x-label (if (.endsWith file-name "seq") "Sequence/String" "String")]
       (view d)
       (with-data d
         (doto (line-chart :size :mean
                           :group-by :name
                           :legend true
                           :title title
                           :x-label (str x-label " Length")
                           :y-label "Time (ms)")
        
           (view :width width)
           (save (str "charts/" file-name ".png") :width width))))))

(defn test-range [size points]
  (let [mutations (quot (* size 9) 10)
        step (quot mutations points)]
    (range 1 (inc mutations) step)))

(defn- move-first-to-end* [a]
  (let [s (seq a)
        f (first s)
        s (drop 1 s)]
    (apply str (concat (vec s) [f]))))

(defn- add-in-the-middle* [a]
  (let [split (map #(apply str %) (split-at (/ (count a) 2) (seq a)))]
    (str (first split) "clj-diff" (last split))))

(defn- delete-half-and-mutate* [a]
  (let [half (quot (count a) 2)
        b (str (.substring a 0 (quot half 2))
               (.substring a (- (count a) (quot half 2))))]
    (mutate b (quot (count b) 10) 2)))

(defn vary-mutation-100 [fns x n]
  (let [d (vary-mutations fns 100 (test-range 100 x)
                          5
                          (quot (* n 2) 3)
                          n)]
    (visualize 100 "mutations_100" d)))

(defn vary-mutation-1000 [fns x n]
  (let [d (vary-mutations fns 1000 (test-range 1000 x)
                          50
                          (quot (* n 2) 3) n)]
    (visualize 1000 "mutations_1000" d)))

(defn move-first-to-end [fns x n name]
  (let [d (vary-string-length fns (range 100 200000 (quot 200000 x))
                              move-first-to-end*
                              (quot (* n 2) 3)
                              n)]
    (visualize-2 "Move First Element to End"
                 (str "length_move_first_to_end_" name)
                 d
                 425)))

(defn add-in-the-middle [fns x n name]
  (let [d (vary-string-length fns (range 100 200000 (quot 200000 x))
                              add-in-the-middle*
                              (quot (* n 2) 3)
                              n)]
    (visualize-2 "Add in the Middle"
                 (str "length_add_in_middle_" name)
                 d
                 425)))

(defn delete-half-and-mutate [fns x n]
  (let [d (vary-string-length fns (range 100 2000 (quot 2000 x))
                              delete-half-and-mutate*
                              (quot (* n 2) 3)
                              n)]
    (visualize-2 "Delete Half and Change" "length_delete_half" d)))

(defn percent-change [fns max p x n]
  (let [percent (/ p 100.0)
        d (vary-string-length fns (range 100 max (quot max x))
                              #(mutate % (* (count %) percent) 10)
                              (quot (* n 2) 3)
                              n)]
    (visualize-2 (str p "% Change") (str "length_" max "_" p) d)))

(defn suite [x]
  (let [fns [["Myers" myers-diff]
             ["Miller" miller-diff]
             ["Fraser" fraser-diff]]
        seq-fns [["Miller - String" miller-diff]
                 ["Miller - Seq" miller-seq-diff]]]
    (do
      (vary-mutation-100 fns x 60)
      (vary-mutation-1000 fns x 20)
      (percent-change fns 15000 5 x 10)
      (percent-change fns 7000 5 x 20)
      (percent-change fns 5000 10 x 10)
      (percent-change fns 2000 50 x 10)
      (move-first-to-end fns 5 30 "string")
      (add-in-the-middle fns 5 30 "string")
      (move-first-to-end seq-fns 5 30 "seq")
      (add-in-the-middle seq-fns 5 30 "seq")
      (delete-half-and-mutate fns x 10))))

(defn performance-tests
  "Run the standard performance tests."
  []
  (suite 15))
