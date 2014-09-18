(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (zero? (value-at board coord))
    false true))

(defn row-values [board [row col]]
  (set (map (fn [c] (value-at board [row c])) (range 9))))

(defn col-values [board [row col]]
  (set (map (fn [r] (value-at board [r col])) (range 9))))

(defn coord-pairs [coords]
  (for [a coords b coords] [a b]))

(defn block-values [board [row col]]
  (let [get-val (fn [x] (* (int (/ x 3)) 3))
        r (get-val row)
        c (get-val col)]
    (set (for [a (range r (+ r 3))
               b (range c (+ c 3))]
            (value-at board [a b])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (set/difference
      (set/difference
        (set/difference 
          #{1 2 3 4 5 6 7 8 9} (block-values board coord))
        (row-values board coord))
      (col-values board coord))))

(defn filled? [board]
  (not (contains? 
         (set (for [a (range 9)
                    b (range 9)]
               (has-value? board [a b])))
          false)))

(defn rows [board] 
  (for [a (range 9)] (row-values board [a 0])))

(defn valid-something [f board]
  (not (contains? 
    (set (map
           (fn [x] (= x #{1 2 3 4 5 6 7 8 9 }))
           (f board)))
     false)))

(defn valid-rows? [board]
  (valid-something rows board))

(defn cols [board]
  (for [b (range 9)] (col-values board [0 b])))

(defn valid-cols? [board]
  (valid-something cols board))

(defn blocks [board]
  (for [a (range 3) b (range 3)]
    (block-values board [(* 3 a) (* 3 b)])))

(defn valid-blocks? [board]
  (valid-something blocks board))

(defn valid-solution? [board]
  (and (valid-blocks? board)
    (and (valid-cols? board)
      (and (valid-rows? board)))))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (fn [x] (not (has-value? board x)))
           (for [a (range 9) b (range 9)]
             [a b]))))

(defn solve-helper [board]
  (if (not (find-empty-point board))
    (if (valid-solution? board)
        [board]
        [])
    (let [empty-point (find-empty-point board)]
      (for [value (valid-values-for board empty-point)
          solution (solve-helper (set-value-at board empty-point value))]
        solution))))

(defn solve [board] (first (solve-helper board)))

