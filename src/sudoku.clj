(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board [row col]]
  (set (board row)))

(defn col-values [board [row col]]
  (set
   (map #(get % col) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row,col]))

(defn top-left-coner [row col]
  (let [init-val (fn [x]
                   (cond 
                     (< x 3) 0
                     (< x 6) 3
                     :else   6))]
    [(init-val row), (init-val col)]))

(defn block-values [board [row col]]
  (let [[init-row init-col] (top-left-coner row col)]
    (set
      (for [rows (range init-row (+ init-row 3)) 
            cols (range init-col (+ init-col 3))]
        (value-at board [rows cols])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference
      all-values
      (row-values board coord)
      (col-values board coord)
      (block-values board coord))))

(defn filled? [board]
  (let [all-values (set 
                     (apply concat board))]
    (not 
      (contains? all-values 0))))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? #(= all-values %) 
          (rows board)))

(defn cols [board]
  (for [cols (range 9)]
    (col-values board [0 cols])))

(defn valid-cols? [board]
  (every? #(= all-values %) 
          (cols board)))

(defn blocks [board]
  (for [rows [0 3 6]
        cols [0 3 6]]
    (block-values board [rows cols])))

(defn valid-blocks? [board]
  (every? #(= all-values %) 
          (blocks board)))

(defn valid-solution? [board]
  (and 
    (valid-rows? board)
    (valid-cols? board)
    (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first 
    (filter #(not (has-value? board %))  
            (for [row (range 9)
                  col (range 9)]
              [row col]))))

(defn solve [board]
  (cond 
   (filled? board) (if (valid-solution? board) 
                     board 
                     [])
   :otherwise (let [empty-location (find-empty-point board)
                    valid-values (valid-values-for board empty-location)]
                (first (filter (complement empty?)
                        (for [values valid-values]
                          (solve (set-value-at board 
                                               empty-location
                                               values))))))))
