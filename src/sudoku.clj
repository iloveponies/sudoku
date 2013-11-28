(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (let [[x y] coord]
    (get-in board [x y])))

(defn has-value? [board coord]
  (if (= (value-at board coord) 0) false true))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (map #(value-at board [row %]) (range 0 9)))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (set (map #(value-at board [% col]) (range 0 9)))))

(defn coord-pairs [coords]  
  (for [row coords
        col coords]
    [row col]))

(defn left-corner [coord]
  (let [[x y] coord]
    [(- x (mod x 3)) (- y (mod y 3))]))

(defn block-values [board coord]
  (let [[x y] (left-corner coord)
        row (range x (+ x 3))
        pairs (map #(vector (first %) (+ (second %) (- y x))) 
                (coord-pairs row))]
    (set (map #(value-at board %) pairs))))

(defn valid-values-for [board coord]
  (if (not= 0 (value-at board coord)) 
    #{}
    (set/difference all-values 
      (set/union (block-values board coord) 
                 (row-values board coord)
                 (col-values board coord)))))

(defn filled? [board]
  (every? #(not (contains? (row-values board [% 0]) 0)) (range 0 9)))

(defn valid-section? [section board]
  (every? #(empty? (set/difference all-values %)) (section board)))

(defn rows [board]
  (map #(row-values board [% 0]) (range 0 9)))

(defn valid-rows? [board]
  (valid-section? rows board))

(defn cols [board]
  (map #(col-values board [0 %]) (range 0 9)))

(defn valid-cols? [board]
  (valid-section? cols board))

(defn blocks [board]
  (map #(block-values board [(first %) (second %)]) (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (valid-section? blocks board))

(defn valid-solution? [board]
  ((every-pred filled? valid-rows? valid-cols? valid-blocks?) board))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first
    (for [row (range 0 9)
          col (range 0 9)
          :when (= 0 (get-in board [row col]))]
      [row col])))

(defn solve [board]
  (cond 
    (valid-solution? board) board
    (filled? board) []
    :else (let [coords (find-empty-point board)] 
            (for [value (valid-values-for board coords)
                  solution (solve (set-value-at board 
                                                coords
                                                value))]
      solution))))
