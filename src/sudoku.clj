(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[x y] coord]
    (set (get board x))))

(defn col-values [board coord]
  (let [[x y] coord]
    (set (map (fn [row] (get row y)) board))))

(defn coord-pairs
  ([coords]

    (for [x coords
          y coords]
      [x y]))
  ([xs ys]
    (for [x xs
          y ys]
      [x y])))

(defn block-values
  ([board coord]
    (let [[x y] coord
          coord-range (fn [n] (range (* (int (/ n 3)) 3) (+ (* (int (/ n 3)) 3) 3)))
          xs (coord-range x)
          ys (coord-range y)]
      (set (map (partial value-at board)
                (coord-pairs xs ys))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))))

(defn filled? [board]
  (not (contains? (set (flatten board)) 0)))

(defn rows [board]
  (map (partial row-values board) (for [x (range 0 9)]
                                    [x 0])))

(defn cols [board]
  (map (partial col-values board) (for [y (range 0 9)]
                                    [0 y])))

(defn blocks [board]
  (map (partial block-values board) (for [x (range 0 9 3)
                                          y (range 0 9 3)]
                                      [x y])))

(defn valid-rows? [board]
  (apply = (conj (rows board) all-values)))

(defn valid-cols? [board] ;meh
  (apply = (conj (cols board) all-values)))

(defn valid-blocks? [board] ;meh
  (apply = (conj (cols board) all-values)))

(defn valid-solution? [board] ;meh
  (apply = [(valid-rows? board) (valid-cols? board) (valid-blocks? board) true]))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-points [board]
  (filter (fn [p] (zero? (value-at board p)))
          (for [x (range 0 9)
                y (range 0 9)]
            [x y])))

(defn find-empty-point [board]
  (first (find-empty-points board)))

(defn solve [board]
  (let [empty-points (find-empty-points board)
        first-empty-point (first empty-points)
        fillable-points (filter (fn [p] (= (count (valid-values-for board p)) 1))
                                empty-points)
        new-board (reduce (fn [board p] (set-value-at board p (first (valid-values-for board p))))
                          board
                          fillable-points)]
    (cond (valid-solution? board)
            board
          (or (empty? empty-points)
              (some empty? (map (partial valid-values-for board) empty-points)))
            nil
          (not (= board new-board))
            (solve new-board)
          (= board new-board)
            (first
              (filter valid-solution?
                      (map (fn [board] (solve board))
                           (map (partial set-value-at board first-empty-point)
                                (valid-values-for board first-empty-point))))))))

