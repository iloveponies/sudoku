(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(def sudoku-board
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(def solved-board
  (board [[5 3 4 6 7 8 9 1 2]
          [6 7 2 1 9 5 3 4 8]
          [1 9 8 3 4 2 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 3 7 9 1]
          [7 1 3 9 2 4 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [3 4 5 2 8 6 1 7 9]]))

(def before-change
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(def after-change
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 4 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board coord]
  (set (get-in board [(first coord)])))

(defn col-values [board coord]
  (set (reduce conj
                (for [row board
                      vals [(get-in row [(last coord)])]] vals)
                []
                )))

(defn coord-pairs [coords]
  (vec (for [row coords
              col coords]
          [row col])))

(defn top-left-of-block
  "returns the top-left coordinate of the coord pair's block"
  [coord]
  [(* 3 (int (/ (first coord) 3))) (* 3 (int (/ (second coord) 3)))])

(defn block-values
  "gets a list of all the coordinate pairs in the target coordinate's block"
  [board coord]
  (let [block-origin (top-left-of-block coord)
        block-end    (map (fn [x] (+ 3 x)) block-origin)
        xy-ranges    (map (fn [x y] [x y]) block-origin block-end)]
    (set (map (fn [x] (value-at board x ))
              (for [x (apply range (first xy-ranges))
                    y (apply range (second xy-ranges))]
                [x y])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (set/union (block-values board coord)
                               (row-values board coord)
                               (col-values board coord)))))
(defn rows [board]
  (vec (map (fn [x] (row-values board [x 0])) (range 0 9))))

(defn filled? [board]
  (let [all-values-set
        (reduce set/union (rows board))]
    (not (contains? all-values-set 0))))

(defn valid-rows? [board]
  (every? (fn [a-set] (= a-set all-values)) (rows board)))

(defn cols [board]
  (vec (map (fn [y] (col-values board [0 y])) (range 0 9))))

(defn valid-cols? [board]
  (every? (fn [a-set] (= a-set all-values)) (cols board)))

(defn blocks [board]
  (vec (for [coord (coord-pairs [0 3 6])]
         (block-values board coord))))

(defn valid-blocks? [board]
  (every? (fn [a-set] (= a-set all-values)) (blocks board)))

(defn valid-solution? [board]
  (and (valid-blocks? board) (valid-rows? board) (valid-cols? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [all-points (coord-pairs (range 0 9))]
    (loop [coords all-points]
      (cond
        (empty? coords)
        nil
        (has-value? board (first coords))
        (recur (rest coords))
        :else
        (first coords)))))

(defn solve-helper [board]
  (let [coord (find-empty-point board)
        remaining (valid-values-for board coord)]
    (if (nil? coord)
      (if (valid-solution? board)
        [board]
        [])
      (for [elem remaining
            solution (solve-helper (set-value-at board coord elem))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
