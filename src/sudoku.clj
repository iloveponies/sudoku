(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (let [val (get-in board coord)]
    (and (not (nil? val))
         (not= 0 val))))

(defn row-values [board [row col]]
  (set (get board row)))

(defn col-values [board [row col]]
  (set (map (fn [x] (get x col)) board)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-values [board [row col]]
  (let [top-left-row (* 3 (quot row 3))
        top-left-col (* 3 (quot col 3))]
    (set (for [x (range top-left-row (+ top-left-row 3))
               y (range top-left-col (+ top-left-col 3))]
           (value-at board [x y])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [rowVals (row-values board coord)
          colVals (col-values board coord)
          blkVals (block-values board coord)
          combined (set/union rowVals colVals blkVals)]
      (set/difference all-values combined))))

(defn filled? [board]
  (empty? (filter (fn [x] (= 0 x)) (flatten board))))

(defn rows [board]
  (map set board))

(defn board-length [board]
  (count (get board 0)))

(defn valid-rows? [board]
  (let [r (rows board)
        length (board-length board)
        num-valid (count (filter (fn [row] (= row all-values)) r))]
    (= length num-valid)))

(defn cols [board]
   (let [num-cols (board-length board)]
    (for [y (range 0 num-cols)]
      (set (col-values board [0 y])))))

(defn valid-cols? [board]
  (let [c (cols board)
        length (board-length board)
        num-valid (count (filter (fn [col] (= col all-values)) c))]
    (= length num-valid)))

(defn blocks [board]
  (let [num-cols (board-length board)
        indicies (range 0 (- (+ num-cols 1) 3) 3)]
    (for [x indicies
          y indicies]
      (block-values board [x y]))))

(defn valid-blocks? [board]
  (let [b (blocks board)
        length (board-length board)
        num-valid (count (filter (fn [blk] (= blk all-values)) b))]
    (= length num-valid)))

(defn valid-solution? [board]
  (and (valid-rows? board) (and (valid-cols? board) (valid-blocks? board))))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [coordinates (coord-pairs (range 0 (board-length board)))]
    (first (filter (fn [c] (not (has-value? board c))) coordinates))))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      [])
    (let [c (find-empty-point board)]
      (apply concat (for [v (valid-values-for board c)]
                      (solve-helper (set-value-at board c v)))))))

(defn solve [board]
  (first (filter seq (solve-helper board))))

