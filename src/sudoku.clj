(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (reduce (fn [the-set row]
              (conj the-set (get row col)))
            #{}
            board)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-coord-pairs [range1 range2]
  (for [x range1
        y range2]
    [x y]))

(defn get-block [coords]
  (let [[row col] coords]
    (cond
     (and (<= 0 row 2) (<= 0 col 2)) [0 0]
     (and (<= 0 row 2) (<= 3 col 5)) [0 3]
     (and (<= 0 row 2) (<= 6 col 8)) [0 6]
     (and (<= 3 row 5) (<= 0 col 2)) [3 0]
     (and (<= 3 row 5) (<= 3 col 5)) [3 3]
     (and (<= 3 row 5) (<= 6 col 8)) [3 6]
     (and (<= 6 row 8) (<= 0 col 2)) [6 0]
     (and (<= 6 row 8) (<= 3 col 5)) [6 3]
     (and (<= 6 row 8) (<= 6 col 8)) [6 6])))

(defn block-values [board coord]
  (let [[row col] (get-block coord)
        row-indexes [row (inc row) (+ 2 row)]
        col-indexes [col (inc col) (+ 2 col)]
        block-coords (block-coord-pairs row-indexes col-indexes)]
    (reduce (fn [the-set coord]
              (conj the-set (value-at board coord)))
            #{}
            block-coords)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union (row-values board coord) (col-values board coord) (block-values board coord)))))

(defn filled? [board]
  (let [f (fn [ret elems]
            (not (or ret (contains? (set elems) 0))))]
    (reduce f false board)))

(defn rows [board]
  (let [f (fn [ret-vec new-set]
            (conj ret-vec (set new-set)))]
    (reduce f [] board)))

(defn valid-rows? [board]
  (let [f (fn [ret new-row]
            (and ret (empty? (set/difference all-values new-row))))]
    (reduce f true (rows board))))

(defn cols [board]
  (let [f (fn [ret col-coord]
        (conj ret (col-values board [0 col-coord])))]
    (reduce f [] (range 9))))

(defn valid-cols? [board]
  (let [f (fn [ret new-col]
            (and ret (empty? (set/difference all-values new-col))))]
    (reduce f (cols board))))

(def all-blocks-coords [[0 0] [0 3] [0 6] [3 0] [3 3] [3 6] [6 0] [6 3] [6 6]])

(defn blocks [board]
  (let [f (fn [ret-vec block-coord]
            (conj ret-vec (block-values board block-coord)))]
    (reduce f [] all-blocks-coords)))

(defn valid-blocks? [board]
  (let [f (fn [ret new-block]
            (and ret (empty? (set/difference all-values new-block))))]
    (reduce f (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board) (filled? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn check-a-row [row]
  (loop [row row
        col-index 0]
    (if (empty? row)
      nil
      (if (= (first row) 0)
        col-index
        (recur (rest row) (inc col-index))))))

(defn find-empty-point [board]
  (loop [row-list board
         row-index 0]
    (if (empty? row-list)
      nil
      (if (check-a-row (first row-list))
        [row-index (check-a-row (first row-list))]
        (recur (rest row-list) (inc row-index))))))

(defn solve-done-checker [board]
  [board])

(defn sudoku-solve-helper [current-board]
  (if (filled? current-board)
    (solve-done-checker current-board)
    (let [first-empty (find-empty-point current-board)
          valid-values (valid-values-for current-board first-empty)]
      (for [elem valid-values
            solution (sudoku-solve-helper (set-value-at current-board first-empty elem))]
        solution))))


(defn solve [board]
  (first (sudoku-solve-helper board)))









