(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})
(def coord-values #{0 1 2 3 4 5 6 7 8})

(def all-coords
    ; returns all valid coordinates for the board
        (for [coords-1 coord-values
              coords-2 coord-values][coords-1 coords-2]))
              
(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board coord]
  (let [[row col] coord](set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
      (set (for [row board](get row col)))))
  
(defn coord-pairs [coords]
    (for [coords-1 coords
          coords-2 coords][coords-1 coords-2]))

; compute the upper-left corner of all blocks on the board
; [[0 0][0 3][0 6][3 0][3 3][3 6][6 0][6 3][6 6]]
(defn block-corners [board]
  (coord-pairs [0 3 6]))
              
; given upper-left corner of block, compute the other coords in the block
; calculate offsets for a generic block (eg (row, row+1) then (row, row+2) etc)
; and apply those offsets to each corner block to get all the block coords
(defn block-coords [block-corner]
  (let [[row col] block-corner]
      (for [offset (coord-pairs [0 1 2])]
          (let [[off-row off-col] offset]
              [(+ row off-row) (+ col off-col)]))))

; yeah, there exists a better way to do this
(defn block-values [board coord]
  (let [[row col] coord]
  (cond
      (and (contains? #{0 1 2} row) (contains? #{0 1 2} col)) (set (map (fn [block-coord] (value-at board block-coord)) (block-coords [0 0])))
      (and (contains? #{0 1 2} row) (contains? #{3 4 5} col)) (set (map (fn [block-coord] (value-at board block-coord)) (block-coords [0 3])))
      (and (contains? #{0 1 2} row) (contains? #{6 7 8} col)) (set (map (fn [block-coord] (value-at board block-coord)) (block-coords [0 6])))
      (and (contains? #{3 4 5} row) (contains? #{0 1 2} col)) (set (map (fn [block-coord] (value-at board block-coord)) (block-coords [3 0])))
      (and (contains? #{3 4 5} row) (contains? #{3 4 5} col)) (set (map (fn [block-coord] (value-at board block-coord)) (block-coords [3 3])))
      (and (contains? #{3 4 5} row) (contains? #{6 7 8} col)) (set (map (fn [block-coord] (value-at board block-coord)) (block-coords [3 6])))
      (and (contains? #{6 7 8} row) (contains? #{0 1 2} col)) (set (map (fn [block-coord] (value-at board block-coord)) (block-coords [6 0])))
      (and (contains? #{6 7 8} row) (contains? #{3 4 5} col)) (set (map (fn [block-coord] (value-at board block-coord)) (block-coords [6 3])))
      :else (set (map (fn [block-coord] (value-at board block-coord)) (block-coords [6 6])))
      )))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union (row-values board coord) (col-values board coord) (block-values board coord)))))

(defn filled? [board]
    (let [filled-rows (map (fn [row] (not (contains? (set row) 0))) board)] ; true/false for each row depending if it's filled
    (reduce (fn [accum next] (and accum next)) true filled-rows)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
    (reduce (fn [accum next] (and accum (= next all-values))) true (rows board)))

(defn cols [board]
    ; grab col-values using first row (any row would do)
    (mapv (fn [row-coord] (col-values board row-coord)) (map (fn [n] [0 n]) (range 9))))

(defn valid-cols? [board]
    (reduce (fn [accum next] (and accum (= next all-values))) true (cols board)))

(defn blocks [board]
    (let [block-starts (block-corners board)] ; the upper-left corner of all 9 blocks on the board
    (map (fn [block-coord-vals] (set (map (fn [coord] (value-at board coord)) block-coord-vals))) (map block-coords block-starts))))


(defn valid-blocks? [board]
    (reduce (fn [accum next] (and accum (= next all-values))) true (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (fn [coord] (not (has-value? board coord))) all-coords)))

(defn solve-helper [current-board]
  (if (valid-blocks? current-board)
      (if (valid-solution? current-board) ; board is filled
            [current-board] ; board has valid solution
            '()) ; board filled but has invalid solution
  (let [next-point (find-empty-point current-board)
        values-to-try (valid-values-for current-board next-point)]
        (for [value-to-try values-to-try
              solution (solve-helper (set-value-at current-board next-point value-to-try))]solution)))) ; board not filled
  
(defn solve [board]
    (first (solve-helper board)))