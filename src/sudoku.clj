(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board [(first coord) (second coord)]))

(defn has-value? [board coord]
  (if (zero? (value-at board coord))
    false
    true))

(defn row-values [board coord]
  (loop [col 0
         numbers '#{}]
    (if (= 9 col)
      numbers
      (recur (inc col) (conj numbers (value-at board [(first coord) col]))))))

(defn col-values [board coord]
  (loop [row 0
         numbers '#{}]
    (if (= 9 row)
      numbers
      (recur (inc row) (conj numbers (value-at board [row (second coord)]))))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board coord]
  (let [helper (fn [cord] [(* 3 (int (/ (first cord) 3))) (* 3 (int (/ (second cord) 3)))])]
    (set (for [rowchange [0 1 2]
                colchange [0 1 2]]
      (value-at board (vector (+ (first (helper coord)) rowchange) (+ (second (helper coord)) colchange)))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
  '#{}
  (set/difference all-values (set/union (row-values board coord) (col-values board coord) (block-values board coord)))))

(defn filled? [board]
  (every? identity (for [row (range 0 9)
        col (range 0 9)]
    (if (has-value? board [row col])
      true
      false))))

(defn rows [board]
  (for [row (range 0 9)]
    (row-values board [row 0])))

(defn valid-rows? [board]
  (every? identity (for [row (range 0 9)]
    (if (= (nth (rows board) row) all-values)
      true
      false))))

(defn cols [board]
  (for [col (range 0 9)]
    (col-values board [0 col])))

(defn valid-cols? [board]
  (every? identity (for [col (range 0 9)]
    (if (= (nth (cols board) col) all-values)
      true
      false))))

(defn blocks [board]
  (for [row [0 3 6]
        col [0 3 6]]
    (block-values board [row col])))

(defn valid-blocks? [board]
  (every? identity (for [block (range 0 9)]
    (if (= (nth (blocks board) block) all-values)
      true
      false))))

(defn valid-solution? [board]
  (and (valid-cols? board) (valid-rows? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
    (some #(if (not (empty? %)) %) (for [row (range 0 9)
                          col (range 0 9)]
    (if (has-value? board [row col])
      []
      [row col]))))

(defn solve-helper [board]
  (cond
    (valid-solution? board) board
    (filled? board) '()
    :else (let [target (find-empty-point board)
                possibilities (valid-values-for board target)]
            (for [pos possibilities
                  solution (solve-helper (set-value-at board target pos))]
              solution))))

(defn solve [board]
  (solve-helper board))
