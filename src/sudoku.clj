(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (let [col (for [row [0 1 2 3 4 5 6 7 8]]
              (value-at board [row (second coord)]))]
    (set col)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-values [board coord]
  (let [top-left (fn [x]
                   (cond
                     (< x 3)
                       0
                     (< x 6)
                       3
                     :else
                       6))
        top-left-x (top-left (first coord))
        top-left-y (top-left (second coord))
        values (for [x [top-left-x (+ 1 top-left-x) (+ 2 top-left-x)]
                     y [top-left-y (+ 1 top-left-y) (+ 2 top-left-y)]]
                 (value-at board [x y]))]
    (set values)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (block-values board coord)
                    (row-values board coord)
                    (col-values board coord))))

(defn filled? [board]
  (let [contains-zero? (fn [row] (some #{0} row))]
    (not (first (filter contains-zero? board)))))

(defn rows [board]
  (let [halp (fn [rows row]
               (conj rows (set row)))]
    (reduce halp [] board)))

(defn valid-rows? [board]
  (let [checker (fn [row]
                  (and (not (contains? row 0))
                       (empty? (set/difference all-values row))))
        the-rows (rows board)]
    (every? checker the-rows)))

(defn cols [board]
  (for [col [0 1 2 3 4 5 6 7 8]]
        (col-values board [0 col])))

(defn valid-cols? [board]
  (let [checker (fn [col]
                  (and (not (contains? col 0))
                       (empty? (set/difference all-values col))))
        the-cols (cols board)]
    (every? checker the-cols)))

(defn blocks [board]
  (for [x [0 3 6]
        y [0 3 6]]
    (block-values board [x y])))

(defn valid-blocks? [board]
  (let [checker (fn [block]
                  (and (not (contains? block 0))
                       (empty? (set/difference all-values block))))
        the-blocks (blocks board)]
    (every? checker the-blocks)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [coords (coord-pairs [0 1 2 3 4 5 6 7 8])
        checker (fn [coord] (not (has-value? board coord)))]
    (first (filter checker coords))))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      '())
    (let [empty-loc (find-empty-point board)]
      (for [value all-values
            solution (solve-helper (set-value-at board empty-loc value))]
        solution))))

(defn solve [board]
  nil)
