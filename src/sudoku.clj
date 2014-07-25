(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== (value-at board coord) 0)))

(defn row-values [board coord]
  (let [row-vals (get board (get coord 0))]
    (set row-vals)))

(defn col-values [board coord]
  (let [no-rows (count board),
        col-no (last coord),
        val-in-col (fn[row] (value-at board [row col-no])),
        col-vals (map val-in-col (range no-rows))]
    (set col-vals)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board coord]
  (let [top-left-single (fn[x] (- x (mod x 3))),
        top-left (fn[c] [(top-left-single (first c)) (top-left-single (last c))]),
        [top-left-row top-left-col] (top-left coord),
        block-coords (for [row (range top-left-row (+ top-left-row 3)),
                           col (range top-left-col (+ top-left-col 3))]
                           [row col]),
        val-at-coord (fn[c] (value-at board c)),
        block-values-list (map val-at-coord block-coords)]
    (set block-values-list)))
    
(defn valid-values-for [board coord]
  (if (contains? all-values (value-at board coord))
    #{}
    (set/difference all-values 
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))))

(defn filled? [board]
  (let [all-entries (reduce concat board),
        is-value (fn[x] (contains? all-values x))]
    (every? is-value all-entries)))

(defn rows [board]
  (let [list->set (fn[x] (set x))]
    (map list->set board))) 

(defn set-equals [a,b]
 (empty? (set/difference a b)))

(defn valid-rows? [board]
  (let [valid-row (fn[r] (set-equals all-values r))]
    (every? valid-row (rows board))))

(defn cols [board]
  (let [no-rows (count board),
        colvals->set (fn[col] (set (col-values board [0 col])))]
    (map colvals->set (range no-rows))))

(defn valid-cols? [board]
  (let [valid-col (fn[c] (set-equals all-values c))]
    (every? valid-col (cols board))))

(defn blocks [board]
  (let [coords [0 3 6],
        block-coords (for [row coords,
                           col coords]
                       [row col]),
        coord->set (fn[coord] (set (block-values board coord)))]
    (map coord->set block-coords)))

(defn valid-blocks? [board]
  (let [valid-block (fn[b] (set-equals all-values b))]
    (every? valid-block (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board) 
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [no-rows (count board),
        coords (coord-pairs (range no-rows)),
        empty-coord? (fn[coord] (not (contains? all-values (value-at board coord)))),
        empties (filter empty-coord? coords)]
    (first empties))) 

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      [])
    (let [empty (find-empty-point board),
          poten-values (valid-values-for board empty)]
      (for [value poten-values
            solution (solve-helper (set-value-at board empty value))]
        solution))))

(defn solve [board]
  (let [not-empty? (fn[x] (not (empty? x)))]
    (first (filter not-empty? (solve-helper board)))))

