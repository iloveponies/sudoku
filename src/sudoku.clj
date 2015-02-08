(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})


(defn value-at [board coord]
  (get (get board (get coord 0)) (get coord 1)))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (reduce (fn [acc inp] (conj acc inp)) #{} (get board (get coord 0))))

(defn col-values [board coord]
  (reduce (fn [acc kulli] (conj acc (get kulli (second coord)))) #{} board))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-upleft-coord [coord]
  (let [[row col] coord]
    [(- row (mod row 3)) (- col (mod col 3))]))

(defn block-coord-helper [coord]
  (let [corner (block-upleft-coord coord)
        all-y (for [n [0 1 2]] (+ n (first corner)))
        all-x (for [n [0 1 2]] (+ n (second corner)))]
    (for [i all-y
          j all-x]
      [i j])))

(defn block-values [board coord]
  (let [all-coords (block-coord-helper coord)]
    (reduce (fn [acc g] (conj acc (value-at board g))) #{} all-coords)))

(defn valid-values-for [board coord]
  (let [block (block-values board coord)
        row (row-values board coord)
        col (col-values board coord)
        used (set/union block row col)]
    (if (not (zero? (value-at board coord)))
      #{}
      (set/difference all-values used))))

(defn filled? [board]
  (not (contains? (set (flatten board)) 0)))

(defn valid? [sets]
  (every? #(= all-values %) sets))

(defn rows [board]
  (reduce (fn [acc row] (conj acc (set row))) [] board))

(defn valid-rows? [board]
  (valid? (rows board)))

(defn cols [board]
  (let [get-col (fn [x] (set (col-values board [0 x])))
        helper (fn [acc x] (conj acc (get-col (count acc))))]
    (reduce helper [] board)))

(defn valid-cols? [board]
  (valid? (cols board)))

(defn blocks [board]
  (let [beg-the-block (for [x [0 3 6]
                            y [0 3 6]]
                        [x y])
        blockses (for [k beg-the-block]
                   (block-values board k))]
    (map set blockses)))

(defn valid-blocks? [board]
  (valid? (blocks board)))

(defn valid-solution? [board]
  (and (valid-cols? board) (valid-rows? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  nil)

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
