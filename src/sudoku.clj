(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{0 1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board coord]
  (apply sorted-set (get board (first coord))))

(defn col-values [board coord]
  (apply
    sorted-set
    (for [row board]
      (get row (second coord)))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-values [board coord]
  (apply sorted-set
         (let [x (first coord)
               y (second coord)
               a (- x (mod x 3))
               b (- y (mod y 3))]
           (for [i (range a (+ a 3))
                 j (range b (+ b 3))]
             (value-at board [i j])))))


(defn valid-values-for [board coord]
  (if (not (= 0 (value-at board coord)))
    #{}
    (set/difference
      all-values
      (set/union
        (col-values board coord)
        (row-values board coord)
        (block-values board coord)
        #{0}))))

(defn filled? [board]
  (not (contains? (apply sorted-set (flatten board)) 0)))

(defn rows [board]
  (for [row board]
    (apply sorted-set row)))

(defn pred-and [acc x] (and acc x))

(defn valid-set? [nums]
  (= (set/difference all-values nums) #{0}))

(defn valid-collection? [nums]
  (reduce
    pred-and
    true
    (for [n nums]
      (valid-set? n))))

(defn valid-rows? [board]
  (valid-collection? (rows board)))

(defn cols [board]
  (for [col (range 0 9)]
    (apply sorted-set (col-values board [0 col]))))

(defn valid-cols? [board]
  (valid-collection? (cols board)))

(defn blocks [board]
  (for [coord (coord-pairs [0 3 6])]
    (apply sorted-set (block-values board coord))))

(defn valid-blocks? [board]
  (valid-collection? (blocks board)))

(defn valid-solution? [board]
  (and
    (valid-rows? board)
    (valid-cols? board)
    (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (fn [coord] (= 0 (value-at board coord)))
    (coord-pairs (range 0 9)))))

(defn solve [board]
  (cond
    (valid-solution? board) board
    (filled? board) []
    :else (let [pos (find-empty-point board)]
            (for [candidate (valid-values-for board pos)
                  solution (solve (set-value-at board pos candidate))]
              solution))))

