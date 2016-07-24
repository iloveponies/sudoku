(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board coord]
  (let [[y _] coord]
    (set (get board y))))

(defn col-values [board coord]
  (let [[_ x] coord]
    (set (map (fn [y] (get y x)) board))))

(defn coord-pairs [coords]
  (for [y coords
        x coords]
    [y x]))

(defn top-left [coord]
  (let [[y x] coord]
    [(- y (mod y 3)) (- x (mod x 3))]))

(defn block-values [board coord]
  (let [[tly tlx] (top-left coord)]
     (set (for [y (range 3)
                x (range 3)]
            (value-at board [(+ tly y) (+ tlx x)])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference #{1 2 3 4 5 6 7 8 9}
                    (set/union (row-values board coord)
                               (col-values board coord)
                               (block-values board coord)))))

(defn filled? [board]
  (every? (fn [coord] (has-value? board coord))
          (coord-pairs (range 9))))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? (fn [rownums] (= rownums (set (range 1 10))))
          (rows board)))

(defn cols [board]
  (map set (for [col (range 9)]
             (col-values board [0 col]))))

(defn valid-cols? [board]
  (every? (fn [colnums] (= colnums (set (range 1 10))))
          (cols board)))

(defn blocks [board]
  (for [y [0 3 6]
        x [0 3 6]]
    (block-values board [y x])))

(defn valid-blocks? [board]
  (every? (fn [blocknums] (= blocknums (set (range 1 10))))
          (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (fn [coord] (not (has-value? board coord)))
                 (coord-pairs (range 9)))))

(defn solve-help [board]
  (cond (and (filled? board) (valid-solution? board)) [board]
        (and (filled? board) (not (valid-solution? board))) []
        :else (let [coord (find-empty-point board)]
                (for [valids (valid-values-for board coord)
                      solution (solve-help (set-value-at board coord valids))]
                  solution))))

(defn solve [board]
  (first (solve-help board)))
