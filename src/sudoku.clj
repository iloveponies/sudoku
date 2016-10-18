(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
 (get-in board coord))

(defn has-value? [board coord]
 (if (== 0 (value-at board coord)) false true))

(defn row-values [board coord]
 (let [[x _] coord]
   (set (get-in board [x]))))

(defn col-values [board [_ col]]
 (set (map #(get % col) board)))

(defn coord-pairs [coords]
 (for [row coords
       col coords]
   (vector row col)
  ))

(defn block-values [board [x y]]
  (let [block-topleft-x (- x (mod x 3))
        block-topleft-y (- y (mod y 3))
        block-rows (range block-topleft-x (+ block-topleft-x 3))
        block-cols (range block-topleft-y (+ block-topleft-y 3))]
    (set
      (for [row block-rows
            col block-cols]
        (value-at board [row col])))))

(defn valid-values-for [board coord]
 (cond
   (has-value? board coord) #{}
   :else (clojure.set/difference (set (range 0 10)) (clojure.set/union (row-values board coord) (col-values board coord) (set (block-values board coord))))
 )
)

(defn filled? [board]
 (not (some zero? (apply concat board)))
)

(defn rows [board]
 (map ( fn [x] (set (get board x))) (range 0 9)
 )
)

(defn valid-rows? [board]
 (every? (fn [x] (= x #{1 2 3 4 5 6 7 8 9})) (rows board))
)

(defn cols [board]
  (for [col (range 0 9)]
 (set (map #(get % col) board)))
)

(defn valid-cols? [board]
 (every? (fn [x] (= x #{1 2 3 4 5 6 7 8 9})) (cols board))
)

(defn blocks [board]
 (for [rvals (range 0 9 3)
       cvals (range 0 9 3)]
   (block-values board [rvals cvals])
 )
)

(defn valid-blocks? [board]
 (every? (fn [x] (= x #{1 2 3 4 5 6 7 8 9})) (blocks board))
)

(defn valid-solution? [board]
 (and (valid-blocks? board) (valid-cols? board) (valid-rows? board))
)

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value)
)

(defn find-empty-point [board]
 (first (filter (fn [coord] (== 0 (value-at board coord))) (coord-pairs (range 0 9))))
)

(defn solve-help [board]
  (cond (and (filled? board) (valid-solution? board)) [board]
        (and (filled? board) (not (valid-solution? board))) []
        :else (let [coord (find-empty-point board)]
                (for [valids (valid-values-for board coord)
                      solution (solve-help (set-value-at board coord valids))]
                  solution))))

 (defn solve [board]
  (first (solve-help board)))
