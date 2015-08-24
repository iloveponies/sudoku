(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (set (map #(get % col) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board [row col]]
  (let [top-x (- row (mod row 3))
        top-y (- col (mod col 3))]
  (into #{} (for [x (range top-x (+ top-x 3))
                  y (range top-y (+ top-y 3))]
        (value-at board [x y])))))

(defn valid-values-for [board coord]
  (let [invalid
        (set/union
         (block-values board coord)
         (row-values board coord)
         (col-values board coord))]
  (if (has-value? board coord) #{}
    (set/difference all-values invalid))))

(defn filled? [board]
  (every? #(every? (complement zero?) %) board))

(defn valid-* [board f]
  (every? #(= 9 (count %)) (f board)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (valid-* board rows))

(defn cols [board]
  (map #(col-values board [nil %]) (range 0 9)))

(defn valid-cols? [board]
  (valid-* board cols))

(defn blocks [board]
  (for [x (range 0 9) :when (= (mod x 3) 0)
        y (range 0 9) :when (= (mod y 3) 0)]
  (block-values board [x y])))

(defn valid-blocks? [board]
  (valid-* board blocks))

(defn valid-solution? [board]
  (and (valid-blocks? board)
       (valid-rows? board)
       (valid-cols? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (into []
      (first (for [x (range 9)
                   y (range 9)
                   :when (= (value-at board [x y]) 0)]
               [x y]))))


(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      nil)
    (let [free (find-empty-point board)]
      (first (filter (complement empty?)
                     (for [value (valid-values-for board free)]
                     (solve (set-value-at board free value))))))))
