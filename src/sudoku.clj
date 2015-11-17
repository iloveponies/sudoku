(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (> (value-at board coord) 0))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (set (map #(get %1 (second coord)) board )))

(defn coord-pairs
  ([coords]
    (for [row coords
          col coords]
      [row col])))

(defn top-left [coord]
  (let [[row col] coord]
    (vector (- row (int (mod row 3))) (- col (int (mod col 3))))))

(defn block-values [board coord]
  (let [[x-coord y-coord] (top-left coord)]
    (set (for [x (range 3) y (range 3)] (value-at board [(+ x-coord x) (+ y-coord y)])))))

(defn valid-values-for [board coord]
  (let [all-values #{1 2 3 4 5 6 7 8 9}]
  (if (has-value? board coord) #{}
  (clojure.set/difference all-values (clojure.set/union (col-values board coord) (row-values board coord) (block-values board coord))))))

(defn filled? [board]
  (nil? (some #{0} (set (for [x (range 9) y (range 9)] (value-at board [x y]))))))

(defn rows [board]
  (for [row board] (set row)))


(defn valid-rows? [board]
  (let [all #{1 2 3 4 5 6 7 8 9}]
    (every? true? (map #(= all %) (rows board)))))

(defn cols [board]
  (for [x (range 0 9)]
    (set (col-values board [0 x]))))

(defn valid-cols? [board]
  (let [all-values #{1 2 3 4 5 6 7 8 9}]
    (every? true? (map #(= all-values %) (cols board)))))

(defn blocks [board]
  (seq (for [x (range 0 9 3) y (range 0 9 3)] (block-values board [x y]))))

(defn valid? [f board]
  (let [remaining #(set/difference all-values %)]
    (every? identity (map empty? (map remaining (f board))))))

(defn valid-blocks? [board]
  (valid? blocks board))

(defn valid-solution? [board]
  (and (valid-cols? board) (valid-rows? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [coords (for [y (range 0 9)
                     x (range 0 9)]
                 [y x])
        func (fn [value] (= 0 (value-at board value)))]
    (first (filter func coords))))

(defn find-empty-points [board]
  (let [coords (for [y (range 0 9)
                     x (range 0 9)]
                 [y x])]
    (filter #(= 0 (value-at board %)) coords)))

(defn solve [board]
  (if (valid-solution? board)
    board
    (let [empty-points (find-empty-point board)]
         (for [valid (valid-values-for board empty-points)
               solution (solve (set-value-at board empty-points valid))]
           solution))))
