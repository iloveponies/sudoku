(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board [(first coord) (second coord)]))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (set (map (fn [row] (get row (second coord))) board)))

(defn coord-pairs [coords]
  (for [c1 coords c2 coords]
    [c1 c2]))


(defn block-start [coord]
  (let [x (first coord) y (second coord)
        get-x (fn [x y] (cond
                          (< x 3) [0 y]
                          (< x 6) [3 y]
                          :else [6 y]))]
    (cond
      (< y 3) (get-x x 0)
      (>= y 6) (get-x x 6)
      :else (get-x x 3))))

(defn block-values [board coord]
  (let [coords (block-start coord) coordinates-of-block
        (for [c1 [(first coords) (+ (first coords) 1) (+ (first coords) 2)] c2 [(second coords) (+ (second coords) 1)  (+ (second coords) 2)]]
          [c1 c2])]
    (set (map (fn [block-cell-coordinate] (value-at board block-cell-coordinate)) coordinates-of-block))))



(defn valid-values-for [board coord]
 (if (has-value? board coord)
   #{}
   (set/difference all-values (set/union (col-values board coord) (row-values board coord) (block-values board coord)))))

(defn filled? [board]
  (let [all-number-seq (fn [board] (reduce concat board))]
    (not (contains? (set (all-number-seq board)) 0))))

(defn rows [board]
  (map (fn [row-coord] (row-values board [row-coord 0])) (range (count board)) ))

(defn valid-rows? [board]
  (every? (fn [row] (= all-values row)) (rows board)))

(defn cols [board]
  (map (fn [col-coord] (col-values board [0 col-coord])) (range (count board))))

(defn valid-cols? [board]
  (every? (fn [col] (= all-values col)) (cols board)))

(defn blocks [board]
  (map (fn [block-coord] (block-values board block-coord)) (for [c1 [0 3 6] c2 [0 3 6]] [c1 c2])))

(defn valid-blocks? [board]
  (every? (fn [block] (= all-values block)) (blocks board)))

(defn valid-solution? [board]
  (and (filled? board) (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (some (fn [coord] (if (not (has-value? board coord)) coord)) (for [c1 (range 9) c2 (range 9)] [c1 c2])))

(defn solve [board]
  (cond
    (valid-solution? board) board
    (filled? board) []
    :else (let [empty-point (find-empty-point board)
                remaining (valid-values-for board empty-point)]
            (for [number remaining
                  solution (solve (set-value-at board empty-point number))]
              solution))))
