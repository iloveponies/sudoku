(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board coord]
  (set (board (coord 0))))

(defn col-values [board coord]
  (reduce (fn [a-set row] (conj a-set (row (coord 1)))) #{} board))

(defn coord-pairs [coords]
  (for [x coords y coords]
    [x y]))

(defn block-values [board coord]
  (let [block-size 3
        top-left-coord (fn [coord]
                          (let [[row col] coord]
                            [(* block-size (quot row block-size))
                             (* block-size (quot col block-size))]))
        block-coords (fn [top-left]
                 (let [[row col] top-left]
                   (for [x (map (partial + row) [0 1 2])
                         y (map (partial + col) [0 1 2])]
                     [x y])))]
        (reduce (fn [a-set coord] (conj a-set (value-at board coord)))
                #{}
                (block-coords (top-left-coord coord)))))


(defn block-helper [coords]
  (let [[x y] coords]
    (for [i [x (+ x 1) (+ x 2)]
          j [y (+ y 1) (+ y 2)]]
      (conj [] i j))))

(defn block-values [board coord]
  (let [[x y] coord
        value (fn [x] (value-at board x))]
    (set (map value (block-helper [(* 3 (quot x 3)) (* 3 (quot y 3))])))))

(defn valid-values-for [board coord]
  (let [all-values #{1 2 3 4 5 6 7 8 9}
        checked-vals [row-values col-values block-values]]
    (if (has-value? board coord)
      #{}
      (set/difference
       all-values
       (reduce (fn [a-set f] (set/union a-set (f board coord))) #{} checked-vals)))))

(defn filled? [board]
  (loop [coords (coord-pairs (range 10))]
    (cond
     (= 0 (value-at board (first coords))) false
     (empty? coords) true
     :else (recur (rest coords)))))

(defn rows [board]
  (for [x (range (count board))]
    (row-values board [x nil])))

(defn valid-rows? [board]
  (let [all-values (set (range 1 10))]
    (every? (fn [row] (= all-values row)) (rows board))))

(defn cols [board]
  (for [x (range (count board))]
    (col-values board [nil x])))

(defn valid-cols? [board]
  (let [all-values (set (range 1 10))]
    (every? (fn [col] (= all-values col)) (cols board))))

(defn blocks [board]
  (let [corners (coord-pairs [0 3 6])]
    (map (partial block-values board) corners)))

(defn valid-blocks? [board]
  (let [all-values (set (range 1 10))]
    (every? (fn [block] (= all-values block)) (blocks board))))

(defn valid-solution? [board]
  (let [preds [valid-rows? valid-blocks? valid-cols?]]
  (every? (fn [p?] (p? board)) preds)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (complement (partial has-value? board)) (coord-pairs (range 10)))))


(defn solve [board]
  (cond
   (valid-solution? board) board
   (filled? board) []
   :else (let [empty-point (find-empty-point board)
               valid-vals (valid-values-for board empty-point)]
           (for [v valid-vals
                 solution (solve (set-value-at board empty-point v))]
             solution))))
