(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (let [value (value-at board coord)]
    (if (zero? value) false true)))

(defn row-values [board coord]
  (let [[r _] coord
        row (get board r)]
    (reduce conj #{} row)))

(defn col-values [board coord]
  (let [[_ c] coord
        col (->> board (flatten) (drop c) (take-nth 9))]
    (reduce conj #{} col)))

(defn coord-pairs [coords]
  (for [x coords y coords] [x y]))

(defn block-values [board coord]
  (let [block-top (map #(* (quot % 3) 3) coord)
        [block-rows, block-cols] (map #(take 3 (iterate inc %)) block-top)
        block-coords (for [r block-rows c block-cols] [r c])]
    (set (map #(value-at board %) block-coords))))

(defn valid-values-for [board coord]
  (if (zero? (value-at board coord))
    (set/difference all-values
                    (set/union (row-values board coord)
                               (col-values board coord)
                               (block-values board coord)))
    #{}))

(defn filled? [board]
  (not (contains? (set (flatten board)) 0)))

(defn rows [board]
  (reduce (fn [memo, r]
            (conj memo (row-values board [r, 0])))
          []
          (range 0 9)))

(defn valid-rows? [board]
  (every? #(= (count %) 9) (rows board)))

(defn cols [board]
  (reduce (fn [memo, c]
            (conj memo (col-values board [0, c])))
          []
          (range 0 9)))

(defn valid-cols? [board]
  (every? #(= (count %) 9) (cols board)))

(defn blocks [board]
  (let [tops (coord-pairs [0 3 6])]
    (reduce (fn [memo, coord] (conj memo (block-values board coord)))
            []
            tops)))

(defn valid-blocks? [board]
  (every? #(= (count %) 9) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [coord [0, 0]]
    (let [val (value-at board coord)
          [r, c] coord
          next-coord (if (= c 8)
                       [(inc r), 0]
                       [r, (inc c)])]
      (cond
        (zero? val) coord
        (= coord [9, 9]) nil
        :else (recur next-coord)))))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      '())
    (let [coord (find-empty-point board)]
      (for [value (valid-values-for board coord)
            solution (solve-helper (set-value-at board coord value))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
