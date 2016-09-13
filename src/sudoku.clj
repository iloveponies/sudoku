(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def sudoku-board
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= (value-at board coord) 0)
    false
    true))

(defn row-values [board coord]
  (let [[x y] coord]
    (set (get board x))))

(defn col-values [board coord]
  (let [[k i] coord]
        (set (map (fn [x] (get x i)) board))))

(defn coord-pairs [coords]
  (into [] (for [row coords
        col coords]
        [row col])))

(defn block-values [board coord]
  (let [top-left-coord
          (fn [coord]
            (let [[row col] coord]
              [(- row (mod row 3)) (- col (mod col 3))]))
        [top-left-x top-left-y] (top-left-coord coord)]
    (set (for [x (range 3) y (range 3)]
           (value-at board [(+ top-left-x x) (+ top-left-y y)])))))

(defn valid-values-for [board coord]
  (let [block-vals (block-values board coord)
        row-vals (row-values board coord)
        col-vals (col-values board coord)
        combined-vals (set/union block-vals row-vals col-vals)]
      (if (has-value? board coord)
        #{}
        (set/difference all-values combined-vals))))

(defn filled? [board]
  (let [board-values
          (fn [board]
            (set (for [x (range 9) y (range 9)]
              (value-at board [x y]))))]
    (not (contains? (board-values board) 0))))

(defn rows [board]
  (into [] (for [x (range 9)]
          (row-values board [x 0]))))


(defn valid-rows? [board]
  (let [all-rows (rows board)]
    (loop [each-row all-rows]
            (cond
              (empty? each-row) true
              (not (= (first each-row) all-values)) false
              :else (recur (rest each-row))))))

(defn cols [board]
  (into [] (for [y (range 9)]
        (col-values board [0 y]))))

(defn valid-cols? [board]
    (let [all-cols (cols board)]
      (loop [each-col all-cols]
            (cond
              (empty? each-col) true
              (not (= (first each-col) all-values)) false
              :else (recur (rest each-col))))))

(defn blocks [board]
  (let [blocks-of-three (reduce (fn [res x] (concat res (partition 3 x))) [] board)
        blocks-of-nine-of-three (partition 9 blocks-of-three)
        block-sets (map (fn [x]
              (for [row-num (range 3)]
                (set
                  (concat
                    (nth x row-num)
                    (nth x (+ 3 row-num))
                    (nth x (+ 6 row-num)))))) blocks-of-nine-of-three)]
      (into [] (reduce
        (fn [res x]
          (concat res x)) [] block-sets))))

(defn valid-blocks? [board]
  (let [all-blocks (blocks board)]
    (loop [each-block all-blocks]
          (cond
            (empty? each-block) true
            (not (= (first each-block) all-values)) false
            :else (recur (rest each-block))))))

(defn valid-solution? [board]
  (if (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)) true false))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [nulls-of-this-row (fn [i row]
      (for [x (range 9)
            :let [row-val (nth row x)]
            :when (= 0 row-val)]
        [i x]))
    get-nulls (fn [index boardd null-coords]
                (if (empty? boardd)
                  null-coords
                  (recur (inc index) (rest boardd) (concat null-coords (nulls-of-this-row index (first boardd))))))]
    (get-nulls 0 board [])))

(defn solve [board]
  nil)
