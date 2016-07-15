(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{0 1 2 3 4 5 6 7 8 9})
(def valid-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (pos? (value-at board coord)))

(defn row-values [board coord]
  (let [row-index (first coord)
        row (get board row-index)]
    (set row)))

(defn col-values [board coord]
  (let [col-index (second coord)
        col (map (fn [row] (get row col-index)) board)]
    (set col)))

(defn coord-pairs-combine [rows cols]
  (loop [res-seq `()
         coords cols
         coords-rest rows]
    (let [coord (first coords-rest)]
      (if (empty? coords-rest)
        res-seq
        (recur
          (concat res-seq (map (fn [item] (list coord item)) coords))
          coords
          (rest coords-rest))))))

(defn coord-pairs [coords]
  (coord-pairs-combine coords coords))

(defn block-of-3 [x]
  (int (/ x 3)))

(defn block-expand-3 [x]
  (let [x0 (* x 3)
        x1 (inc x0)
        x2 (inc x1)]
    (list x0 x1 x2)))


(defn block-coords [coord]
  (let [row-start (block-of-3 (first coord))
        col-start (block-of-3 (second coord))
        rows (block-expand-3 row-start)
        cols (block-expand-3 col-start)]
    (coord-pairs-combine rows cols)))

(defn block-values [board coord]
  (let [coords (block-coords coord)
        values (map (fn [coord] (value-at board coord)) coords)]
    (set values)))

(defn valid-values-for [board coord]
  (cond
    (nil? board) #{}
    (nil? coord) #{}
    :else
      (let [value (value-at board coord)
            b-values (block-values board coord)
            r-values (row-values board coord)
            c-values (col-values board coord)
            values (set/union b-values r-values c-values)]
        (if (pos? value)
          #{}
          (set/difference all-values values)))))

(defn filled? [board]
  (cond
    (empty? board) true
    (some (fn [item] (== item 0)) (first board)) false
    :else (recur (rest board))))

(defn rows [board]
  (map set board))

(defn populate-seq [seqs a-seq]
  (loop [res-seqs []
         seqs seqs
         a-seq a-seq]
    (if (empty? a-seq)
      res-seqs
      (recur
        (conj res-seqs (conj (first seqs) (first a-seq)))
        (rest seqs)
        (rest a-seq)))))

(defn cols [board]
  (loop [cols-values [[] [] [] [] [] [] [] [] []]
         rows-values board]
    (if (empty? rows-values)
      (map set cols-values)
      (recur
        (populate-seq cols-values (first rows-values))
        (rest rows-values)))))

(defn blocks [board]
  (map (fn [coord] (block-values board coord)) [[0 0] [0 3] [0 6]
                                                [3 0] [3 3] [3 6]
                                                [6 0] [6 3] [6 6]]))

(defn valid-set? [a-set]
  (= valid-values a-set))

(defn valid-sets? [sets]
  (every? valid-set? sets))

(defn valid-rows? [board]
  (valid-sets? (rows board)))

(defn valid-cols? [board]
  (valid-sets? (cols board)))

(defn valid-blocks? [board]
  (valid-sets? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-rows? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn coord-next [coord]
  (let [x-coord (first coord)
        y-coord (second coord)
        coord-index (+ (* x-coord 9) y-coord)
        coord-index-next (inc coord-index)
        x-coord-next (int (/ coord-index-next 9))
        y-coord-next (mod coord-index-next 9)
        at-max (>= coord-index 80)]
    (if at-max
      nil
      (list x-coord-next y-coord-next))))

(defn find-empty-point [board]
  (loop [coord (list 0 0)
         board board]
    (cond
      (nil? coord) nil
      (has-value? board coord) (recur (coord-next coord) board)
      :else coord)))

(defn solve-value [board coord values]
  (cond
    (filled? board) (if (valid-solution? board) board nil)
    (nil? coord) nil
    (empty? values) nil
    :else
      (let [next-board (set-value-at board coord (first values))
            next-coord (find-empty-point next-board)
            next-values (valid-values-for next-board next-coord)
            solution (solve-value next-board next-coord next-values)]
        (if (nil? solution)
          (solve-value board coord (next values))
          solution))))


(defn solve [board]
  (let [coord (find-empty-point board)]
    (solve-value board coord (valid-values-for board coord))))


