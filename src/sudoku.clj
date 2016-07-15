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
  (let [r-coord (first coord)
        c-coord (second coord)
        at-max? (fn [x] (>= x 9))
        c-coord-next (inc c-coord)
        c-at-max (at-max? c-coord-next)
        r-coord-next (if c-at-max (inc r-coord) r-coord)
        r-at-max (at-max? r-coord-next)]
    (if (and r-at-max c-at-max)
      nil
      (list r-coord-next (if c-at-max 0 c-coord-next)))))

(defn find-empty-point [board]
  (loop [coord (list 0 0)
         board board]
    (cond
      (nil? coord) nil
      (has-value? board coord) (recur (coord-next coord) board)
      :else coord)))

; Example for empty 2x2 board
; - [[[0 0] [0 0]]]
; - [[[1 0] [0 0]]
;    [[2 0] [0 0]]
;    [[3 0] [0 0]]
;    [[4 0] [0 0]]]
; - [[[1 2] [0 0]]
;    [[1 3] [0 0]]
;    [[1 4] [0 0]]
;    [[2 0] [0 0]]
;    [[3 0] [0 0]]
;    [[4 0] [0 0]]]
; - [[[1 2] [3 0]]
;    [[1 2] [4 0]]
;    [[1 3] [0 0]]
;    [[1 4] [0 0]]
;    [[2 0] [0 0]]
;    [[3 0] [0 0]]
;    [[4 0] [0 0]]]
; - [[[1 2] [3 4]]
;    [[1 2] [4 0]]
;    [[1 3] [0 0]]
;    [[1 4] [0 0]]
;    [[2 0] [0 0]]
;    [[3 0] [0 0]]
;    [[4 0] [0 0]]]
; [[1 2] [3 4]]
(defn solve-recur [boards]
  (let [board (first boards)]
    (cond
      (empty? boards) nil
      (filled? board) (if (valid-solution? board) board (recur (rest boards)))
      :else
        (let [point (find-empty-point board)
              values (valid-values-for board point)
              value-boards (map (fn [value] (set-value-at board point value)) values)
              new-boards (concat value-boards (rest boards))]
          (recur new-boards)))))


(defn solve [board]
  (solve-recur (list board)))

