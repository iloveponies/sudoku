(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board coord]
  (let [[row col] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [dodo (fn [a-set row]
               (conj a-set (get row (second coord))))]
    (reduce dodo #{} board)))


(defn coord-pairs [coords]
  (for [i coords
        j coords]
    (vector i j)))

(defn block-values [board coord]
  (let [helper (fn [x] (- x (mod x 3)))
        [row col] coord
        top-row (helper row)
        top-col (helper col)
        rows (range top-row (+ top-row 3))
        cols (range top-col (+ top-col 3))]
    (set (for [i rows
          j cols]
      (value-at board [i j])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union
                                (block-values board coord)
                                (row-values board coord)
                                (col-values board coord)))))

(defn filled? [board]
  (let [r (range 0 9)
        all (set (for [i r
                       j r]
                   (value-at board [i j])))]
    (not (contains? all 0))))

(defn rows [board]
  (for [i (range 0 9)]
    (row-values board [i 0])))

(defn valid-rows? [board]
  (not (contains? (set (for [row (rows board)]
    (= row all-values))) false)))

(defn cols [board]
  (for [j (range 0 9)]
    (col-values board [0 j])))

(defn valid-cols? [board]
  (not (contains? (set (for [col (cols board)]
    (= col all-values))) false)))

(defn blocks [board]
  (for [i [0 3 6]
        j [0 3 6]]
    (block-values board [i j])))

(defn valid-blocks? [board]
  (not (contains? (set (for [block (blocks board)]
    (= block all-values))) false)))

(defn valid-solution? [board]
  (and
   (valid-rows? board)
   (valid-cols? board)
   (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [coord [0 0]]
    (if (has-value? board coord)
      (let [next [(first coord) (inc (second coord))]
            next (if (< (second next) 9) next [(inc (first next)) 0])]
        (recur next))
      coord)))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      '())
    (let [coord (find-empty-point board)]
      (loop [pos-vals (valid-values-for board coord)]
        (if (empty? pos-vals)
          '()
          (let [next-board (set-value-at board coord (first pos-vals))
                solved (solve next-board)]
            (if (empty? solved)
              (recur (rest pos-vals))
              solved)))))))
