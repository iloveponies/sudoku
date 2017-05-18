(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (pos? (value-at board coord)))

(defn row-values [board [x y]]
  (set (get board x)))

(defn col-values [board [x y]]
  (loop [s #{}
         row 0
         col y]
    (if (= row 9)
      s
      (recur (conj s (value-at board [row col]))
             (inc row)
             col))))

(defn coord-pairs [coords]
  (if (empty? coords)
    nil
    (for [a coords
          b coords]
      (vector a b))))

(defn block-values [board [x y]]
  (let [corner-value (fn [v] (cond
                               (<= 0 v 2) 0
                               (<= 3 v 5) 3
                               (<= 6 v 8) 6))
        cx (corner-value x)
        cy (corner-value y)
        ebx (+ cx 3)
        eby (+ cy 3)]
    (loop [s #{}
           row cx
           col cy]
      (cond
        (and (= row ebx)) s
        (= col eby) (recur s (inc row) cy)
        :else (recur (conj s (value-at  board[row col])) row (inc col))))))

(defn valid-values-for [board coord]
  (let [block (block-values board coord)
        row (row-values board coord)
        col (col-values board coord)
        un (set/union block row col)]
    (if (> (value-at board coord) 0)
      #{}
      (set/difference all-values un))))

(defn filled? [board]
  (let [helper (fn [bo]
                 (loop [s #{}
                        row 0]
                   (if (= row 9)
                     s
                     (recur (set/union s
                                       (row-values bo [row 0]))
                                       (inc row)))))]
    (not (contains? (helper board) 0))))

(defn rows [board]
  (loop [v (vector)
         row 0]
    (if (= row 9)
      v
      (recur (conj v (row-values board [row 0]))
             (inc row)))))

(defn valid-rows? [board]
  (let [r (rows board)]
    (loop [check true
         row 0]
    (if (or (not check) (= row 9))
      check
      (recur (and (= (count (get r row)) 9)
                  (not (contains? (get r row) 0)))
             (inc row))))))

(defn cols [board]
  (loop [v (vector)
         col 0]
    (if (= col 9)
      v
      (recur (conj v (col-values board [0 col]))
             (inc col)))))

(defn valid-cols? [board]
  (let [r (cols board)]
    (loop [check true
           col 0]
      (if (or (not check) (= col 9))
        check
        (recur (and (= (count (get r col)) 9)
                    (not (contains? (get r col) 0)))
               (inc col))))))

(defn blocks [board]
  (loop [v (vector)
         row 0
         col 0]
    (cond
      (= row 9) v
      (= col 9) (recur v (+ row 3) 0)
      :else (recur (conj v (block-values board [row col])) row (+ col 3)))))

(defn valid-blocks? [board]
  (let [r (blocks board)]
    (loop [check true
           block 0]
      (if (or (not check) (= block 9))
        check
        (recur (and (= (count (get r block)) 9)
                    (not (contains? (get r block) 0)))
               (inc block))))))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [row 0
         col 0]
    (cond
      (and (= col 9) (= row 9)) nil
      (= col 9) (recur (inc row) 0)
      (= (value-at board [row col]) 0) [row col]
      :else (recur row (inc col)))))

(defn solve [board]
  (if (valid-solution? board)
    board
    (let [coord (find-empty-point board)
          options (valid-values-for board coord)]
      (for [num options
            sol (solve (set-value-at board coord num))]
        sol))))


