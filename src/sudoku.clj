(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (set (map (fn [x] (value-at board [(first coord) x])) (range 0 9))))

(defn col-values [board coord]
  (set (map (fn [x] (value-at board [x (second coord)])) (range 0 9))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board coord]
  (let [block-thirds (fn [x] (cond
                               (<= 0 x 2) [0 1 2]
                               (<= 3 x 5) [3 4 5]
                               (<= 6 x 8) [6 7 8]
                               :else nil))]
    (set (map (fn [x] (value-at board x))
              (for [row (block-thirds (first coord))
                    col (block-thirds (second coord))]
                [row col])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference #{1 2 3 4 5 6 7 8 9}
                    (set/union
                      (row-values board coord)
                      (col-values board coord)
                      (block-values board coord)))))

(defn filled? [board]
  (not (contains? (set (flatten board)) 0)))

(defn valid-set? [a-set]
  (= a-set #{1 2 3 4 5 6 7 8 9}))

(defn vector-and [a-vect]
  (cond
    (empty? a-vect) true
    (first a-vect) (recur (rest a-vect))
    :else false))

(defn rows [board]
  (map (fn [x] (row-values board [x 0])) (range 0 9)))

(defn valid-rows? [board]
  (vector-and (map valid-set? (rows board))))

(defn cols [board]
  (map (fn [x] (col-values board [0 x])) (range 0 9)))

(defn valid-cols? [board]
 (vector-and (map valid-set? (cols board))))

(defn blocks [board]
  (map (fn [x] (block-values board x)) (for [rows [1 4 7]
                                             cols [1 4 7]]
                                         [rows cols])))

(defn valid-blocks? [board]
  (vector-and (map valid-set? (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [helper (fn [coords] (cond
                              (empty? coords) nil
                              (= (value-at board (first coords)) 0) (first coords)
                              :else (recur (rest coords))))]
    (helper (coord-pairs [0 1 2 3 4 5 6 7 8]))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      nil)
    (apply concat (for [emp [(find-empty-point board)]
                        new-val (valid-values-for board emp)]
                    (solve (set-value-at board emp new-val))))))



