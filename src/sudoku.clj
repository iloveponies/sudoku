(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coordinates]
  (get-in board coordinates))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row _]]
  (reduce (fn [x y] (conj x y)) #{} (get board row)))

(defn col-values [board [_ col]]
  (reduce (fn [x y] (conj x y)) #{} (map (fn [x] (get x col)) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board [row col]]
  (let [tr (* (int (/ row 3)) 3)
        lc (* (int (/ col 3)) 3)
        indexes (for [r (range tr (+ tr 3)) c (range lc (+ lc 3))] [r c])]
    (reduce (fn [x y] (conj x y)) 
            #{} 
            (map (fn [x] (get-in board x)) indexes))))


(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference 
      all-values
      (set/union 
        (row-values board coord)
        (col-values board coord)
        (block-values board coord)))))

(defn filled? [board]
  (let [coords (coord-pairs (range 9))]
    (every? (fn [x] (has-value? board x)) coords)))

(defn all-full? [parts]
  (every? (fn [x] (empty? (set/difference all-values x))) parts))

(defn rows [board]
  (map (fn [x] (row-values board [x 0])) (range 9)))

(defn valid-rows? [board]
  (all-full? (rows board)))

(defn cols [board]
  (map (fn [x] (col-values board[0 x])) (range 9)))

(defn valid-cols? [board]
  (all-full? (cols board)))

(defn blocks [board]
  (map (fn [x] (block-values board x))
       [[0 0] [0 3] [0 6] [3 0] [3 3] [3 6] [6 0] [6 3] [6 6]]))

(defn valid-blocks? [board]
  (all-full? (blocks board)))

(defn valid-solution? [board]
  (and (valid-blocks? board)
       (valid-cols? board)
       (valid-rows? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [coords (coord-pairs (range 9))]
    (first (filter (fn [x] (not (has-value? board x))) coords))))

(defn solve [board]
  (let [first-empty (find-empty-point board)]
    (if (nil? first-empty)
      (if (valid-solution? board)
        board
        false)
      (let [result
            (first 
              (filter boolean
                      (map (fn [x] (solve (set-value-at board first-empty x)))
                           (valid-values-for board first-empty))))]
        (if (nil? result)
          false
          result)))))

