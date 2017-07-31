(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (loop [row  0
        col   (last coord)
        a-set #{}]
    (if (= row 9)
      a-set
      (recur (inc row) col (conj a-set (value-at board [row col]))))))

(defn coord-pairs [coords]
  (for [k coords
        c coords]
    (vector k c)))

(defn my-coord-pairs [coords-1 coords-2]
  (for [k coords-1
        c coords-2]
    (vector k c)))

(defn asked-block [x]
  (cond
    (= x 1) (my-coord-pairs [0 1 2] [0 1 2])  ; upper left corner
    (= x 2) (my-coord-pairs [0 1 2] [3 4 5])  ; upper middle
    (= x 3) (my-coord-pairs [0 1 2] [6 7 8])
    (= x 4) (my-coord-pairs [3 4 5] [0 1 2])  ; rightest block, middle row of blocks
    (= x 5) (my-coord-pairs [3 4 5] [3 4 5])  ; ...
    (= x 6) (my-coord-pairs [3 4 5] [6 7 8])
    (= x 7) (my-coord-pairs [6 7 8] [0 1 2])
    (= x 8) (my-coord-pairs [6 7 8] [3 4 5])
    :else    (my-coord-pairs [6 7 8] [6 7 8])))

(defn block-values [board coord]
  (let [row   (first coord)
        col   (last coord)]
    (set
      (map
        (fn [point] (value-at board point))
        (asked-block
          (cond                               ; which value should be sent to asked-block function
           (and (<= row 2) (<= col 2)) 1
           (and (<= row 2) (<= col 5)) 2
           (and (<= row 2) (<= col 8)) 3
           (and (<= row 5) (<= col 2)) 4
           (and (<= row 5) (<= col 5)) 5
           (and (<= row 5) (<= col 8)) 6
           (and (<= row 8) (<= col 2)) 7
           (and (<= row 8) (<= col 5)) 8
           :else                       9))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (clojure.set/union (row-values board coord) (col-values board coord) (block-values board coord)))))

(defn all-board-values [board]
  (loop [a-set #{}
         ind   0]
    (if (= ind 10)
      a-set
      (recur (clojure.set/union a-set (set (get board ind))) (inc ind)))))

(defn filled? [board]
  (not (contains? (all-board-values board) 0)))

(defn rows [board]
  (loop [ind   0
         a-vec []]
    (if (= ind 9)
      a-vec
      (recur (inc ind) (assoc a-vec (count a-vec) (row-values board [ind 0]))))))

(defn valid-rows? [board]
  (not (contains? (set (map (fn [a-set] (= all-values a-set)) (rows board))) false)))

(defn cols [board]
  (loop [ind   0
         a-vec []]
    (if (= ind 9)
      a-vec
      (recur (inc ind) (assoc a-vec (count a-vec) (col-values board [0 ind]))))))

(defn valid-cols? [board]
  (not (contains? (set (map (fn [a-set] (= all-values a-set)) (cols board))) false)))

(defn blocks [board]
  (loop [a-vec  []
         ind    0
         coords (for [k [0 3 6]
                      c [0 3 6]]
                  (vector k c))]
    (if (= ind 9)
      a-vec
      (recur (assoc a-vec (count a-vec) (block-values board (first coords))) (inc ind) (rest coords)))))

(defn valid-blocks? [board]
  (not (contains? (set (map (fn [a-set] (= all-values a-set)) (blocks board))) false)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [row 0
         col 0]
    (cond
      (= 0 (value-at board [row col])) [row col]
      (= col 8)                  (recur (inc row) 0)
      :else                      (recur row (inc col)))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      '())
    (let [coord     (find-empty-point board)
          remaining (valid-values-for board coord)]
      (for [elem remaining
            solution (solve (set-value-at board coord elem))]
        solution))))
