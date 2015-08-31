(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board coord]
  (let [[row x] coord]
    (loop [col  0
           vals #{}]
      (if (= 9 col)
        vals
        (recur (inc col) (conj vals (value-at board [row col])))))))

(defn col-values [board coord]
  (let [[x col] coord]
    (loop [row  0
           vals #{}]
      (if (= 9 row)
        vals
        (recur (inc row) (conj vals (value-at board [row col])))))))

(defn coord-pairs [coords]
  (apply vector (for [c1 coords
                      c2 coords]
                  (vector c1 c2))))

(defn block-values [board coord]
  (let [[row col] coord
        [cr cc]   [(cond (<= row 2) 0 (<= row 5) 3 :else 6)
                   (cond (<= col 2) 0 (<= col 5) 3 :else 6)]]
    (set (for [onecoord (apply vector (for [rows [cr (+ 1 cr) (+ 2 cr)]
                                            cols [cc (+ 1 cc) (+ 2 cc)]]
                                        (vector rows cols)))]
           (value-at board onecoord)))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference
      all-values
      (set/union (col-values board coord)
                         (row-values board coord)
                         (block-values board coord)))))

(defn filled? [board]
  (not (contains? (set (apply concat (for [row board]
                                       (seq row)))) 0)))

(defn rows [board]
  (apply vector (for [coord [[0 0] [1 0] [2 0] [3 0] [4 0] [5 0] [6 0] [7 0] [8 0]]]
    (row-values board coord))))

(defn valid-rows? [board]
  (not-any? false? (for [row (rows board)]
                     (= all-values row))))

(defn cols [board]
  (apply vector (for [coord [[0 0] [0 1] [0 2] [0 3] [0 4] [0 5] [0 6] [0 7] [0 8]]]
    (col-values board coord))))

(defn valid-cols? [board]
  (not-any? false? (for [col (cols board)]
                     (= all-values col))))

(defn blocks [board]
  (apply vector (for [coord (coord-pairs [0 3 6])]
    (block-values board coord))))

(defn valid-blocks? [board]
  (not-any? false? (for [block (blocks board)]
                     (= all-values block))))

(defn valid-solution? [board]
  (and (true? (valid-rows? board))
       (true? (valid-cols? board))
       (true? (valid-blocks? board))))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [row 0
         col 0]
    (cond
     (= 0 (value-at board [row col]))
       [row col]
     (and (= 9 row) (= 9 col))
       nil
     (= 9 col)
       (recur (inc row) 0)
     :else
       (recur row (inc col)))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      '())
    (let [emptypoint (find-empty-point board)
          validvals  (valid-values-for board emptypoint)]
      (for [sudokunbr validvals
            solution (solve (set-value-at board emptypoint sudokunbr))]
        solution))))
