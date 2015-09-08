(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== (value-at board coord) 0)))

(defn row-values [board coord]
  (set (get board (get coord 0))))

(defn col-values [board coord]
  (let [[row col] coord
        get-val   (fn [r] (get r col))]
    (set (map get-val board))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    (vector row col)))

(defn block-coords [coord]
  (let [[x y]   coord
        x-off   (* (int (/ x 3)) 3)
        y-off   (* (int (/ y 3)) 3)
        add-off (fn [co]
                  (map + co [x-off y-off]))]
    (map add-off (coord-pairs [0 1 2]))))

(defn block-values [board coord]
  (set (map (fn [co] (value-at board co))
            (block-coords coord))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (clojure.set/difference #{1 2 3 4 5 6 7 8 9}
                            (col-values board coord)
                            (row-values board coord)
                            (block-values board coord))))

(defn filled? [board]
  (not (some (fn [row] (contains? (set row) 0))
             board)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? (fn [row] (= all-values row))
          (rows board)))

(defn cols [board]
  (apply map (comp set vector) board))

(defn valid-cols? [board]
  (every? (fn [col] (= all-values col))
          (cols board)))

(defn blocks [board]
  (let [fun (fn [co]
              (set (block-values board co)))]
    (map fun (coord-pairs [0 3 6]))))

(defn valid-blocks? [board]
  (every? (fn [blk] (= all-values blk))
          (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn index-of-empty [row]
  (loop [col 0]
    (cond
      (or (empty? row)
          (== (count row) col)) nil
      (== (row col) 0)          col
      :else                     (recur (inc col)))))

(defn find-empty-point [board]
  (loop [row 0]
    (let [col (index-of-empty (board row))]
      (cond
        (== (count board) row) nil  ; needed?
        col                    [row col]
        :else                  (recur (inc row))))))

(defn solve [board]
  (if (valid-solution? board)
    board
    (let [coord (find-empty-point board)]
      (for [candidate (valid-values-for board coord)
            solution  (solve (set-value-at board coord candidate))]
        solution))))

