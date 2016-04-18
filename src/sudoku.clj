(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (set (for [row (range (count board))]
         (get-in board [row col]))))

(defn coord-pairs [coords]
  (vec (for [i coords
             j coords]
         (vector i j))))

(defn block-values [board coord]
  (let [[row col] coord
        row-tl (* 3 (quot row 3))
        col-tl (* 3 (quot col 3))]
    (set (for [i (range row-tl (+ row-tl 3))
               j (range col-tl (+ col-tl 3))]
           (value-at board (vector i j))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))))

(defn filled? [board]
  (every? #(has-value? board %1)
          (coord-pairs (range (count board)))))

(defn rows [board]
  (for [row (range (count board))]
    (row-values board (vector row 0))))

(defn valid-rows? [board]
  (every? #(= %1 all-values) (rows board)))

(defn cols [board]
  (for [col (range (count board))]
    (col-values board (vector 0 col))))

(defn valid-cols? [board]
  (every? #(= %1 all-values) (cols board)))

(defn blocks [board]
  (vec (map #(block-values board %1) (coord-pairs [1 4 7]))))

(defn valid-blocks? [board]
  (every? #(= %1 all-values) (blocks board)))

(defn valid-solution? [board]
  (and (valid-cols? board)
       (valid-rows? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [coords (coord-pairs (range (count board)))]
    (if (empty? coords)
      [-1 -1]
      (let [coord-pair (first coords)
            value (value-at board coord-pair)]
        (if (zero? value)
          coord-pair
          (recur (rest coords)))))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      [])
    (let [empty-loc (find-empty-point board)]
      (for [value (valid-values-for board empty-loc)
            solution (solve (set-value-at board empty-loc value))]
        solution))))

