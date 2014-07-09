(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0
        (value-at board coord)))

(defn row-values [board [row column]]
  (set (get board row)))

(defn col-values [board [row column]]
  (set (map (fn [mapped-row]
              (value-at board [mapped-row column]))
            (range 9))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn top-left [[row col]]
  [(* (quot row 3)
      3)
   (* (quot col 3)
      3)])

(defn block-values [board coord]
  (let [[top-row top-col] (top-left coord)]
    (set (for [row (range top-row
                          (+ top-row 3))
               col (range top-col
                          (+ top-col 3))]
           (value-at board [row col])))))

(defn valid-values-for [board coord]
  (if (not= (value-at board coord)
            0)
    #{}
    (set/difference #{1 2 3 4 5 6 7 8 9}
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))))

(defn all-cells []
  (coord-pairs [0 1 2 3 4 5 6 7 8]))

(defn filled? [board]
  (every? (fn [coord]
            (has-value? board coord))
          (all-cells)))

(defn rows [board]
  (map (fn [row] (row-values board [row 0]))
       (range 9)))

(defn valid-rows? [board]
  (every? (fn [row]
            (= row
               #{1 2 3 4 5 6 7 8 9}))
          (rows board)))

(defn cols [board]
  (map (fn [column] (col-values board [0 column]))
       (range 9)))

(defn valid-cols? [board]
  (every? (fn [col]
            (= col
               #{1 2 3 4 5 6 7 8 9}))
          (cols board)))

(defn blocks [board]
  (map (fn [[block-x block-y]]
         (block-values board
                       [block-x block-y]))
       (for [x (range 3)
             y (range 3)]
         [(* x 3)
          (* y 3)])))

(defn valid-blocks? [board]
  (every? (fn [block]
            (= block
               #{1 2 3 4 5 6 7 8 9}))
          (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board
            coord
            new-value))

(defn find-empty-point [board]
  (some (fn [coord]
          (when (= 0
                   (value-at board
                             coord))
            coord))
        (all-cells)))

(defn solve [board]
  (if (valid-solution? board)
    board
    (let [empty-point (find-empty-point board)
          valid-values (valid-values-for board empty-point)]
      (first (filter identity (map (fn [value]
                                     (solve (set-value-at board
                                                          empty-point
                                                          value)))
                                   valid-values))))))
