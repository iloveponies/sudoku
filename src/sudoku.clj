(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[x y] coord]
    (set (get board x))))

(defn col-values [board coord]
  (let [[x y] coord]
    (set (map (fn [row] (get row y)) board))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board coord]
  (let [[x y] coord
        x0 (* (int (/ x 3)) 3)
        y0 (* (int (/ y 3)) 3)]
    (set (for [row (apply vector (range x0 (+ x0 3)))
               col (apply vector (range y0 (+ y0 3)))]
           (get-in board [row col])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (set/union (row-values board coord)
                               (col-values board coord)
                               (block-values board coord)))))

(defn filled? [board]
  (not (contains? (apply set/union (for [row (apply vector (range 0 9))]
                                     (row-values board [row 0])))
                  0)))

(defn rows [board]
  (for [row (apply vector (range 0 9))]
    (row-values board [row 0])))

(defn valid-rows? [board]
  (every? (fn [x] (= all-values x)) (rows board)))

(defn cols [board]
  (for [col (apply vector (range 0 9))]
    (col-values board [0 col])))

(defn valid-cols? [board]
  (every? (fn [x] (= all-values x)) (cols board)))

(defn blocks [board]
  (for [row [0 3 6]
        col [0 3 6]]
    (block-values board [row col])))

(defn valid-blocks? [board]
  (every? (fn [x] (= all-values x)) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (remove empty?
                 (for [x (apply vector (range 0 9))
                       y (apply vector (range 0 9))]
                   (if (has-value? board [x y])
                     []
                     [x y])))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      [])
    (let [coord (find-empty-point board)]
      (first (remove empty? (for [new-values (valid-values-for board coord)]
                              (solve (set-value-at
                                      board
                                      coord
                                      new-values))))))))

