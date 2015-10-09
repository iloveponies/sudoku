(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board [row _]]
  (reduce (fn [result col]
            (conj result (value-at board [row col])))
          #{}
          (range 0 9)))

(defn col-values [board [_ col]]
  (reduce (fn [result row]
            (conj result (value-at board [row col])))
          #{}
          (range 0 9)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board [row col]]
  (let [coord+coord (fn [[x1 y1] [x2 y2]]
                      [(+ x1 x2) (+ y1 y2)])
        top-left [(* 3 (quot row 3))
                  (* 3 (quot col 3))]
        block-coords (map (fn [init-coords]
                            (coord+coord init-coords top-left))
                          (coord-pairs [0 1 2]))]
    (set (map (fn [coord]
                (value-at board coord))
          block-coords))))

(defn valid-values-for [board coord]
  (if (not= (value-at board coord) 0)
    #{}
    (set/difference all-values
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))))

(defn filled? [board]
  (every? (partial has-value? board)
          (coord-pairs (range 0 9))))

(defn rows [board]
  (map #(row-values board [% 0])
       (range 0 9)))

(defn valid-rows? [board]
  (every? (fn [row]
            (== (count row) 9))
          (rows board)))

(defn cols [board]
  (map #(col-values board [0 %])
       (range 0 9)))

(defn valid-cols? [board]
  (every? (fn [col]
            (== (count col) 9))
          (cols board)))

(defn blocks [board]
  (map (fn [[row col]]
         (block-values board [row col]))
       (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (every? (fn [block]
            (== (count block) 9))
          (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [empty-points (filter (partial (complement has-value?) board)
                             (coord-pairs (range 0 9)))]
    (first empty-points)))

(defn solve-helper [board]
  (let [empty-point (find-empty-point board)]
    (if (nil? empty-point)
      (if (valid-solution? board)
        [board]
        [])
      (for [value (valid-values-for board empty-point)
            solution (solve-helper (set-value-at board
                                                 empty-point
                                                 value))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
