(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== (value-at board coord) 0)))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (into #{} (map (fn [row]
                     (get row col))
                   board))))

(defn coord-pairs [coords]
  (into []
        (for [row coords
              col coords]
          [row col])))

(defn block-values [board coord]
  (let [top-left-coord (fn [[x y]]
                         [(* 3 (int (/ x 3))) (* 3 (int (/ y 3)))])
        [x0 y0] (top-left-coord coord)]
    (into #{} (for [[dx dy] (coord-pairs [0 1 2])]
       (value-at board [(+ x0 dx) (+ y0 dy)])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))))

(defn filled? [board]
  (not (contains? (reduce (fn [acc s]
                            (set/union acc s))
                          #{}
                          (map (fn [v]
                                 (into #{} v))
                               board))
                  0)))

(defn rows [board]
  (for [row (range 9)]
    (row-values board [row 0])))

(defn valid-rows? [board]
  (every? (fn [s]
            (= s all-values))
          (rows board)))

(defn cols [board]
  (for [col (range 9)]
    (col-values board [0 col])))

(defn valid-cols? [board]
  (every? (fn [s]
            (= s all-values))
          (cols board)))

(defn blocks [board]
  (for [row (range 0 9 3)
        col (range 0 9 3)]
    (block-values board [row col])))

(defn valid-blocks? [board]
  (every? (fn [s]
            (= s all-values))
          (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [chopped-list (drop-while (fn [[r c v]]
                                   (not (zero? v)))
                                 (for [row (range 9)
                                       col (range 9)]
                                   [row col (value-at board [row col])]))]
    (if (empty? chopped-list)
      nil
      [(first (first chopped-list)) (second (first chopped-list))])))

(defn solve-helper [board solutions]
  (if (filled? board)
    [board]
    (let [coord (find-empty-point board)]
      (for [v (valid-values-for board coord)
            solution (solve-helper (set-value-at board coord v) solutions)]
        solution))))

(defn solve [board]
  (first (solve-helper board [])))
