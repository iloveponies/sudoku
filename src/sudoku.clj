(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (set (reduce
           (fn [init x]
             (cons (get x col) init))
           '()
           board))))

(defn coord-pairs [coords]
  (vec (apply concat (for [coord1 coords]
                       (for [coord2 coords]
                         [coord1 coord2])))))

(defn upperleft-block-coords [coords]
  (let [[row col] coords
        row-mod (mod row 3)
        col-mod (mod col 3)]
    [(- row row-mod) (- col col-mod)]))

(defn block-values [board coord]
  (let [[ul-y ul-x] (upperleft-block-coords coord)
        block-coords (coord-pairs [0 1 2])]
    (set (for [[y x] block-coords]
           (value-at board [(+ ul-y y) (+ ul-x x)])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [block-values (block-values board coord)
          row-values (row-values board coord)
          col-values (col-values board coord)]
      (clojure.set/difference all-values block-values row-values col-values))))

(defn filled? [board]
  (let [all (set(apply concat board))]
    (not (contains? all 0))))

(defn rows [board]
  (let [loop-values (range 0 (count board))]
    (for [row-col loop-values]
      (row-values board [row-col row-col]))))

(defn valid-rows? [board]
  (let [rows (rows board)]
    (every? (fn [x] (= all-values x)) rows)))

(defn cols [board]
  (let [loop-values (range 0 (count board))]
    (for [row-col loop-values]
      (col-values board [row-col row-col]))))

(defn valid-cols? [board]
  (let [cols (cols board)]
    (every? (fn [x] (= all-values x)) cols)))

(defn blocks [board]
  (let [pairs (coord-pairs [0 3 6])]
    (for [coords pairs]
      (block-values board coords))))

(defn valid-blocks? [board]
  (let [blocks (blocks board)]
    (every? (fn [x] (= all-values x)) blocks)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (let [[row col] coord
        row-vec (get board row)
        new-row-vec (assoc row-vec col new-value)]
    (assoc board row new-row-vec)))

(defn find-empty-point [board]
  (let [row-index (loop [row 0
                         board board]
                    (cond
                     (empty? board) nil
                     (some zero? (first board)) row
                     :else (recur (inc row) (rest board))))]
    (if (nil? row-index)
      nil
      (loop [col-index 0
             row-items (get board row-index)]
        (cond
         (empty? row-items) nil
         (zero? (first row-items)) [row-index col-index]
         :else (recur (inc col-index) (rest row-items)))))))

(defn solve-helper [board]
   (if (filled? board)
           (if (valid-solution? board)
             [board]
             '())
           (let [empty-loc (find-empty-point board)
                 valid-values (valid-values-for board empty-loc)]
             (for [valid-value valid-values
                   solution (solve-helper (set-value-at board empty-loc valid-value))]
               solution))))

(defn solve [board]
  (first (solve-helper board)))
