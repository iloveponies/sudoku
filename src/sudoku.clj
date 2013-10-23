(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values (set (range 1 10)))
(def all-coords (set (range 0 9)))
(def all-block-coords (set (range 0 9 3)))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (get-in board coord))))

(defn row-values [board coord]
  (let [row (first coord)]
    (set (map (fn [col] (value-at board [row col])) all-coords))))

(defn col-values [board coord]
  (let [col (second coord)]
    (set (map (fn [row] (value-at board [row col])) all-coords))))

(defn coord-pairs [coords]
  (for [row coords col coords] [row col]))

(defn block-values [board coord]
  (let [block-coords (fn [coord] [(* 3 (int (/ coord 3)))
                                  (+ 3 (* 3 (int (/ coord 3))))])]
    (set (for [row (apply range (block-coords (first coord)))
          col (apply range (block-coords (second coord)))]
      (value-at board [row col])))))

(defn valid-values-for [board coord]
  (if (zero? (value-at board coord))
    (set/difference all-values
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))
    #{}))

(defn filled? [board]
  (not (some zero? (for [row all-coords col all-coords] (value-at board [row col])))))

(defn rows [board]
  (for [row all-coords] (row-values board [row 0])))

(defn valid-rows? [board]
  (every? (fn [x] (= x all-values)) (rows board)))

(defn cols [board]
  (for [col all-coords] (col-values board [0 col])))

(defn valid-cols? [board]
  (every? (fn [x] (= x all-values)) (cols board)))

(defn blocks [board]
  (for [row all-block-coords col all-block-coords]
    (block-values board [row col])))

(defn valid-blocks? [board]
  (every? (fn [x] (= x all-values)) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [f (fn [ac x] (if (zero? (value-at board x)) x ac))
        my-all-coords (for [row all-coords col all-coords] [row col])]
  (reduce f nil my-all-coords)))

(defn find-empty-points [board]
  (let [f (fn [ac x] (if (zero? (value-at board x)) (cons x ac) ac))
        my-all-coords (for [row all-coords col all-coords] [row col])]
  (reduce f () my-all-coords)))

(defn possible-boards-after-one-move [board]
  (let [point (find-empty-point board)]
    (for [v (valid-values-for board point)]
      (set-value-at board point v))))

(defn solve-helper [board]
  (if (valid-solution? board)
    (list board)
    (for [new-board (possible-boards-after-one-move board)
          solutions (solve-helper new-board)]
      solutions)))

(defn solve [board]
  (first (solve-helper board)))

