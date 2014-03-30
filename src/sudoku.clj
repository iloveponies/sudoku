(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== 0 (value-at board coord))))

(defn row-values [board [row col]]
  (set (get board row)))

(defn col-values [board [row col]]
  (let [col-val (fn [acc curr] (set (cons (get curr col) acc)))]
    (reduce col-val [] board)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-values [board [row col]]
  (let [corner-row (* 3 (int (/ row 3)))
        corner-col (* 3 (int (/ col 3)))
        additions (coord-pairs [0 1 2])
        all-coords-in-block (map (fn [[row-add col-add]] [(+ corner-row row-add) (+ corner-col col-add)]) additions)]

    (reduce (fn [a-set curr] (conj a-set (value-at board curr))) #{} all-coords-in-block)))

(defn valid-values-for [board coord]
  (let [curr-value (value-at board coord)]
    (if (> curr-value 0)
      #{}
      (set/difference
        all-values
        (block-values board coord)
        (row-values board coord)
        (col-values board coord)))))

(defn filled? [board]
  (not (contains? (set (flatten board)) 0)))

(defn rows [board]
  (reduce (fn [prev curr] (conj prev (set curr))) [] board))

(defn valid-rows? [board]
  (reduce (fn [prev curr]
            (and
              prev
              (= 0
                (count
                  (set/difference
                    all-values
                    (set curr))))))
    true
    (rows board)))

(defn cols [board]
  (for [col (range 0 9)]
    (col-values board [0 col])))

(defn valid-cols? [board]
  (reduce (fn [prev curr]
            (and
              prev
              (= 0
                (count
                  (set/difference
                    all-values
                    (set curr))))))
    true
    (cols board)))

(defn blocks [board]
  (for [block (coord-pairs [0 3 6])]
    (block-values board block)))

(defn valid-blocks? [board]
  (reduce (fn [prev curr]
            (and
              prev
              (= 0
                (count
                  (set/difference
                    all-values
                    (set curr))))))
    true
    (blocks board)))

(defn valid-solution? [board]
  (and (and (valid-rows? board) (valid-cols? board)) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [empty-points (filter (fn [coord] (== 0 (value-at board coord))) (coord-pairs (range 0 9)))]
    (if (empty? empty-points) nil (first empty-points))))

(defn santas-little-helper [board]
  (if (valid-solution? board)
   board
   (let [empty-point (find-empty-point board)]
     (if (nil? empty-point) '()
       (for [valid-choice (valid-values-for board empty-point)
             solution (santas-little-helper (set-value-at board empty-point valid-choice))]
         solution)))))

(defn solve [board]
  (santas-little-helper board))


