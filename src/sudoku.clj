(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board [f _]]
  (set (get board f)))

(defn col-values [board [_ c]]
  (set (map (fn [row] (get row c)) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board [row col]]
  (let [top-left-coords [(* 3 (quot row 3)) (* 3 (quot col 3))]
        tl-row-point (first top-left-coords)
        tl-col-point (last top-left-coords)]
    (set (map (fn [c] (value-at board c)) (for [rr (range tl-row-point (+ tl-row-point 3))
                                                cc (range tl-col-point (+ tl-col-point 3))]
                                            [rr cc])))))

(defn valid-values-for [board coord]
  (let [all-values #{1 2 3 4 5 6 7 8 9}]
    (if (not (= 0 (value-at board coord)))
      #{}
      (clojure.set/difference all-values (clojure.set/union (row-values board coord) (col-values board coord) (block-values board coord))))))


(defn filled? [board]
  (empty? (filter (fn [c] (= 0 c)) (flatten board))))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (empty? (filter (fn [row] (not (= row #{1 2 3 4 5 6 7 8 9}))) (rows board))))

(defn cols [board]
  (for [c (range 0 (count (get board 0)))]
    (col-values board [0 c])))

(defn valid-cols? [board]
  (empty? (filter (fn [col] (not (= col #{1 2 3 4 5 6 7 8 9}))) (cols board))))

(defn blocks [board]
  (map (fn [point] (block-values board point)) (for [row (range 0 9 3)
                                                    col (range 0 9 3)]
                                                [row col])))

(defn valid-blocks? [board]
  (empty? (filter (fn [block] (not (= block #{1 2 3 4 5 6 7 8 9}))) (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (fn [p] (not (has-value? board p))) (for [x (range 0 9)
                                                           y (range 0 9)]
                                                       [x y]))))

(defn solve-helper [board]
  (if (valid-solution? board)
    [board]
    (let [c-empty-point (find-empty-point board)
          valid-vals (valid-values-for board c-empty-point)]
      (for [val valid-vals
            result (solve-helper (set-value-at board c-empty-point val))]
        result))))

(defn solve [board]
  (first (solve-helper board)))
