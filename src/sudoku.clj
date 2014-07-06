(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (set (map (fn [x] (get x col)) board))))

(defn coord-pairs-generic [row-coords col-coords]
  (for [row row-coords
        col col-coords]
    [row col]))

(defn coord-pairs
  ([coords] (coord-pairs-generic coords coords))
  ([row-coords col-coords] (coord-pairs-generic row-coords col-coords)))

(defn top-left-corner [coords]
  (let [[r c] coords
        tl-row (* (quot r 3) 3)
        tl-col (* (quot c 3) 3)]
    [tl-row tl-col]))

(defn block-values [board coord]
  (let [[tl-row tl-col] (top-left-corner coord)
        block-coords (coord-pairs
                      (range tl-row (+ tl-row 3))
                      (range tl-col (+ tl-col 3)))]
    (set (map (fn [c] (value-at board c)) block-coords))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [square-value (value-at board coord)
          row-and-col-values (set/union
                              (row-values board coord)
                              (col-values board coord))
          taken-values (set/union
                        row-and-col-values
                        (block-values board coord))]
      (set/difference all-values taken-values))))

(defn filled? [board]
  (let [all-numbers (apply set/union (map set board))]
    (not (contains? all-numbers 0))))

(defn rows [board]
  (map set board))

(defn valid-generic [a-list-of-sets]
  (reduce (fn [acc x] (and acc x)) true
          (map (fn [x] (= x all-values)) a-list-of-sets)))

(defn valid-rows? [board]
  (valid-generic (rows board)))

(defn cols [board]
  (for [col (range 9)]
    (set (col-values board [0 col]))))

(defn valid-cols? [board]
  (valid-generic (cols board)))

(defn blocks [board]
  (for [bx (range 3)
        by (range 3)]
    (set (block-values board [(* bx 3) (* by 3)]))))

(defn valid-blocks? [board]
  (valid-generic (blocks board)))

(defn valid-solution? [board]
  (and
   (valid-rows? board)
   (valid-cols? board)
   (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [coords (coord-pairs (range 9) (range 9))]
    (let [coord (first coords)]
      (cond
       (empty? coords) nil
       (not (has-value? board coord)) coord
       :else (recur (rest coords))))))

(defn solve-helper [cur-board]
  (let [next-empty (find-empty-point cur-board)]
    (if (nil? next-empty)
      (if (valid-solution? cur-board)
        [cur-board]
        nil)
      (let [remaining (valid-values-for cur-board next-empty)]
        (for [value remaining
              solution (solve-helper
                        (set-value-at cur-board next-empty value))]
          solution)))))

(defn solve [board]
  (first (solve-helper board)))
