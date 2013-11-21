(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))


(defn has-value? [board coord]
  (cond
   (= 0 (value-at board coord)) false
   :else                        true))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (set (for [row [0 1 2 3 4 5 6 7 8]]
           (value-at board [row col])))))

(defn coord-pairs [coords]
  (for [r coords
        c coords]
    [r c]))

(defn block-origo [r c]
   (let [trnsfrm (fn [x]
                   (cond
                    (<= 6 x) 6
                    (<= 3 x) 3
                    :else  0))]
     [(trnsfrm r) (trnsfrm c)]))

(defn block-coords[coord]
  (let [[r c] coord
        [r-origo c-origo] (block-origo r c)
        r-range (range r-origo (+ 3 r-origo))
        c-range (range c-origo (+ 3 c-origo))]
    (for [row r-range
          col c-range]
      [row col])))

(defn block-values [board coord]
  (set (map (fn [c] (value-at board c)) (block-coords coord))))

(defn valid-values-for [board coord]
  (cond
   (= 0 (value-at board coord)) (clojure.set/difference
                                 all-values
                                 (block-values board coord)
                                 (row-values board coord)
                                 (col-values board coord))
   :else                        '#{}))

(defn filled? [board]
  (empty? (filter zero? (apply concat board))))

(defn rows [board]
  (for [row [0 1 2 3 4 5 6 7 8]]
    (row-values board [row 0])))

(defn cols [board]
  (for [col [0 1 2 3 4 5 6 7 8]]
    (col-values board [0 col])))

(defn blocks [board]
  (for [n [[0 0] [0 3] [0 6] [3 0] [3 3] [3 6] [6 0] [6 3] [6 6]]]
    (block-values board n)))

(defn valid-filled? [section]
  (and
   (not (contains? section 0))
   (empty? (clojure.set/difference all-values section))))

(defn valid-rows? [board]
  (every? valid-filled? (rows board)))

(defn valid-cols? [board]
  (every? valid-filled? (cols board)))

(defn valid-blocks? [board]
  (every? valid-filled? (blocks board)))

(defn valid-solution? [board]
  (and
   (valid-rows? board)
   (valid-cols? board)
   (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (complement nil?) (for [x [0 1 2 3 4 5 6 7 8]
        y [0 1 2 3 4 5 6 7 8]]
    (if (= 0 (value-at board [x y])) [x y])))))

(defn solve [board]
  (filter (complement empty?) (if (nil? (find-empty-point board))
    (if (valid-solution? board)
      board)
    (for [n (valid-values-for board (find-empty-point board))
          solution (solve (set-value-at board (find-empty-point board) n))]
      solution))))





