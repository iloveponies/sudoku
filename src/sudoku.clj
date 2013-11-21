(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (zero? (value-at board coord)) false true))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (set (for [n (range 0 9)]
      (value-at board [n col])))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-coords [board coord]
  (let [[x _] coord
        [_ y] coord
        x1st (cond
              (<= 0 x 2) 0
              (<= 3 x 5) 3
              :else 6)
        y1st (cond
              (<= 0 y 2) 0
              (<= 3 y 5) 3
              :else 6)
        xrest (range x1st (+ x1st 3))
        yrest (range y1st (+ y1st 3))]
    (for [x1st xrest
          y1st yrest]
      [x1st y1st])))

(defn block-values [board coord]
  (set (map (fn [x] (value-at board x)) (block-coords board coord))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (clojure.set/difference all-values (clojure.set/union
                                       (block-values board coord)
                                       (row-values board coord)
                                       (col-values board coord)))))

(defn filled? [board]
  (let [s (set (apply concat board))]
    (not (contains? s 0))))

(defn rows [board]
  (for [n (range 0 9)]
    (row-values board [n 0])))

(defn valid-rows? [board]
  (let [s (apply concat (rows board))]
    (and
     (not (contains? (set s) 0))
     (apply = (vals (frequencies s))))))

(defn cols [board]
  (for [n (range 0 9)]
    (col-values board [0 n])))

(defn valid-cols? [board]
      (let [s (apply concat (cols board))]
    (and
     (not (contains? (set s) 0))
     (apply = (vals (frequencies s))))))

(defn blocks [board]
  (for [n (coord-pairs [0 3 6])]
    (block-values board n)))

(defn valid-blocks? [board]
      (let [s (apply concat (blocks board))]
    (and
     (not (contains? (set s) 0))
     (apply = (vals (frequencies s))))))

(defn valid-solution? [board]
  (and
   (valid-rows? board)
   (valid-cols? board)
   (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (fn [x] (not (has-value? board x))) (coord-pairs (range 0 9)))))


(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      nil)
    (let [fill-this (find-empty-point board)]
          (for [with-this (valid-values-for board fill-this)
                ihateponies (solve (set-value-at board fill-this with-this))]
            ihateponies))))




