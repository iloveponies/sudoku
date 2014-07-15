(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (get-in board coord))))

(defn row-values [board coord]
  (let [[rowi _] coord
        {rowv rowi} board]
    (set rowv)))

(defn col-values [board coord]
  (let [[_ coli] coord
        colv (reduce #(conj %1 (nth %2 coli)) [] board)]
    (set colv)))

(defn coord-pairs [coords]
    (vec (for [x coords 
          y coords]
          [x y])))

(defn coord-2pairs [rcoords ccoords]
    (vec (for [x rcoords 
          y ccoords]
          [x y])))

(defn block-values [board coord]
  (let [[rowi coli] coord
         row-start (* (quot rowi 3) 3)
         col-start (* (quot coli 3) 3)
         row-end (+ row-start 3)
         col-end (+ col-start 3)
         row-r (range row-start row-end)
         col-r (range col-start col-end)]
    (set (map (partial value-at board) 
         (coord-2pairs row-r col-r)))))

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    (set [])
    (set/difference all-values 
                    (set/union (block-values board coord) (row-values board coord) (col-values board coord)))))

(defn filled? [board]
  (reduce #(and %1 (empty? %2)) true (map (partial filter zero?) board)))

(defn rows [board]
  (vec (map set board)))

(defn valid-rows? [board]
  (reduce #(and %1 (empty? %2)) true (map #(set/difference all-values %) (rows board))))

(defn cols [board]
  (vec (map #(col-values board [0 %]) (range 9))))

(defn valid-cols? [board]
  (reduce #(and %1 (empty? %2)) true (map #(set/difference all-values %) (cols board))))

(defn blocks [board]
  (vec (for [x (range 0 9 3)
             y (range 0 9 3)]
             (block-values board [x y]))))

(defn valid-blocks? [board]
  (reduce #(and %1 (empty? %2)) true (map #(set/difference all-values %) (blocks board))))

(defn valid-solution? [board]
  (and (valid-blocks? board) (valid-cols? board) (valid-rows? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
 (first (for [x (range 0 9)
        y (range 0 9)
        :when (not (has-value? board [x y]))]
        [x y])))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      board 
      [])
    (let [loc (find-empty-point board)]
      (for [tval (valid-values-for board loc)
            solution (solve-helper (set-value-at board loc tval))]
       solution))))

(defn solve [board]
  (solve-helper board))
