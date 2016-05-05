(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
 (not (zero? (get-in board coord))))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (set (map (fn [x] (value-at board [x col])) (range 9)))))

(defn coord-pairs [coords]
  (apply vector (for [row coords
                      col coords]
                  [row col])))

(defn block-values-helper [coord]
  (let [[x y] coord]
    [(* (quot x 3) 3) (* (quot y 3) 3)]))

(defn block-values [board coord]
  (let [[start-r start-c] (block-values-helper coord)]
    (set (for [row (range start-r (+ 3 start-r))
               col (range start-c (+ 3 start-c))]
           (value-at board [row col])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))))

(defn filled-helper [board]
  (set (apply concat (for [row (range 9)]
                       (row-values board [row 0])))))

(defn filled? [board]
  (not (contains? (filled-helper board) 0)))

(defn rows [board]
  (for [row (range 9)]
    (set (row-values board [row 0]))))

(defn subset-valid? [acc subset]
  (and acc (empty? (set/difference all-values subset))))

(defn valid-rows? [board]
  (reduce subset-valid? #{true} (rows board)))

(defn cols [board]
  (for [col (range 9)]
    (set (col-values board [0 col]))))

(defn valid-cols? [board]
  (reduce subset-valid? #{true} (cols board)))

(defn blocks [board]
  (for [x [0 1 2]
        y [0 1 2]
        :let [row (* x 3)
              col (* y 3)]]
    (set (block-values board [row col]))))

(defn valid-blocks? [board]
  (reduce subset-valid? #{true} (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (for [row (range 9)
               col (range 9)
               :while
                 (not (has-value? board [row col]))]
          [row col])))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      [])
    (let [next-empty-point (find-empty-point board)]
      (for [new-value (valid-values-for board next-empty-point)
            solution (solve (set-value-at board next-empty-point new-value))]
        solution))))
