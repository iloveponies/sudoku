(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= (value-at board coord) 0)))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord
        helper (fn [a-seq row]
                 (conj a-seq (get row col)))]
    (reduce helper #{} board)))

(defn coord-pairs
  ([coords] (for [row coords
                  col coords]
              [row col]))
  ([rows cols] (for [row rows
                     col cols]
                 [row col])))

(defn block-values [board coords]
  (let [top-left-point (fn [coords]
                         (let [[x y] coords]
                           (vector (* (int (/ x 3)) 3)
                                   (* (int (/ y 3)) 3))))
        block-ranges (fn [top-left]
                       (vector (range (first top-left) (+ (first top-left) 3))
                               (range (second top-left) (+ (second top-left) 3))))]

    (set (for [point (coord-pairs (first (block-ranges (top-left-point coords)))
                                  (second (block-ranges (top-left-point coords))))]
           (value-at board point)))))


(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))))

(defn filled? [board]
  (not (contains? (set (apply concat board)) 0)))

(defn rows [board]
  (for [row board]
    (set row)))

(defn valid-rows? [board]
  (every? identity (for [row (rows board)]
                          (= row all-values))))

(defn cols [board]
    (for [col (range 9)]
    (col-values board [0 col])))

(defn valid-cols? [board]
  (every? identity (for [col (cols board)]
                     (= col all-values))))

(defn blocks [board]
  (for [point (coord-pairs [0 3 6])]
    (block-values board point)))

(defn valid-blocks? [board]
  (every? identity (for [block (blocks board)]
                     (= block all-values))))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [idx (.indexOf (apply concat board) 0)]
    [(int (/ idx 9)) (mod idx 9)]))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board])
    (let [empty-point (find-empty-point board)]
      (for [value (valid-values-for board empty-point)
            next-step (solve-helper (set-value-at board empty-point value))]
        (if (not (= next-step '()))
          next-step)))))

(defn solve [board]
  (first (solve-helper board)))
