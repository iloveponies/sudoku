(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (set (map #(get % col) board)))

(defn coord-pairs
  ([coords]
   (for [row coords
         col coords]
     (vector row col)))
  ([coords1 coords2]
   (for [row coords1
         col coords2]
     (vector row col))))

(defn block-values [board [row col]]
  (let [tl-row (- row (mod row 3))
        tl-col (- col (mod col 3))
        block (coord-pairs
               (range tl-row (+ 3 tl-row))
               (range tl-col (+ 3 tl-col)))]
    (set (map #(value-at board %) block))))

(defn valid-values-for [board coord]
  (let [row-vals (row-values board coord)
        col-vals (col-values board coord)
        block-vals (block-values board coord)]
    (if (has-value? board coord)
      #{}
      (set/difference all-values row-vals col-vals block-vals))))

(defn filled? [board]
  (let [board-vals (reduce (fn [values value]
                             (set/union values (set value))) #{} board)]
    (not (contains? board-vals 0))))

(defn rows [board]
  (map set board))

(defn- check-validity [board-sets]
  (every? #(= all-values %) board-sets))

(defn valid-rows? [board]
  (check-validity (rows board)))

(defn cols [board]
  (map #(col-values board [0 %]) (range 0 9)))

(defn valid-cols? [board]
  (check-validity (cols board)))

(defn blocks [board]
  (let [block-corners (coord-pairs (range 0 9 3))]
    (map #(block-values board %) block-corners)))

(defn valid-blocks? [board]
  (check-validity (blocks board)))

(defn valid-solution? [board]
  (every? identity ((juxt valid-rows? valid-cols? valid-blocks?) board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn- find-empty-points [board]
  (reduce (fn [points p]
            (if (zero? (value-at board p))
              (conj points p)
              points)) [] (coord-pairs (range 0 9))))

(defn find-empty-point [board]
  (first (find-empty-points board)))

(defn- solve- [board]
  (if (filled? board)
    (if (valid-solution? board) [board] [])
    (let [first-empty (find-empty-point board)
          valid-values (valid-values-for board first-empty)]
      (for [value valid-values
            solution (solve- (set-value-at board first-empty value))]
        solution))))

(defn solve [board]
  (first (solve- board)))
