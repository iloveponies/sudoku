(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row column]]
  (set (board row)))

(defn col-values [board [row column]]
  (set (map #(value-at board [% column]) (range (count board)))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn top-left-of-block [[row column]]
  [(* 3 (quot row 3))
   (* 3 (quot column 3))])

(defn block-coords [coord]
  (let [[row column] (top-left-of-block coord)]
    (for [x (range 3)
          y (range 3)]
      [(+ row x) (+ column y)])))

(defn block-values [board coord]
  (set (map (partial value-at board) (block-coords coord))))

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
      (set/difference
       all-values
       (apply set/union
              ((partial (juxt row-values col-values block-values)
                        board)
               coord)))))

(defn filled? [board]
  (not (some (partial some zero?) board)))

(defn rows [board]
  (map set board))

(defn all-valid [sets]
  (every? (partial = all-values) sets))

(defn valid-rows? [board]
  (all-valid (rows board)))

(defn cols [board]
  (if (every? empty? board) '()
      (cons (set (map first board))
            (cols (map rest board)))))

(defn valid-cols? [board]
  (all-valid (cols board)))

(def top-lefts
  (for [i (range 3)
        j (range 3)]
    [(* 3 i) (* 3 j)]))

(defn blocks [board]
  (map (partial block-values board) top-lefts))

(defn valid-blocks? [board]
  (all-valid (blocks board)))

(defn valid-solution? [board]
  (every? identity ((juxt valid-rows? valid-cols? valid-blocks?) board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (for [x (range 9)
               y (range 9)
               :when (not (has-value? board [x y]))]
           [x y])))

(defn solutions [board]
  (cond (valid-solution? board) [board]
        (filled? board) []
        :else
        (let [point (find-empty-point board)]
          (mapcat (comp solutions (partial set-value-at board point))
                  (valid-values-for board point)))))

(defn solve [board]
  (first (solutions board)))
