(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[r c] coord
         row (get board r)]
    (into #{} row)
    ))

(defn col-values [board coord]
  (let [[r c] coord
        col (map #(get %1 c) board)]
    (into #{} col)
    ))

(defn coord-pairs [coords]
  (for [x coords y coords]
    [x y]))

(defn block-values [board coord]
  (let [[r c] coord
        top-r (* 3(quot r 3))
        left-c (* 3 (quot c 3))]
    (into #{} (for [x (range left-c (+ left-c 3))
          y (range top-r (+ top-r 3))]
      (value-at board [y x])
      ))
    ))

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn valid-values-for [board coord]
  (cond
    (has-value? board coord) #{}
    :else (into #{} (filter (fn [x] (and
                      (not (contains? (row-values board coord) x))
                      (not (contains? (col-values board coord) x))
                      (not (contains? (block-values board coord) x))))
                      all-values))
    ))

(defn filled? [board]
  (empty? (filter (fn [v] (false? v)) (for [x (range 0 9) y (range 0 9)]
    (has-value? board [y x]))))
  )

(defn rows [board]
  (for [r (range 0 9)]
    (row-values board [r 0])
    ))

(defn valid-rows? [board]
  (reduce (fn [x,y] (and x y)) true (map (fn [x] (= 9 (count x))) (rows board))))

(defn cols [board]
  (for [c (range 0 9)]
    (col-values board [0 c])
    ))

(defn valid-cols? [board]
  (reduce (fn [x,y] (and x y)) true (map (fn [x] (= 9 (count x))) (cols board))))

(defn blocks [board]
  (for [r [0 3 6] c [0 3 6]]
    (block-values board [r c])))

(defn valid-blocks? [board]
  (reduce (fn [x,y] (and x y)) true (map (fn [x] (= 9 (count x))) (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [all-coord-values (for [r (range 0 9) c (range 0 9)] [r c])
        empty-value-coords (filter
                             (fn [coord]
                               (zero? (value-at board coord))
                                 )
                             all-coord-values)]
    (first empty-value-coords)
    ))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      nil)
    (let [ next (find-empty-point board)
          possible-values (valid-values-for board next)
          recursive-solution (for [new-val possible-values] (solve (set-value-at board next new-val)))
          filtered-empty-solutions (filter (fn [s] (not (nil? s))) recursive-solution)
          ]
      (first filtered-empty-solutions)
      )
    ))

