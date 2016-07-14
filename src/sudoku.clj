(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord)
  )

(defn has-value? [board coord]
  (if (== 0 (value-at board coord))
    false
    true)
  )

(defn row-values [board coord]
  (set (get-in board [(first coord)]))
  )

(defn col-values [board coord]
  (conj '#{} (get-in board [0 (second coord)])
        (get-in board [1 (second coord)])
        (get-in board [2 (second coord)])
        (get-in board [3 (second coord)])
        (get-in board [4 (second coord)])
        (get-in board [5 (second coord)])
        (get-in board [6 (second coord)])
        (get-in board [7 (second coord)])
        (get-in board [8 (second coord)])
        )
  )

(defn coord-pairs [coords]
  (reduce concat (for [x coords]
    (for [y coords]
      [x y])))
  )

(defn block-values [board coord]
  (let [x (* 3 (int (/ (first coord) 3)))
        y (* 3 (int (/ (second coord) 3)))]
    (conj '#{} (get-in board [(+ 0 x) (+ 0 y)])
               (get-in board [(+ 0 x) (+ 1 y)])
               (get-in board [(+ 0 x) (+ 2 y)])
               (get-in board [(+ 1 x) (+ 0 y)])
               (get-in board [(+ 1 x) (+ 1 y)])
               (get-in board [(+ 1 x) (+ 2 y)])
               (get-in board [(+ 2 x) (+ 0 y)])
               (get-in board [(+ 2 x) (+ 1 y)])
               (get-in board [(+ 2 x) (+ 2 y)]))
  ))

(defn valid-values-for [board coord]
  (if (not (zero? (value-at board coord)))
    '#{}
    (set/difference all-values (block-values board coord) (row-values board coord) (col-values board coord))
    )
  )

(defn filled? [board]
  (not (contains? (set (reduce concat '() (for [coord (coord-pairs [0 1 2 3 4 5 6 7 8])]
                       [(value-at board coord)]))) 0))
  )

(defn rows [board]
  (seq (map set board))
  )

(defn valid-rows? [board]
  (every? (fn [sequ] (and (not (contains? sequ 0)) (== (count sequ) 9))) (rows board))
  )

(defn cols [board]
  (seq (map set (for [col (range 9)] (col-values board [0 col])))
       )
  )

(defn valid-cols? [board]
  (every? (fn [sequ] (and (not (contains? sequ 0)) (== (count sequ) 9))) (cols board))
  )

(defn blocks [board]
  (seq (map set (for [coord [[0 0] [0 3] [0 6] [3 0] [3 3] [3 6] [6 0] [6 3] [6 6]]] (block-values board coord))))
  )

(defn valid-blocks? [board]
  (every? (fn [sequ] (and (not (contains? sequ 0)) (== (count sequ) 9))) (blocks board))
  )

(defn valid-solution? [board]
  (and (valid-cols? board) (valid-rows? board) (valid-blocks? board))
  )

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value)
  )

(defn find-empty-point [board]
  (let [zeros (clojure.set/difference (set (for [x (range 9)
        y (range 9)]
    (if (== (value-at board [x y]) 0)
      [x y]
      [])
  )) '#{[]})]
    (if (not (empty? zeros))
      (first zeros)
      []))
  )

(defn solve [board]
  (cond
    (filled? board) (if (valid-solution? board) board '())
    :else (let [copyboard board
                emptyPoint (find-empty-point board)]
            (for [testiArvo (valid-values-for copyboard emptyPoint)
                  solution (solve (set-value-at copyboard emptyPoint testiArvo))]
              solution
              ))
    )
  )
