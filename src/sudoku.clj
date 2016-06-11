(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})



(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (contains? all-values (value-at board coord)))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (set(map (fn [x] (get x (second coord))) board )))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
  [x y]))

(defn block-values [board coord]
  (let [x (int (* 3 (Math/floor (/ (first coord) 3))))
        y (int (* 3 (Math/floor (/ (second coord) 3))))
        xs (for [x (range x (+ 3 x))
                 y (range y (+ 3 y))]
                (value-at board [x y]))]
    (set xs)
    )
  )

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
  (set/difference all-values
  (set/union (row-values board coord)
             (col-values board coord)
             (block-values board coord))
             )))

(defn filled? [board]
  (not(contains? (set(flatten board)) 0)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (reduce = true (map (partial = all-values)  (rows board))))

(defn cols [board]
  (map set (apply map vector board)))

(defn valid-cols? [board]
  (reduce = true (map (partial = all-values)  (cols board))))

(defn blocks [board]
  (map (partial block-values board) (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (reduce = true (map (partial = all-values)  (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)
    ))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [x (map(fn[x](.indexOf x 0) ) board)
        y (first (keep-indexed #(when (> %2 -1) %1) (into [] x)))
        ]
    [y (first (filter (fn[z] (< -1 z)) x)) ]
    ))



;(defn solve [board]
;  (if (filled? board)
;    (if(valid-solution? board)
;       board
;       nil)
;     (recur(solve (set-value-at board (find-empty-point board) (first(valid-values-for board (find-empty-point board))))))))
(defn almost-flatten
  [x]
  (filter #(and (sequential? %) (not-any? sequential? %))
    (rest (tree-seq #(and (sequential? %) (some sequential? %)) seq x))))

(defn solve-h [board]
  (if (filled? board)
    (if(valid-solution? board)
       board
       nil)
    (for [emp  (valid-values-for board (find-empty-point board) )]

      (solve-h (set-value-at board (find-empty-point board) emp) ))))


(defn solve [board]
  (filter not-empty(almost-flatten  (solve-h board)))
  )
