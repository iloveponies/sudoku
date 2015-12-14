(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord)
)

(defn has-value? [board coord]
  (if (= 0 (value-at board coord)) false true)
)

(defn row-values [board coord]
  (let [[row col] coord]
    (set (get board row)))
)

(defn col-values [board coord]
  (let [[row col] coord
        cnt (range 0 (count board))
        get-cols (fn [seq i]
                   (conj seq (get-in board [i col])))]
    (reduce get-cols #{} cnt))
)

(defn coord-pairs [coords]
  (let [[row col] coords]
    (for [x coords
          y coords]
      [x y]))
)

(defn get-block [coord]
  (let [[row col] coord
        low-row (* 3 (quot row 3))
        low-col (* 3 (quot col 3))
        high-row (+ low-row 2)
        high-col (+ low-col 2)]
    [[low-row low-col] [high-row high-col]]))

; dostanu souradnici z nejakyho bloku, chci vsechny cisla v bloku
(defn block-values [board coord]
  (let [[[low-row low-col] [high-row high-col]] (get-block coord)
        res (for [i (range low-row (+ 1 high-row))
                  j (range low-col (+ 1 high-col))]
              (value-at board [i j]))]
    (set res)
))

(defn valid-values-for [board coord]
  (let [already-there (clojure.set/union
                       (block-values board coord)
                       (row-values board coord)
                       (col-values board coord))
        options (set (range 0 10))
        can-assign (has-value? board coord)]
    (if (not can-assign)
      (clojure.set/difference options already-there)
      #{})
    )
)
(defn filled? [board]
  (every? false? (map (fn [x] (contains? x 0)) (map set board)))
)

(defn rows [board]
  (for [i (range 0 9)]
    (row-values board [i i]))
)

(defn valid-rows? [board]
  (every? (fn [x] (= 9 (count x))) (rows board))
;;   (for [x (rows board)]
;;     (count x))
;;   (count (first (rows board)))
)

(defn cols [board]
(for [i (range 0 9)]
    (col-values board [i i]))
  )

(defn valid-cols? [board]
  (every? (fn [x] (= 9 (count x))) (cols board))
;;   (for [x (rows board)]
;;     (count x))
)

(defn blocks [board]
  (for [row (range 0 9 3)
        col (range 0 9 3)]
    (block-values board [row col]))
  )

(defn valid-blocks? [board]
  (every? (fn [x] (= 9 (count x))) (blocks board))
  )

(defn valid-solution? [board]
  (if (and (valid-blocks? board) (valid-cols? board) (valid-rows? board)) true false)
)

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (fn [x] (not (has-value? board x)))
          (for [x (range 0 9) y (range 0 9)]
            (vector x y))
           ))
)

(defn solve [board]
  (let [iter-empty (find-empty-point board)]
    (if (empty? iter-empty) ; equal to nil
      ; we check solution..
      (if (valid-solution? board) board [])
      (let [possibilities (valid-values-for board iter-empty)]
        (for [i possibilities
              result (solve (set-value-at board iter-empty i))]
          result))))
)
