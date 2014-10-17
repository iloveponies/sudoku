(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))


(defn row-values [board coord]
  (into #{} (map (fn [x] (value-at board [(first coord) x]))  (range 0 9)  ))
  )

(defn col-values [board coord]
  (into #{} (map (fn [x] (value-at board [x (second coord)]))  (range 0 9)  )))



(defn coord-pairs [coords]

  (loop
    [res [] notused coords]

    (if (empty? notused)
      res
      (recur (concat res (for
                           [x coords]
                           [(first notused) x]
                           )) (rest notused)
      )

    )

  )
)

(defn top-left [coord]

  [
   (* 3 (int (/ (first coord) 3)))
    ( * 3 (int (/ (second coord) 3)))
   ]
  )

(defn block-values [board coord]

  (let

    [tl (top-left coord)
     c (coord-pairs [0 1 2])
     ]

    (into #{} (map (fn [q] (value-at board q))
    (for [tmp c]

      (map + tmp tl)
      )))

    )

  )

(defn valid-values-for [board coord]

  (if (has-value? board coord)
    #{}
    (set/difference #{0 1 2 3 4 5 6 7 8 9} (block-values board coord) (col-values board coord) (row-values board coord))
    )

  )

(defn filled? [board]
  (not (contains? (into #{} (apply concat (map (fn [k] (row-values board [k 0])) (range 0 9)))) 0)))


(defn rows [board]
  (map (fn [k] (row-values board [k 0])) (range 0 9)))


(defn valid-rows? [board]
  nil)

(defn cols [board]
  (map (fn [k] (col-values board [0 k])) (range 0 9)))

(defn valid-cols? [board]
  nil)

(defn blocks [board]
  nil
  )


(defn valid-blocks? [board]
  nil)

(defn valid-solution? [board]
  nil)

(defn set-value-at [board coord new-value]
  nil)

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
