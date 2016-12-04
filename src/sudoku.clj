(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (zero? (get-in board coord))
    false
    true))

(defn row-values [board coord]
  (let [[r _] coord]
    (set
          (for [c (range 9)]
           (value-at board [r c]))
      )))


(defn col-values [board coord]
  (let [[_ c] coord]
    (set
          (for [r (range 9)]
           (value-at board [r c]))
      )))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board [r c]]
  (let [top (* 3 (quot r 3))
        left (* 3 (quot c 3))]
    (set
      (for [br (range 3)
            bc (range 3)]
        (value-at board [(+ top br) (+ left bc)])
        ))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [nv (set/union (row-values board coord) (col-values board coord) (block-values board coord))]
      (set/difference all-values nv)
      )))

(defn filled? [board]
  (let [bv (set
      (for [br (range 9)
            bc (range 9)]
        (value-at board [br bc])
        ))]
    (not (contains? bv 0))
    ))

(defn valid-element? [el]
  (empty? (set/difference all-values el)))

(defn rows [board]
  (for [r (range 9)]
    (row-values board [r 0])
    ))

(defn valid-rows? [board]
  (if (= #{true}
         (set (map valid-element? (rows board))))
    true
    false
       ))

(defn cols [board]
  (for [c (range 9)]
    (col-values board [0 c])
    ))

(defn valid-cols? [board]
  (if (= #{true}
         (set (map valid-element? (cols board))))
    true
    false
       ))

(defn blocks [board]
  (for [r [0 3 6]
        c [1 4 7]]
    (block-values board [r c])
    ))

(defn valid-blocks? [board]
  (if (= #{true}
         (set (map valid-element? (blocks board))))
    true
    false
       ))

(defn valid-solution? [board]
  (and (valid-blocks? board)
       (valid-rows? board)
       (valid-cols? board)
       ))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first
    (filter boolean
    (for [r (range 9)
          c (range 9)]
      (if (not (has-value? board [r c]))
        [r c])
      ))))




(defn solve [board]
  (if (valid-solution? board)
    board
    (let [coord (find-empty-point board)]
      (for [posit (valid-values-for board coord)
            solution (solve (set-value-at board coord posit))]
        solution)
      )))
