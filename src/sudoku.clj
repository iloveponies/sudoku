(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if
   (== 0 (value-at board coord))
    false
    true)
  )

(defn row-values [board coord]
  (set (get board (get coord 0)))
  )

(defn col-values [board coord]
  (let [helper (fn [acc row]
                 (conj acc
                       (get row (get coord 1)))
                 )]
  (reduce helper #{} board))
  )

(defn coord-pairs [coords]
  (apply concat
         (for [number1 coords]
           (for [number2 coords]
             (let [helper (fn [acc next-num]
                            (conj [next-num] number2))]
               (reduce helper [] [number1])))))
  )

(defn top-left-corner [coords]
  (let
    [corner
     (fn [coord]
       (cond
       (== (rem coord 3) 0) coord
       (== (rem coord 3) 1) (- coord 1)
       :else (- coord 2)))]
    [(corner (get coords 0)) (corner (get coords 1))])
  )

(defn block-coordinates [coords]
  (let [all-pairs
        (coord-pairs
         (concat
          (range (get (top-left-corner coords) 0) (+ (get (top-left-corner coords) 0) 3))
          (range (get (top-left-corner coords) 1) (+ (get (top-left-corner coords) 1) 3))))
        valid?
        (fn [x]
          (if (and
               (>= (get x 0) (get (top-left-corner coords) 0))
               (<= (get x 0) (+ 2 (get (top-left-corner coords) 0)))
               (>= (get x 1) (get (top-left-corner coords) 1))
               (<= (get x 1) (+ 2 (get (top-left-corner coords) 1))))
            true
            false))]
    (filter valid? all-pairs))
  )

(defn block-values [board coord]
  (let [helper (fn [acc next-coord]
                 (conj acc (value-at board next-coord))
                 )]
    (reduce helper #{} (block-coordinates coord)))
  )

(defn valid-values-for [board coord]
  (if
    (has-value? board coord)
    #{}
    (set/difference
     (identity all-values)
     (set/union
      (row-values board coord)
      (col-values board coord)
      (block-values board coord))
     )
    )
  )

(defn all-board-values [board]
  (let [helper (fn [acc row]
                 (set/union
                  acc
                  (set row)))]
    (reduce helper #{} board))
  )

(defn filled? [board]
  (if
    (contains? (all-board-values board) 0)
    false
    true)
  )

(defn rows [board]
  (let [helper (fn [acc row]
                 (conj
                  acc
                  (set row)))]
    (reduce helper [] board))
  )

(defn valid-helper [board vals-seq]
  (cond
   (empty? vals-seq) true
   (= (first vals-seq) (identity all-values)) (valid-helper board (rest vals-seq))
   :else false))

(defn valid-rows? [board]
    (valid-helper board (rows board))
  )

(defn cols-helper [board acc colnum]
  (if (>= colnum (count board))
    acc
    (cols-helper
     board
     (conj
      acc
      (col-values board [0 colnum]))
     (inc colnum)))
  )


(defn cols [board]
  (cols-helper board [] 0)
  )

(defn valid-cols? [board]
  (valid-helper board (cols board))
  )

(defn block-helper [board acc rownum colnum]
  (cond
   (>= rownum (count board)) acc
   (>= colnum (count board)) (block-helper board acc (+ 3 rownum) 0)
   :else (block-helper
          board
          (conj acc (block-values board [rownum colnum]))
          rownum
          (+ 3 colnum))
   )
  )

(defn blocks [board]
  (block-helper board [] 0 0)
  )

(defn valid-blocks? [board]
  (valid-helper board (blocks board))
  )

(defn valid-solution? [board]
  (if
    (and
     (valid-rows? board)
     (valid-cols? board)
     (valid-blocks? board))
    true
    false)
  )

(defn set-value-at [board coord new-value]
  (assoc-in
   board
   coord
   new-value)
  )

(defn find-empty-point-helper [board rownum colnum]
  (cond
   (>= rownum (count board)) nil
   (>= colnum (count board)) (find-empty-point-helper board (inc rownum) 0)
   (== 0 (value-at board [rownum colnum])) [rownum colnum]
   :else (find-empty-point-helper board rownum (inc colnum))
   )
  )

(defn find-empty-point [board]
  (find-empty-point-helper board 0 0)
  )

(defn solve-helper [board]
  (if (filled? board)
    (if
      (valid-solution? board)
      board
      [])
    (let [next-empty (find-empty-point board)
          possible-vals (valid-values-for board next-empty)]
      (for [val possible-vals
            solution (solve-helper (set-value-at board next-empty val))]
        solution)))
  )

(defn solve [board]
  (solve-helper board)
  )
