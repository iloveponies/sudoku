(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (let [[x y] coord]
    (get-in board [x y])
    )
  )

(defn has-value? [board coord]
  (let [[x y] coord]
    (not (= (get-in board [x y]) 0))
    )
  )

(defn row-values [board coord]
  (let [[x y] coord]
    (set (get-in board [x]))
    )
  )

(defn col-values [board coord]
  (let [[x y] coord]
    (let [helper (fn [n aseq]
                   (if (= 9 n)
                     aseq
                     (recur (inc n)(conj aseq (value-at board [n y])))
                     )
                   )]
      (set (helper 0 []))
      )
    )
  )

(defn coord-pairs [coords]
  (for [f coords
        g coords]
    [f g]
    )
  )

(defn top-left [coords]
  (let [[x y] coords]
    (cond (< x 3) (cond (< y 3) [0 0]
                        (< y 6) [0 3]
                        (< y 9) [0 6]
                   )
          (< x 6) (cond (< y 3) [3 0]
                        (< y 6) [3 3]
                        (< y 9) [3 6]
                   )
          (< x 9) (cond (< y 3) [6 0]
                        (< y 6) [6 3]
                        (< y 9) [6 6]
                   )
          )
    )
  )

(defn block-coords [coords]
  (let [[x y] coords]
    [[x y] [(+ 1 x) y] [(+ 2 x) y]
     [x (+ 1 y)] [(+ 1 x) (+ 1 y)] [(+ 2 x) (+ 1 y)]
     [x (+ 2 y)] [(+ 1 x) (+ 2 y)] [(+ 2 x) (+ 2 y)]]
    )
  )

(defn block-values [board coord]
  (let [block (block-coords (top-left coord))
       values (fn [block]
                 (for [coords block]
                   (value-at board coords)
                  )
                )]
    (set (values block))
   )
  )

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    (set ())
    (set/difference all-values (set/union (block-values board coord) (row-values board coord) (col-values board coord)))
    )
  )

(defn filled? [board]
  (let [all (fn [n aset]
              (if (= 9 n)
                aset
                (set/difference aset (set (get-in board [n])))
                )
              )]
      (empty? (all 0 all-values)
        )
    )
  )

(defn rows [board]
  (let [helper (fn [n aseq]
                 (if (= 9 n)
                   aseq
                   (recur (inc n) (conj aseq (row-values board [n 0])))
                   )
                 )]
    (helper 0 [])
    )
  )

(defn valid-rows? [board]
    (apply = #{1 2 3 4 5 6 7 8 9} (rows board))
  )

(defn cols [board]
     (let [helper (fn [n aseq]
                 (if (= 9 n)
                   aseq
                   (recur (inc n) (conj aseq (col-values board [0 n])))
                   )
                 )]
    (helper 0 [])
    )
  )

(defn valid-cols? [board]
  (apply = #{1 2 3 4 5 6 7 8 9} (cols board))
  )

(def all-blocks
  [[0 0][0 3] [0 6] [3 0] [3 3] [3 6] [6 0] [6 3] [6 6]]
  )

(defn blocks [board]
   (for [x all-blocks] (block-values board x))
  )

(defn valid-blocks? [board]
  (apply = #{1 2 3 4 5 6 7 8 9} (blocks board))
  )

(defn valid-solution? [board]
  (and (valid-cols? board) (valid-rows? board) (valid-blocks? board))
  )

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value)
  )

(defn find-empty-point [board]
  (let [helper (fn [x y]
                 (cond (= 9 x) (recur 0 (inc y))
                       (= 9 y) nil
                       (= 0 (get-in board [x y])) [x y]
                       :else (recur (inc x) y)
                       )
                 )]
    (helper 0 0)
    )
  )

(defn solve [board]
  nil)

