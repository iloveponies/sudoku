(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (get-in board coord))))

(defn row-values [board coord]
  (let [[row col] coord]
      (set (get board row))
    ))

(defn col-values [board coord]
  (let [[row col] coord
         helper (fn [coll] (get coll col))]
         (set (map helper board))
  ))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
        [x y]))

(defn block-values [board coord]
  (let [x-coord (first coord)
        y-coord (last coord)
        first-part  #{0 1 2}
        second-part #{3 4 5}
        third-part  #{6 7 8}
        row-helper (fn [row-indices] (for [row row-indices] (get board row))   )
        get-rows (fn [] (cond
            (contains? first-part x-coord) (row-helper first-part)
            (contains? second-part x-coord) (row-helper second-part)
            (contains? third-part x-coord) (row-helper third-part)
           ))
        get-cols (fn [coll] (cond
            (contains? first-part y-coord) (map (fn [row] (take 3 row)) coll)
            (contains? second-part y-coord) (map (fn [row] (take 3 (drop 3 row))) coll)
            (contains? third-part y-coord) (map (fn [row] (drop 6 row)) coll)
          ))]
        (set (reduce concat '() (get-cols (get-rows))))
  ))

(defn valid-values-for [board coord]
  (if (not (zero? (value-at board coord))) #{}
      (let [valid-values #{1 2 3 4 5 6 7 8 9}
            used-row-values (row-values board coord)
            used-col-values (col-values board coord)
            used-block-values (block-values board coord)
            used-values (set/union used-row-values used-col-values used-block-values)]
        (set/difference valid-values used-values)
        )
     )
  )

(defn filled? [board]
  (reduce (fn [x y] (and (not (contains? (set y) 0 )) x)) true board))

(defn rows [board]
  (map (fn [x] (set x)) board))

(defn valid-rows? [board]
  (let [valid-row #{1 2 3 4 5 6 7 8 9}
        row-values (rows board)]
        (every? empty? (map (fn [row] (set/difference valid-row row)) row-values))
 ))

(defn cols [board]
  (for [x (take (count board) (range))]
  (set (map (fn [row] (get row x)) board))
   ))

(defn valid-cols? [board]
  (let [valid-col #{1 2 3 4 5 6 7 8 9}
        col-values (cols board)]
        (every? empty? (map (fn [col] (set/difference valid-col col)) col-values))
 ))

(defn blocks [board]
  (for [x-coord [0 3 6]
        y-coord [0 3 6]]
   (block-values board [x-coord y-coord])
    )
  )

(defn valid-blocks? [board]
  (let [valid-block #{1 2 3 4 5 6 7 8 9}
        block-values (blocks board)]
        (every? empty? (map (fn [block] (set/difference valid-block block)) block-values))
 ))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
(let [x-coord (first coord)
      y-coord (last coord)
      old-row (get board x-coord)
      new-row (assoc old-row y-coord new-value)]
    (assoc board x-coord new-row)
  ))

(defn find-empty-point [board]
  (let [helper (fn [ind] (if (contains? (set (get board ind)) 0) ind nil ))
        rows-with-empty-points (filter (fn [x] (not (nil? x))) (for [x (take 9 (range))] (helper x)))
        x-coord (first rows-with-empty-points)
        row (get board x-coord)
        cols-with-empty-points (filter (fn [x] (not (nil? x))) (for [x (take 9 (range))] (if (zero? (get row x)) x nil)))
        y-coord (first cols-with-empty-points)]
        [x-coord y-coord]
  ))

(defn solve [board]
    (if (valid-solution? board)
      board
      (let [empty-point (find-empty-point board)]
        (for [value (valid-values-for board empty-point)
          solved (solve (set-value-at board empty-point value))]
          solved))))
