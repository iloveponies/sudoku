(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= 0 (value-at board coord))
    false
    true))

(defn row-values [board coord]
  (let [[row-num col-num] coord
        row (get board row-num)]
    (into #{} row)))

(defn col-values [board coord]
  (let [[row-num col-num] coord
        col (map get board (repeat 9 col-num))]
    (into #{} col)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-start [z]
  (cond (< z 3) 0
        (< z 6) 3
        :else 6))

(defn block-coords [coord]
  (let [[row-num col-num] coord
        row-start (block-start row-num)
        col-start (block-start col-num)
        rows [row-start (inc row-start) (inc (inc row-start))]
        cols [col-start (inc col-start) (inc (inc col-start))]]
    (for [row rows
          col cols]
      [row col])))

(defn block-values [board coord]
  (let [coords (block-coords coord)]
    (into #{} (map (fn [n] (value-at board n)) coords))))

(defn valid-values-for [board coord]
  (if (not= 0 (value-at board coord))
    #{}
    (let [block-row-col-values (set/union (block-values board coord)
                                          (row-values board coord)
                                          (col-values board coord))]
      (set/difference all-values block-row-col-values))))

(defn filled? [board]
  (reduce (fn [not-empty? coord] (and not-empty? (has-value? board coord)))
          true
          (coord-pairs (range 9))))

(defn rows [board]
  (map (fn [row] (row-values board [row 0])) (range 9)))

(defn valid-rows? [board]
  (reduce (fn [valid? row] (and valid? (empty? (set/difference all-values row)))) true (rows board)))

(defn cols [board]
  (map (fn [col] (col-values board [0 col])) (range 9)))

(defn valid-cols? [board]
  (reduce (fn [valid? col] (and valid? (empty? (set/difference all-values col)))) true (cols board)))

(defn blocks [board]
  (map (fn [coord] (block-values board coord)) (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (reduce (fn [valid? block] (and valid? (empty? (set/difference all-values block)))) true (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [empty-coord nil
         coords (coord-pairs (range 9))]
    (if (not (has-value? board (first coords)))
      (first coords)
      (recur nil (rest coords)))))

(defn solve-helper [board]
  (if (filled? board)
    [board]
    (let [try-coord (find-empty-point board)
          valid-elems (valid-values-for board try-coord)]
      (for [elem valid-elems
            solution (solve-helper (set-value-at board try-coord elem))]
        solution))))

(defn solve [board]
  (first (reduce (fn [acc solution]
                   (if (valid-solution? solution)
                     (conj acc solution))) [] (solve-helper board))))
