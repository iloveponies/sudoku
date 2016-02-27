(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (set (map (fn [x] (value-at board [x col])) (range 9)))))

(defn coord-pairs [coords]
  (vec (for [row coords
             col coords]
         (conj [] row col))))

(defn block-values [board coord]
  (let [[row col] coord
        top-left [(* 3 (int (/ row 3)))
                  (* 3 (int (/ col 3)))]
        [tlr tlc] top-left]
    (set (for [r (range tlr (+ 3 tlr))
               c (range tlc (+ 3 tlc))]
       (value-at board [r c])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference #{1 2 3 4 5 6 7 8 9}
                    (set/union
                     (row-values board coord)
                     (col-values board coord)
                     (block-values board coord)))))

(defn filled? [board]
  (empty? (filter (fn [x] (contains? (set (get board x)) 0)) (range 9))))

(defn rows [board]
  (vec (map (fn [i] (row-values board [i 0])) (range 9))))

(defn valid-rows? [board]
  (every? (fn [row] (= row #{1 2 3 4 5 6 7 8 9})) (rows board)))

(defn cols [board]
  (vec (map (fn [i] (col-values board [0 i])) (range 9))))

(defn valid-cols? [board]
   (every? (fn [col] (= col #{1 2 3 4 5 6 7 8 9})) (cols board)))

(defn blocks [board]
  (vec (map (fn [coord] (block-values board coord)) (coord-pairs [0 3 6]))))

(defn valid-blocks? [board]
  (let [helper (fn [s coll]
                 (if (empty? coll)
                   true
                   (if (empty? s)
                     (recur
                      (set/difference #{1 2 3 4 5 6 7 8 9} (first coll))
                      (rest coll))
                     false)))]
    (helper #{} (blocks board))))

(defn valid-solution? [board]
  (if (and (valid-rows? board)
           (valid-cols? board)
           (valid-blocks? board)) true false))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [points (coord-pairs (range 9))
        finder (fn [point
                    coll]
                 (cond
                   (= 0 (value-at board point)) point
                   (empty? coll) nil
                   :else (recur (first coll) (rest coll))))]
    (finder (first points) (rest points))))

(defn solve
  ([board] (first (solve board (find-empty-point board))))

  ([board coord]
   (if (valid-solution? board)
     [board]
     (let [remaining (valid-values-for board coord)]
       (for [elem remaining
             solution (solve
                       (set-value-at board coord elem)
                       (find-empty-point
                        (set-value-at board coord elem)))]
         solution)))))
