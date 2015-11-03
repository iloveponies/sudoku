(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn coord-range [] (range 0 9))

(defn all-values [] (set (range 1 10)))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not(zero? (value-at board coord))))

(defn row-values [board [row _]]
  (set (for [col (coord-range)]
         (value-at board [row col]))))

(defn col-values [board [_ col]]
  (set (for [row (coord-range)]
         (value-at board [row col]))))

(defn coord-pairs [coords]
  (for [row coords col coords]
    [row col]))

(defn top-left [[row col]] 
  [(* 3 (unchecked-divide-int row 3))
   (* 3 (unchecked-divide-int col 3))])

(defn block-values [board coord]
  (let [[row col] (top-left coord)]
    (set (for [i (range 0 3)
               j (range 0 3)]
           (value-at board [(+ row i) (+ col j)])
           ))))

(defn valid-values-for [board coord]
  (if (> (value-at board coord) 0) 
    #{}
    (clojure.set/difference (all-values)
                            (block-values board coord)
                            (row-values board coord)
                            (col-values board coord)
                            )))

(defn counter-to-coord [i]
  [(unchecked-divide-int i 9) 
   (mod i 9)])

(defn filled? [board]
  (loop [i 0]
    (cond 
      (= 0 (value-at board (counter-to-coord i))) false
      (>= i 81) true
      :else (recur (inc i)))))

(defn rows [board]
  (for [row (coord-range)]
    (row-values board [row 0])))

(defn cols [board]
  (for [col (coord-range)]
    (col-values board [0 col])))

(defn blocks [board]
  (for [row [0 3 6] col [0 3 6]]
    (block-values board [row col])))

(defn has-all-values? [values]
  (empty? (clojure.set/difference (all-values) values)))

(defn valid-rows? [board]
  (every? has-all-values? (rows board)))

(defn valid-cols? [board]
  (every? has-all-values? (cols board)))

(defn valid-blocks? [board]
  (every? has-all-values? (blocks board)))

(defn valid-solution? [board]
  (and (valid-blocks? board)
       (valid-rows? board)
       (valid-cols? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [i 0]
    (cond 
      (= 0 (value-at board (counter-to-coord i))) (counter-to-coord i)
      (>= i 81) #{}
      :else (recur (inc i))
      )))

(defn solve [board]
  (if (filled? board)
    ;; We're at the end, check solution!
    (if (valid-solution? board) board nil)
    ;; We still got some empty points
    (let [empty-point (find-empty-point board)]
      (some not-empty ;; Just take the first one that's okay
            (for [value (valid-values-for board empty-point)]
              (solve (set-value-at board empty-point value)))))))
