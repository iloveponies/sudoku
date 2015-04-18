(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board [y _]]
  (set (get board y)))

(defn col-values [board [_ x]]
  (set (map (fn [row] (get row x)) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board [y x]]
  (let [row-start (- y (mod y 3))
        col-start (- x (mod x 3))]
    (set (map
          (fn [[row col]] (let [coords [(+ row-start row) (+ col-start col)]]
                            (value-at board coords)))
          (coord-pairs [0 1 2])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [invalid (set/union (row-values board coord) (col-values board coord) (block-values board coord))]
      (set/difference all-values invalid))))

(defn filled? [board]
  (not (contains? (set (flatten board)) 0)))

(defn rows [board]
  (map set board))

(defn valid-number-sets? [sets]
  (reduce (fn [a a-set] (and a (empty? (set/difference all-values a-set)))) true sets))


(defn valid-rows? [board]
  (valid-number-sets? (rows board)))

(defn cols [board]
    (map (fn [col]
         (set (map (fn [row] (value-at board [row col])) (range 9))))
       (range 9)))

(defn valid-cols? [board]
  (valid-number-sets? (cols board)))

(defn blocks [board]
  (let [corners (coord-pairs [0 3 6])]
    (map (fn [pair] (set (block-values board pair))) corners)))

(defn valid-blocks? [board]
  (valid-number-sets? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (fn [coord] (= 0 (value-at board coord))) (coord-pairs (range 9)))))

(defn solve [board]
  (let [filled (filled? board)]
    (cond (and filled (valid-solution? board)) board
          filled #{}
          :else (let [point (find-empty-point board)
                      valid-vals (valid-values-for board point)]
                  (if (empty? valid-vals)
                    #{}
                    (loop [values valid-vals]
                      (if (empty? values) #{}
                        (let [candidate (solve (set-value-at board point (first values)))]
                          (if (empty? candidate)
                            (recur (rest values))
                            candidate)))))))))

