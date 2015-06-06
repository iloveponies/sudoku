(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (set (map #(get % col) board))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board coord]
  (let [[r c] coord
        top-row (- r (mod r 3))
        top-col (- c (mod c 3))]
    (set (for [row (range top-row (+ top-row 3))
               col (range top-col (+ top-col 3))]
           (value-at board [row col])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))))

(defn filled? [board]
  (= #{()} (set (map #(filter zero? %) board))))

(defn rows [board]
  (into [] (for [row (range 0 9)] (row-values board [row 0]))))

(defn valid-rows? [board]
  (every? #(= (count %) (count (set %))) (map #(filter pos? %) board)))

(defn cols [board]
  (into [] (for [col (range 0 9)] (col-values board [0 col]))))



(defn col-values [board coord]
  (let [[_ col] coord]
    (set (map #(get % col) board))))

(defn valid-cols? [board]
  (every? #(= (count %) (count (set %)))
          (for [col (range 0 9)] (filter pos? (map #(get % col) board)))))

(defn blocks [board]
  (into [] (for [row [0 3 6] col [0 3 6]] (block-values board [row col]))))

(defn valid-blocks? [board]
  (every? #(= (count %) (count (set %))) (for [row [0 3 6]
                                               col [0 3 6]]
                                           (filter pos? (for [r (range row (+ row 3))
                                                              c (range col (+ col 3))]
                                                          (value-at board [r c]))))))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [row 8
         col 8]
    (cond (zero? (value-at board [row col])) [row col]
          (zero? col) (if (zero? row) nil (recur (dec row) 8))
          :else (recur row (dec col)))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      nil)
    (loop [empty-point (find-empty-point board)
           values (valid-values-for board empty-point)]
      (if (empty? values)
        nil
        (let [b (solve (set-value-at board empty-point (first values)))]
          (if (nil? b)
            (recur empty-point (rest values))
            b))))))
