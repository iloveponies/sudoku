(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})
(def block-size 3)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (set (map #(value-at board [% col]) (range (count board)))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board [row col]]
  (let [block-row (int (* (int (/ row block-size)) block-size))
        block-col (int (* (int (/ col block-size)) block-size))]
    (set
      (for [r (range block-row (+ block-row block-size))
            c (range block-col (+ block-col block-size))]
        (value-at board [r c])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [used-values (set/union (row-values board coord)
                                 (col-values board coord)
                                 (block-values board coord))]
      (set/difference all-values used-values))))


(defn filled? [board]
  (not (contains? (set (apply concat board)) 0)))

(defn rows [board]
  (map #(row-values board [% 0]) (range (count board))))

(defn valid-rows? [board]
  (= #{all-values} (set (rows board))))

(defn cols [board]
  (map #(col-values board [0 %]) (range (count board))))

(defn valid-cols? [board]
  (= #{all-values} (set (cols board))))

(defn blocks [board]
  (let [block-range (range 0 (count board) block-size)]
    (for [r block-range
          c block-range]
      (block-values board [r c]))))

(defn valid-blocks? [board]
  (= #{all-values} (set (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [index (.indexOf (apply concat board) 0)
        row (int (/ index (count board)))
        col (mod index (count board))]
    [row col]))

(defn solve [board]
  (if (filled? board)
    (when (valid-solution? board)
      board)
    (let [empty-point (find-empty-point board)
          valid-values (valid-values-for board empty-point)]
      (mapcat #(solve (set-value-at board empty-point %)) valid-values))))
