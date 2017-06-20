(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[& row] (get board (get coord 0))]
  (set row)))

(defn col-values [board coord]
  (loop [row 0
         col-vals #{}]
    (let [col-val (value-at board [row (get coord 1)])]
      (if (= 9 row)
        col-vals
        (recur (inc row) (conj col-vals col-val))))))

(defn coord-pairs [coords]
  (for [c1 coords
        c2 coords]
   (vector c1 c2)))

(defn block-corner [coord]
  (let [row (get coord 0)
        col (get coord 1)]
    (cond
      (and (<= 0 row 2) (<= 0 col 2)) [0 0] ; top-left
      (and (<= 0 row 2) (<= 3 col 5)) [0 3] ; top-middle
      (and (<= 0 row 2) (<= 6 col 8)) [0 6] ; top-right
      (and (<= 3 row 5) (<= 0 col 2)) [3 0] ; middle-left
      (and (<= 3 row 5) (<= 3 col 5)) [3 3] ; center
      (and (<= 3 row 5) (<= 6 col 8)) [3 6] ; middle-right
      (and (<= 6 row 8) (<= 0 col 2)) [6 0] ; bottom-left
      (and (<= 6 row 8) (<= 3 col 5)) [6 3] ; bottom-middle
      :else [6 6]))) ; bottom-right

(defn block-coords [coord]
  (let [top-left (block-corner coord)
        base-coords (coord-pairs [0 1 2])
        inc-coords (fn [coords coord] (conj coords (map + coord top-left)))]
    (reduce inc-coords [] base-coords)))

(defn block-values [board coord]
  (let [block (block-coords coord)
        values (fn [res c] (conj res (value-at board c)))]
    (reduce values #{} block)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference
      all-values
      (set/union (block-values board coord)
                 (row-values board coord)
                 (col-values board coord)))))

(defn filled? [board]
 (loop [row 0]
    (cond
      (= row 9) true
      (contains? (row-values board [row]) 0) false
      :else (recur (inc row)))))

(defn rows [board]
  (loop [row 0
         rows []]
    (if (= 9 row)
      rows
      (recur (inc row) (conj rows (row-values board [row]))))))

(defn valid-rows? [board]
  (let [rows (rows board)]
    (loop [row 0]
      (cond
        (= 9 row) true
        (not (= (get rows row) all-values)) false
        :else (recur (inc row))))))

(defn cols [board]
  (loop [col 0
         cols []]
    (if (= 9 col)
      cols
      (recur (inc col) (conj cols (col-values board [0 col]))))))

(defn valid-cols? [board]
  (let [cols (cols board)]
    (loop [col 0]
      (cond
        (= 9 col) true
        (not (= (get cols col) all-values)) false
        :else (recur (inc col))))))

(defn blocks [board]
  (let [blocks (coord-pairs [0 3 6])
        values (fn [res block] (conj res (block-values board block)))]
    (reduce values [] blocks)))

(defn valid-blocks? [board]
  (let [blocks (blocks board)]
    (loop [block 0]
      (cond
        (= 9 block) true
        (not (= (get blocks block) all-values)) false
        :else (recur (inc block))))))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [row 0
         col 0]
    (cond
      (= 9 row) nil
      (= 9 col) (recur (inc row) 0)
      :else (if (has-value? board [row col])
              (recur row (inc col))
              [row col]))))

(defn solve [board]
  nil)
