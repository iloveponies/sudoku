(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (set
   (map #(get % (second coord)) board)))

(defn coord-pairs [coords]
  (for [acoords coords
        bcoords coords]
    [acoords bcoords]))

(defn block-values [board coord]
  (let [triples [[0 1 2] [3 4 5] [6 7 8]]
        topleft [(first (filter #(some #{(first coord)} %) triples))
                 (first (filter #(some #{(second coord)} %) triples))]
        c-pairs (for [f-triple (first topleft)
                      s-triple (second topleft)]
                  [f-triple s-triple])]
    (set (map (partial get-in board) c-pairs))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [row-set (row-values board coord)
          col-set (col-values board coord)
          block-set (block-values board coord)]
      (reduce set/difference [all-values row-set col-set block-set]))))

(defn filled? [board]
  (let [all-nums (reduce concat board)]
    (every? #(not= % 0) all-nums)))

(defn rows [board]
  (loop [row 0
         acc []]
    (if (= row 8)
      (conj acc (row-values board [row 0]))
      (recur (inc row) (conj acc (row-values board [row 0]))))))

(defn valid-rows? [board]
  (every? #(= all-values %) (rows board)))

(defn cols [board]
  (loop [col 0
         acc []]
    (if (= col 9)
      acc
      (recur (inc col)(conj acc (col-values board [0 col]))))))

(defn valid-cols? [board]
  (every?  #(= all-values %) (cols board)))

(defn blocks [board]
  (let [toplefts (for [trows [0 3 6]
                       tcols [0 3 6]]
                   [trows tcols])]
    (map (partial block-values board) toplefts)))


(defn valid-blocks? [board]
  (every? #(= all-values %) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [row 0]
    (let [colind (loop [col 0]
                   (cond (= col 9) nil
                         (not (has-value? board [row col])) col
                         :else (recur (inc col))))]
      (cond (= row 9) nil
            ((complement nil?) colind) [row colind]
            :else (recur (inc row))))))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      [])
    (let [nextempty (find-empty-point board)]
      (for [valid-vals  (valid-values-for board nextempty)
            solution (solve-helper (set-value-at board nextempty valid-vals))]
       solution))))

(defn solve [board]
  (first (solve-helper board)))
