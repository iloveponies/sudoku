(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn is-empty? [board coord]
  (zero? (value-at board coord)))

(defn has-value? [board coord]
  (not (is-empty? board coord)))

(defn row-values [board [row _]]
  (set (get-in board [row])))

(defn col-values [board [_ col]]
  (loop [i 0
         result #{}]
    (if (= i 9)
      result
      (recur (inc i) (conj result (get-in board [i col]))))))

(defn coord-pairs
  ([coords]
   (apply
    vector
    (for [row coords
          col coords]
      (vector row col))))
  ([row-coords col-coords]
   (apply
    vector
    (for [row row-coords
          col col-coords]
      (vector row col)))))

(defn block-values [board [row col]]
  (let [[row col] ((fn [r c]
                     [(- r (mod r 3))
                      (- c (mod c 3))]) row col)]
    (reduce
     (fn [result cell] (conj result (get-in board cell)))
     #{}
     (coord-pairs
      (range row (+ row 3))
      (range col (+ col 3))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (clojure.set/difference
     all-values
     (clojure.set/union
      (row-values board coord)
      (col-values board coord)
      (block-values board coord)))))

(defn filled? [board]
  (let [board-values (map
                      (fn [cell]
                        (value-at board cell))
                      (coord-pairs (range 9)))]
    (not (some zero? board-values))))

(defn rows [board]
  (apply vector
         (for [row (range 9)]
           (row-values board [row 0]))))

(defn valid-rows? [board]
  (every?
   (fn [row]
     (= all-values row))
   (rows board)))

(defn cols [board]
  (apply vector
         (for [col (range 9)]
           (col-values board [0 col]))))

(defn valid-cols? [board]
  (every?
   (fn [col]
     (= all-values col))
   (cols board)))

(defn blocks [board]
  (apply vector
         (for [row (range 0 9 3)
               col (range 0 9 3)]
           (block-values board [row col]))))

(defn valid-blocks? [board]
  (every?
   (fn [block]
     (= all-values block))
   (blocks board)))

(defn valid-solution? [board]
  (and
   (valid-rows? board)
   (valid-cols? board)
   (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first
   (filter
    (fn [cell] (is-empty? board cell))
    (coord-pairs (range 9)))))

(defn solve [board]
  (let [solve-helper (fn helper [board]
                       (let [pos (find-empty-point board)]
                         (if (nil? pos)
                           (if (valid-solution? board)
                             [board]
                             [])
                           (for [value (valid-values-for board pos)
                                 solution (helper (set-value-at
                                                   board
                                                   pos
                                                   value))]
                             solution))))]
    (first (solve-helper board))))
