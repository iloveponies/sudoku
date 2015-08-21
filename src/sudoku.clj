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
  (set (for [row board]
         (get row (second coord)))))

(defn coord-pairs [coords]
  (apply concat (map (fn [a] (map (fn [b] (vector a b)) coords)) coords)))

(def top-block-coords
  (coord-pairs [0 1 2]))

(defn block-coords [coord]
  (let [x (- (first coord) (mod (first coord) 3))
        y (- (second coord) (mod (second coord) 3))]
    (map #(vector (+ (first %) x) (+ (second %) y)) top-block-coords)))

(defn block-values [board coord]
  (set (map #(value-at board %) (block-coords coord))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (clojure.set/difference all-values (clojure.set/union
                                         (row-values board coord)
                                         (col-values board coord)
                                         (block-values board coord)))))

(defn filled? [board]
  (not-any? zero? (flatten board)))

(defn rows [board]
  (map #(row-values board [% 0]) (range 9)))

(defn cols [board]
  (map #(col-values board [0 %]) (range 9)))

(defn blocks [board]
  (map #(block-values board [% (* 3 (mod % 3))]) (range 9)))

(defn valid-rows? [board]
  (every? #(= all-values %) (rows board)))

(defn valid-cols? [board]
  (every? #(= all-values %) (cols board)))

(defn valid-blocks? [board]
  (every? #(= all-values %) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn coord-from-index [index]
  (vector (mod index 9) (int (/ index 9))))

(defn find-next-empty [board index]
  (let [coord (coord-from-index index)]
    (if (= 0 (value-at board coord)) coord
      (find-next-empty board (+ 1 index)))))

(defn find-empty-point [board]
  (if (filled? board) nil
    (find-next-empty board 0)))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board) board nil)
    (let [coord (find-empty-point board)]
      (first (filter (complement nil?)
                     (map #(solve (set-value-at board coord %))
                          (valid-values-for board coord)))))))
