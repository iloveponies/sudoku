(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (set (map #(get % col) board)))

(defn coord-pairs [coord]
  (for [r coord, c coord]
    [r c]))

(defn block-values [board coord]
  (let [topleft (map #(* 3 (quot % 3)) coord)
        indices (for [c topleft] (map #(+ c %) (range 3)))
        block   (for [i (first indices), j (second indices)] [i j])]
    (into #{} (map #(value-at board %) block))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [used-values
          (reduce set/union #{}
                  (map #(% board coord)
                       [row-values col-values block-values]))]
      (set/difference all-values used-values))))

(defn filled? [board]
  (every? (fn [x] (every? (fn [y] (> y 0)) x)) board))

(defn rows [board]
  (map set board))

(defn- valid? [board vals]
  (every? #(= % all-values) vals))

(defn valid-rows? [board]
  (and (filled? board) (valid? board (rows board))))

(defn- transpose [board]
  (apply map vector board))

(defn cols [board]
  (let [transposed (transpose board)]
    (map set transposed)))

(defn valid-cols? [board]
  (and (filled? board) (valid? board (cols board))))

(defn blocks [board]
  (let [block-coords [[0 0] [0 3] [0 6]
                      [3 0] [3 3] [3 6]
                      [6 0] [6 3] [6 6]]]
    (map (partial block-values board) block-coords)))

(defn valid-blocks? [board]
  (and (filled? board) (valid? board (blocks board))))

(defn valid-solution? [board]
  (every? true? (map #(% board) [valid-rows? valid-cols? valid-blocks?])))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter #(= 0 (value-at board %)) (coord-pairs (range 10)))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      [])
    (let [empty-point (find-empty-point board)
          valid-values (valid-values-for board empty-point)]
      (for [vv valid-values
            solution (solve (set-value-at board empty-point vv))]
        solution))))
