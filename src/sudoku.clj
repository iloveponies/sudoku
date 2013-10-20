(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [u v]]
  (into #{} (board u)))

(defn col-values [board [u v]]
  (into #{}
        (map (fn [row] (row v))
             board)))

(defn coord-pairs [coords]
  (for [a coords
        b coords]
    [a b]))

(defn block-topleft [[u v]]
  [(* 3 (int (/ u 3))) (* 3 (int (/ v 3)))])

(defn block-values [board coord]
  (let [[tu tv] (block-topleft coord)]
    (into #{}
          (for [[du dv] (coord-pairs [0 1 2])]
            (let [u (+ tu du)
                  v (+ tv dv)]
              (value-at board [u v]))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (set/union (row-values board coord)
                               (col-values board coord)
                               (block-values board coord)))))

(defn filled? [board]
  (every? (comp not zero?)
          (for [coord (coord-pairs (range 0 9))]
            (value-at board coord))))

(defn rows [board]
  (map (partial row-values board)
       (map vector (range 0 9) (repeat 9 0))))

(defn valid-rows? [board]
  (every? (comp (partial == 9) count)
          (rows board)))

(defn cols [board]
  (map (partial col-values board)
       (map vector (repeat 9 0) (range 0 9))))

(defn valid-cols? [board]
  (every? (comp (partial == 9) count)
          (cols board)))

(defn blocks [board]
  (map (partial block-values board)
       (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (every? (comp (partial == 9) count)
          (blocks board)))

(defn valid-solution? [board]
  (every? (fn [f] (f board))
          [filled?
           valid-rows?
           valid-cols?
           valid-blocks?]))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (some (fn [coord]
          (if (has-value? board coord)
            nil
            coord))
        (coord-pairs (range 0 9))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      nil)
    (let [coord (find-empty-point board)]
      (some (fn [value]
              (solve (set-value-at board coord value)))
            (valid-values-for board coord)))))
