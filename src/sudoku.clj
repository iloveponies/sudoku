(ns sudoku
  (:require [clojure.set :as set]))


(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board coord]
  (into #{} (get board (first coord))))

(defn col-values [board coord]
  (into #{} (map #(get % (second coord)) board)))

(defn coord-pairs [coords]
  (for [y coords
        x coords]
    [y x]))

(defn tl-coord-pairs [coords]
  (let [coord-ranges (map #(range % (+ 3 %)) coords)](for [y (first coord-ranges)
                                                           x (second coord-ranges)]
     [y x])))

(defn block-values [board coord]
  (let [topleft-of-block
        (fn [coord] (for [co coord]
                      (* 3 (Math/floorDiv co 3))))
        tl-cps (tl-coord-pairs (topleft-of-block coord))]
    (into #{} (for [cp tl-cps]
                (value-at board cp)))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (apply set/union (map #(% board coord) [block-values row-values col-values])))))

(defn filled? [board]
  (not-any? #(== % 0) (flatten board)))

(defn rows [board]
  (for [y-coord (range 9)]
    (row-values board [y-coord 0])))

(defn valid-sets? [sets]
  (every? #(= all-values %) sets))

(defn valid-rows? [board]
  (valid-sets? (rows board)))

(defn cols [board]
  (for [x-coord (range 9)]
    (col-values board [0 x-coord])))

(defn valid-cols? [board]
  (valid-sets? (cols board)))


(defn blocks [board]
  (for [block-coord-pairs (for [tl-y (range 0 7 3)
                                tl-x (range 0 7 3)]
                            (tl-coord-pairs [tl-y tl-x]))]
    (into #{} (map #(value-at board %) block-coord-pairs))))

(defn valid-blocks? [board]
  (valid-sets? (blocks board)))

(defn valid-solution? [board]
  (every? identity (map #(% board) [valid-blocks? valid-rows? valid-cols?])))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [y 0]
    (let [x (.indexOf (get board y) 0)]
      (if (not= -1 x)
        [y x]
        (recur (inc y))))))

(defn solve [board]
  (if (valid-solution? board)
    board
    (let [this-coord (find-empty-point board)
          poss-values (valid-values-for board this-coord)]
      (if (empty? poss-values)
        (lazy-seq)
        (for [next-val poss-values
              solution (solve (set-value-at board this-coord next-val))]
          solution)))))
