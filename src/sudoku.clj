(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (contains? all-values (value-at board coord)))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (loop [acc 0
         sec (second coord)
         values #{}]
    (cond
     (> acc 8) values
     :else (recur (inc acc) sec (conj values (value-at board [acc sec]))))))

(defn coord-pairs [coords]
  (for [a coords b coords]
    [a b]))

(defn block-values [board coord]
  (let [[x y]
          coord
        value
          (fn [z] (* 3 (int (/ z 3))))
        top-row
          (value x)
        top-col
          (value y)]
    (set
     (for [row (range top-row (+ top-row 3))
           col (range top-col (+ top-col 3))]
          (value-at board [row col])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
      #{}
      (set/difference all-values
                      (set/union
                        (block-values board coord)
                        (row-values board coord)
                        (col-values board coord)))))

(defn board-values [board]
  (loop [acc 0 values #{}]
    (if (> acc 8)
        values
        (recur (inc acc) (clojure.set/union values (row-values board [acc acc]) (col-values board [acc acc]))))))

(defn filled? [board]
  (not (contains? (board-values board) 0)))

(defn rows [board]
  (loop [acc 0 all-rows []]
    (if (< 8 acc)
        all-rows
        (recur (inc acc) (conj all-rows (row-values board [acc acc]))))))

(defn valid-rows? [board]
  (every? (fn [row]
            (empty? (set/difference all-values row)))
          (rows board)))

(defn cols [board]
  (loop [acc 0 all-cols []]
    (if (< 8 acc)
        all-cols
        (recur (inc acc) (conj all-cols (col-values board [acc acc]))))))

(defn valid-cols? [board]
    (every? (fn [col]
            (empty? (set/difference all-values col)))
          (cols board)))

(defn blocks [board]
  (let [a (for [b (range (count board))
                :when (zero? (rem b 3))]
            b)]
    (for [b-row a
          b-col a]
      (block-values
         board
         [b-row b-col]))))

(defn valid-blocks? [board]
    (every? (fn [block]
            (empty? (set/difference all-values block)))
          (blocks board)))

(defn valid-solution? [board]
  (and (valid-blocks? board) (valid-cols? board) (valid-rows? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [has-zero? (fn [coord]
                      (let [[x y] coord]
                           (not (has-value? board [x y]))))]
    (first (filter has-zero?
                   (coord-pairs (range (count board)))))))

(defn solve [board]
  (if (and (filled? board)
           (valid-solution? board))
      board
    (let [coord (find-empty-point board)]
      (if (nil? coord)
          nil
          (first (filter boolean
                         (for [new-value (valid-values-for board coord)]
                              (solve (set-value-at board coord new-value)))))))))
