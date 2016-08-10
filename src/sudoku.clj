(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= (value-at board coord) 0)
    false
    true))

(defn row-values [board coord]
  (let [[r c] coord]
    (set (get board r))))

(defn col-values [board coord]
  (let [[r c] coord]
    (loop [acc #{}
           n 0]
      (let [v (value-at board [n c])]
        (cond (nil? v) acc
              :else (recur (conj acc v) (inc n)))))))

(defn coord-pairs [coords]
  (for [a coords
        b coords]
    (seq [a b])))

(defn block-values [board coord]
  (let [[r c] coord
        top-row (- r (mod r 3))
        top-col (- c (mod c 3))]
    (set (for [row (range top-row (+ top-row 3))
               col (range top-col (+ top-col 3))]
           (value-at board [row col])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (let [r (row-values board coord)
          c (col-values board coord)
          b (block-values board coord)
          u (set/union r c b)
          v (set (range 10))]
      (set/difference v u))))

(defn filled? [board]
  (cond
    (empty? board) true
    (some (fn [item] (== item 0)) (first board)) false
    :else (recur (rest board))))

(defn rows [board]
  (into [] (for [row (range 0 9)] (row-values board [row 0]))))

(defn valid-rows? [board]
  (every? #(= (count %) (count (set %))) (map #(filter pos? %) board)))

(defn cols [board]
  (seq (map set (for [col (range 9)] (col-values board [0 col])))))

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
  (and (valid-rows? board) (valid-rows? board) (valid-blocks? board)))

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
