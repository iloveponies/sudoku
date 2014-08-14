(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  ((complement zero?) (value-at board coord)))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (set (map (fn [x] (get x (second coord))) board)))


(defn coord-pairs [coords]
  (vec (for [n1 coords
        n2 coords]
    (vector  n1 n2))))

(defn block-coord [num]
  (* 3 (int (/ num 3))))


(defn block-values [board coord]
  (let [x (block-coord (first coord))
        y (block-coord (second coord))]
   (set (map (fn [x] (value-at board x)) (for [n1 (range x (+ 3 x))
          n2 (range y (+ 3 y))]
      (vector n1 n2))))))

(defn valid-values-for [board coord]
  (if (zero? (value-at board coord))
    (set/difference all-values
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))
    #{}))

(defn filled? [board]
  ((complement contains?) (set (apply clojure.set/union board)) 0))

(defn rows [board]
  (vec (map set board)))

(defn valid-rows? [board]
  (every? true? (map (fn [x] (= x all-values)) (rows board))))

(defn cols [board]
  (for [cols (range 9)]
    (col-values board [0 cols])))

(defn valid-cols? [board]
  (every? true? (map (fn [x] (= x all-values)) (cols board))))

(defn blocks [board]
  (for [n1 [1 3 6]
        n2 [1 3 6]]
    (block-values board (vector n1 n2))))

(defn valid-blocks? [board]
  (every? true? (map (fn [x] (= x all-values)) (blocks board))))

(defn valid-solution? [board]
  (and (valid-cols? board) (valid-rows? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [vals (for [n1 (range 9) n2 (range 9)] (vector n1 n2))]
    (cond
     (empty? vals) nil
     (zero? (value-at board (first vals))) (first vals)
     :else (recur (rest vals)))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      nil)
    (let [tyh (find-empty-point board)]
      (loop [vals (valid-values-for board tyh)]
        (if (empty? vals)
          nil
          (let [ehdokas (solve (set-value-at board tyh (first vals)))]
            (if ((complement nil?) ehdokas)
              ehdokas
              (recur (rest vals)))))))))
