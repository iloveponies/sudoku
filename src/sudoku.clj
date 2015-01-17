(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board coord]
  (let [[x y] coord]
    (set (get-in board [x]))))

(defn col-values [board coord]
  (let [[x y] coord]
    (set (reduce (fn [a b] (conj a (get-in board [b y]))) [] [0 1 2 3 4 5 6 7 8]))))

(defn coord-pairs
  ([coords] (seq (for [row coords
                      colum coords]
                    (conj [row] colum))))
  ([coords1 coords2] (seq (for [row coords1
                                colum coords2]
                            (conj [row] colum)))))

(defn block-coord [coord]
  (let [[x y] coord]
     (cond (< x 3) (cond (< y 3) (coord-pairs [0 1 2])
                         (< y 6) (coord-pairs [0 1 2] [3 4 5])
                         :else   (coord-pairs [0 1 2] [6 7 8]))

           (< x 6) (cond (< y 3) (coord-pairs [3 4 5] [0 1 2])
                         (< y 6) (coord-pairs [3 4 5])
                         :else   (coord-pairs [3 4 5] [6 7 8]))

           :else   (cond (< y 3) (coord-pairs [6 7 8] [0 1 2])
                         (< y 6) (coord-pairs [6 7 8] [3 4 5])
                         :else   (coord-pairs [6 7 8])))))

(defn block-values [board coord]
  (set (reduce (fn [a c] (conj a (value-at board c))) [] (block-coord coord))))

(defn valid-values-for [board coord]
  (if (= (value-at board coord) 0)
    (set/difference all-values (block-values board coord) (row-values board coord) (col-values board coord))
    #{}))

(defn filled? [board]
  (let [coords-in-board (coord-pairs [0 1 2 3 4 5 6 7 8])
        values-in-board (reduce (fn [a c] (conj a (value-at board c))) #{} coords-in-board)]
    (not (contains? values-in-board 0))))

(defn rows [board]
  (reduce (fn [a c] (conj a (row-values board [c 0]))) [] [0 1 2 3 4 5 6 7 8]))

(defn valid-rows? [board]
  (reduce (fn [x y] (and x y)) true (for [row (rows board)]
                                      (= (count row) 9))))

(defn cols [board]
  (reduce (fn [a c] (conj a (col-values board [0 c]))) [] [0 1 2 3 4 5 6 7 8]))

(defn valid-cols? [board]
  (reduce (fn [x y] (and x y)) true (for [col (cols board)]
                                      (= (count col) 9))))

(defn blocks [board]
  (reduce (fn [a c] (conj a (block-values board c))) [] (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (reduce (fn [x y] (and x y)) true (for [block (blocks board)]
                                      (= (count block) 9))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [all-coord (coord-pairs [0 1 2 3 4 5 6 7 8])]
    (loop [coord (first all-coord)
           rest-coord (rest all-coord)]
      (cond (empty? rest-coord) nil
            (= 0 (value-at board coord)) coord
            :else (recur (first rest-coord) (rest rest-coord))))))

(defn solve-helper [current-board]
  (if (filled? current-board)
    (if (valid-solution? current-board)
      [current-board]
      [])
    (let [next-point (find-empty-point current-board)
          test-seq (valid-values-for current-board next-point)]
      (for [value test-seq
            solution (solve-helper (set-value-at current-board next-point value))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
