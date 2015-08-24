(ns sudoku
  (:require [clojure.set :as set]))


(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})


(defn value-at [board coords]
  (get-in board coords))


(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))


(defn row-values [board [c _]]
  (set (get board c)))


(defn col-values [board [_ c]]
  (reduce conj #{} (map (fn [x] (value-at board [x c])) (range 9))))


(defn coord-pairs
  ([coords] (for [n1 coords
                  n2 coords]
              [n1 n2]))
  ([c1 c2] (for [n1 c1
                 n2 c2]
             [n1 n2])))


(defn top-left [coords]
  (map (fn [x] (* 3 (int (Math/floor (/ x 3))))) coords))


(defn block-values [board coords]
  (set
   (map (fn [z] (value-at board z))
        (apply coord-pairs
               (map
                (fn [x] (map
                         (fn [y] (+ x y))
                         (range 3)))
                (top-left coords))))))


(defn valid-values-for [board coords]
  (if (has-value? board coords)
    #{}
    (set/difference
     all-values
     (set/union
      (row-values board coords)
      (col-values board coords)
      (block-values board coords)))))


(defn filled? [board]
  (every? (fn [coords] (has-value? board coords)) (coord-pairs (range 9))))


(defn rows [board]
  (map set board))


(defn is-valid? [board sets]
  (every? (fn [a-set] (= a-set all-values)) sets))


(defn valid-rows? [board]
  (is-valid? board (rows board)))


(defn cols [board]
  (map (fn [x] (reduce conj #{} (col-values board [0 x]))) (range 9)))


(defn valid-cols? [board]
  (is-valid? board (cols board)))


(defn blocks [board]
  (map (fn [x] (block-values board x)) (coord-pairs (range 0 8 3))))


(defn valid-blocks? [board]
  (is-valid? board (blocks board)))


(defn valid-solution? [board]
  (and
   (valid-rows? board)
   (valid-cols? board)
   (valid-blocks? board)))


(defn set-value-at [board coords new-value]
  (assoc-in board coords new-value))


(defn find-empty-point [board]
  (some (fn [x] (if (has-value? board x) false x)) (coord-pairs (range 9))))


(defn find-empty-point-non-bt [board]
  (some (fn [coords]
          (if (not (has-value? board coords))
            (let [valid-values (valid-values-for board coords)]
              (if (= 1 (count valid-values))
                [coords (first valid-values)]
                false))
            false))
        (coord-pairs (range 9))))


; Non-backtracking branch. Sets a value in case there is only one that is valid.
(defn solve-non-bt [initial-board]
  (loop [board initial-board]
    (let [res (find-empty-point-non-bt board)]
      (if (nil? res)
        board
        (let [[coords value] res]
          (recur (set-value-at board coords value)))))))


; Backtracking branch. Try one solution and call solve.
; If this doesn't yield a valid solution, try another value.
(declare solve)
(defn solve-bt [initial-board target-coords]
  (loop [valid-values (set (valid-values-for initial-board target-coords))]
    (let [value (first valid-values)]
      (if (nil? value)
        nil ; No valid values left.
        (let [board (solve (set-value-at initial-board target-coords value))]
          (if (valid-solution? board)
            board
            (recur (disj valid-values value))))))))


(defn solve [initial-board]
  (let [board (solve-non-bt initial-board)
        coords (find-empty-point board)]
    (if (nil? coords)
      board
      (solve-bt board coords)))) ; Multiple possible solutions found; try one of them.

