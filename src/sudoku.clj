(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board [row col]]
  (get-in board [row col]))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board [row col]]
  (set (get board row)))

(defn col-values [board [row col]]
  (set (map #(get % col) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board [row col]]
  (let [block-row (* 3 (quot row 3))
        block-col (* 3 (quot col 3))
        coords (for [r (range block-row (+ block-row 3))
                     c (range block-col (+ block-col 3))]
                 [r c])]
    (set (map #(value-at board %) coords))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (clojure.set/difference
     (set (range 1 10))
     (row-values board coord)
     (col-values board coord)
     (block-values board coord))))

(defn filled? [board]
  (let [flat-board (apply concat board)
        all-coords (coord-pairs (range 9))]
    (every? #(has-value? board %) all-coords)))

(defn -valid-numbers? [ns]
  (= (set ns) (set (range 1 10))))

(defn rows [board]
  (map #(row-values board [% 0]) (range 9)))

(defn valid-rows? [board]
  (every? -valid-numbers? (rows board)))

(defn cols [board]
  (map #(col-values board [0 %]) (range 9)))

(defn valid-cols? [board]
  (every? -valid-numbers? (cols board)))

(defn blocks [board]
  (map #(block-values board %) (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (every? -valid-numbers? (blocks board)))

(defn valid-solution? [board]
  (and
   (filled? board)
   (valid-cols? board)
   (valid-rows? board)
   (valid-blocks? board)))

(defn set-value-at [board [row col] new-value]
  (assoc-in board [row col] new-value))

(defn find-empty-points [board]
  (filter #(not (has-value? board %)) (coord-pairs (range 9))))

(defn find-empty-point [board]
  (first (find-empty-points board)))

(defn -sort-by-num-valid-values
  [board locs]
  (let [num-values (map #(count (valid-values-for board %)) locs)
        locs-nums (map list locs num-values)
        locs-nums-sorted (sort-by second locs-nums)]
    (map first locs-nums-sorted)))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      nil)
    (let [free-locs-unsorted (find-empty-points board)
          free-locations (-sort-by-num-valid-values board free-locs-unsorted)]
      (loop [free-locs free-locations]
        (let [free-location (first free-locs)
              valid-values (valid-values-for board free-location)
              solution (loop [valid-values valid-values]
                        (let [solution (solve (set-value-at board free-location (first valid-values)))]
                          (if solution
                            solution
                            (recur (rest valid-values)))))]
        (if solution
          solution
          (recur (rest free-locs))))))))



;; (def sudoku-board
;;   (board [[5 3 0 0 7 0 0 0 0]
;;           [6 0 0 1 9 5 0 0 0]
;;           [0 9 8 0 0 0 0 6 0]
;;           [8 0 0 0 6 0 0 0 3]
;;           [4 0 0 8 0 3 0 0 1]
;;           [7 0 0 0 2 0 0 0 6]
;;           [0 6 0 0 0 0 2 8 0]
;;           [0 0 0 4 1 9 0 0 5]
;;           [0 0 0 0 8 0 0 7 9]]))

;; (defn -main [& args]
;;   (do
;;     (println "Solving...")
;;     (println "Result: \n" (apply str (interpose  "\n" (solve sudoku-board))))))


