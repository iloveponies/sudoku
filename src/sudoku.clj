(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})
(def coord-values (map dec all-values))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (contains? all-values (value-at board coord)))

(defn row-values [board coord]
  (let [row (first coord)]
    (set (get board row))))

(defn col-values [board coord]
  (let [col (last coord)]
    (set (map #(value-at board [% col]) coord-values))))

(defn coord-pairs [coords]
  (for [c1 coords
        c2 coords]
    [c1 c2]))

;; TODO: clean up
(defn block-values [board coord]
  (let [top-left (fn [c1 c2] (for [tl1 c1 tl2 c2] [tl1 tl2]))
        row (first coord)
        col (last coord)
        mr (- row (mod row 3))
        mc (- col (mod col 3))]
    (set (map #(value-at board %) (top-left (range mr (+ mr 3)) (range mc (+ mc 3)))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
     (set/difference all-values (set/union (row-values board coord)
                                           (col-values board coord)
                                           (block-values board coord)))))

(defn filled? [board]
  (loop [coords (for [c1 coord-values
                      c2 coord-values]
                  [c1 c2])]
    (cond
     (empty? coords)
       true
     (not (has-value? board (first coords)))
       false
     :else
       (recur (rest coords)))))

(defn rows [board]
  (map #(row-values board [% 0]) coord-values))

(defn valid-rows? [board]
  (= #{all-values} (set (rows board))))

(defn cols [board]
  (map #(col-values board [0 %]) coord-values))

(defn valid-cols? [board]
  (= #{all-values} (set (cols board))))

(defn blocks [board]
  (let [blox [[0 0] [0 3] [0 6] [3 0] [3 3] [3 6] [6 0] [6 3] [6 6]]]
    (map #(block-values board %) blox)))

(defn valid-blocks? [board]
  (= #{all-values} (set (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [row 0
         col 0]
    (let [last-coord (last coord-values)]
      (cond
       (and (= row last-coord) (= col last-coord))
         ()
       (zero? (value-at board [row col]))
         [row col]
       (= col last-coord)
         (recur (inc row) 0)
       :else
         (recur row (inc col))))))

(defn solve [board]
  (let [empty-coord (find-empty-point board)]
    (if (valid-solution? board)
      board
      (apply concat (map #(solve (set-value-at board empty-coord %)) (valid-values-for board empty-coord))))))
