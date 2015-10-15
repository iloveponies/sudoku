(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (let [helper (fn [values elem]
                (conj values (get elem (second coord))))]
    (reduce helper #{} board)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-values [board coord]
  (let [block-corner [(* 3 (int (/ (first coord) 3)))
                      (* 3 (int (/ (second coord) 3)))]
        rel-coord-pairs (coord-pairs [0 1 2])
        rel-to-abs-pos (fn [x]
                         [(+ (first x) (first block-corner))
                          (+ (second x) (second block-corner))])
        abs-coord-pairs (map rel-to-abs-pos rel-coord-pairs)
        helper (fn [values elem]
                 (conj values (value-at board elem)))]
    (reduce helper #{} abs-coord-pairs)))

(defn valid-values-for [board coord]
  (if (or (nil? coord) (has-value? board coord))
    #{}
    (set/difference
     all-values
     (col-values board coord)
     (row-values board coord)
     (block-values board coord))))

(defn get-all-values [board]
  (let [row-range (range 0 9)
        rows (map (fn [x] (row-values board [x 0])) row-range)
        helper (fn [a-set el]
                 (set/union a-set el))]
    (reduce helper #{} rows)))

(defn filled? [board]
  (not (contains? (get-all-values board) 0)))

(defn rows [board]
  (map (fn [x] (row-values board [x 0])) (range 0 9)))

(defn valid-rows? [board]
  (loop [all-rows (rows board)]
    (cond
     (empty? all-rows)
       true
     (not (empty? (set/difference all-values (first all-rows))))
       false
     :else
       (recur (rest all-rows)))))

(defn cols [board]
  (map (fn [x] (col-values board [0 x])) (range 0 9)))

(defn valid-cols? [board]
  (loop [all-cols (cols board)]
    (cond
     (empty? all-cols)
       true
     (not (empty? (set/difference all-values (first all-cols))))
       false
     :else
       (recur (rest all-cols)))))

(defn blocks [board]
  (let [block-corners (coord-pairs [0 3 6])]
    (map
     (fn [x] (block-values board [(first x) (second x)]))
     block-corners)))

(defn valid-blocks? [board]
  (loop [all-blocks (blocks board)]
    (cond
     (empty? all-blocks)
       true
     (not (empty? (set/difference all-values (first all-blocks))))
       false
     :else
       (recur (rest all-blocks)))))

(defn valid-solution? [board]
  (let [helper (fn [e1 e2]
            (and e1 e2))]
    (reduce helper [(valid-rows? board)
                    (valid-cols? board)
                    (valid-blocks? board)])))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [all-coords (coord-pairs (range 0 9))]
    (loop [crds all-coords]
      (cond
       (empty? crds)
         nil
       (not (has-value? board (first crds)))
         (first crds)
       :else
         (recur (rest crds))))))

(defn solve-helper [board coord valid-nums]
  (if (filled? board)
    (if (valid-solution? board)
      board
      '())
    (if (not (empty? valid-nums))
      (for [num valid-nums]
        (let [brd (set-value-at board coord num)
              empty-point (find-empty-point brd)
              vlds (valid-values-for brd empty-point)
              solution (solve-helper brd empty-point vlds)]
            solution)))))

(defn solve [board]
  (if (filled? board)
    board
    (let [empty-point (find-empty-point board)
          valid-nums (valid-values-for board empty-point)
          solution (solve-helper board empty-point valid-nums)]
      (loop [sol solution]
        (cond
         (vector? (first sol))
           (first sol)
         :else
           (recur (apply concat sol)))))))
