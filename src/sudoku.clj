(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= (value-at board coord) 0)
    false
    true))

(defn row-values [board coord]
  (set (get board (get coord 0))))

(defn col-values [board coord]
  (let [fun (fn [acc el]
                (conj acc (get el (get coord 1))))]
    (reduce fun #{} board)))

(defn coord-pairs [coords]
  (for [one coords
        two coords]
       (vector one two)))

(defn block-values [board coord]
  (let [topleft (fn [x]
                    (cond (<= (get x 0) 2)
                          (cond
                            (<= (get x 1) 2) (vector 0 0)
                            (and (> (get x 1) 2) (<= (get x 1) 5)) (vector 0 3)
                            :else (vector 0 6))
                    (and (> (get x 0) 2) (<= (get x 0) 5))
                          (cond
                            (<= (get x 1) 2) (vector 3 0)
                            (and (> (get x 1) 2) (<= (get x 1) 5)) (vector 3 3)
                            :else (vector 3 6))
                    :else (cond
                            (<= (get x 1) 2) (vector 6 0)
                            (and (> (get x 1) 2) (<= (get x 1) 5)) (vector 6 3)
                            :else (vector 6 6))))]
    (set (for [pairs (coord-pairs [0 1 2])]
              (value-at board (vector (+ (get pairs 0) (get (topleft coord) 0))
                                      (+ (get pairs 1) (get (topleft coord) 1))))))))

(defn valid-values-for [board coord]
  (if (contains? all-values (value-at board coord))
    #{}
    (set/difference all-values (row-values board coord) (col-values board coord)
     (block-values board coord))))

(defn filled? [board]
  (cond
    (contains? (set (get board 0)) 0) false
    (contains? (set (get board 1)) 0) false
    (contains? (set (get board 2)) 0) false
    (contains? (set (get board 3)) 0) false
    (contains? (set (get board 4)) 0) false
    (contains? (set (get board 5)) 0) false
    (contains? (set (get board 6)) 0) false
    (contains? (set (get board 7)) 0) false
    (contains? (set (get board 8)) 0) false
    :else true))

(defn rows [board]
  (vector
    (set (get board 0))
    (set (get board 1))
    (set (get board 2))
    (set (get board 3))
    (set (get board 4))
    (set (get board 5))
    (set (get board 6))
    (set (get board 7))
    (set (get board 8))))

(defn valid-rows? [board]
  (cond
    (not (= (get (rows board) 0) all-values)) false
    (not (= (get (rows board) 1) all-values)) false
    (not (= (get (rows board) 2) all-values)) false
    (not (= (get (rows board) 3) all-values)) false
    (not (= (get (rows board) 4) all-values)) false
    (not (= (get (rows board) 5) all-values)) false
    (not (= (get (rows board) 6) all-values)) false
    (not (= (get (rows board) 7) all-values)) false
    (not (= (get (rows board) 8) all-values)) false
    :else true))

(defn cols [board]
  (vector
    (set (col-values board [0 0]))
    (set (col-values board [0 1]))
    (set (col-values board [0 2]))
    (set (col-values board [0 3]))
    (set (col-values board [0 4]))
    (set (col-values board [0 5]))
    (set (col-values board [0 6]))
    (set (col-values board [0 7]))
    (set (col-values board [0 8]))))

(defn valid-cols? [board]
  (cond
    (not (= (get (cols board) 0) all-values)) false
    (not (= (get (cols board) 1) all-values)) false
    (not (= (get (cols board) 2) all-values)) false
    (not (= (get (cols board) 3) all-values)) false
    (not (= (get (cols board) 4) all-values)) false
    (not (= (get (cols board) 5) all-values)) false
    (not (= (get (cols board) 6) all-values)) false
    (not (= (get (cols board) 7) all-values)) false
    (not (= (get (cols board) 8) all-values)) false
    :else true))

(defn blocks [board]
  (vector
    (set (block-values board [1 1]))
    (set (block-values board [1 4]))
    (set (block-values board [1 7]))
    (set (block-values board [4 1]))
    (set (block-values board [4 4]))
    (set (block-values board [4 7]))
    (set (block-values board [7 1]))
    (set (block-values board [7 4]))
    (set (block-values board [7 7]))))

(defn valid-blocks? [board]
  (cond
    (not (= (get (blocks board) 0) all-values)) false
    (not (= (get (blocks board) 1) all-values)) false
    (not (= (get (blocks board) 2) all-values)) false
    (not (= (get (blocks board) 3) all-values)) false
    (not (= (get (blocks board) 4) all-values)) false
    (not (= (get (blocks board) 5) all-values)) false
    (not (= (get (blocks board) 6) all-values)) false
    (not (= (get (blocks board) 7) all-values)) false
    (not (= (get (blocks board) 8) all-values)) false
    :else true))

(defn valid-solution? [board]
  (if (and (valid-rows? board) (valid-cols? board) (valid-blocks? board))
    true
    false))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [a 0
         b 0]
        (if (= (has-value? board [a b]) false)
          [a b]
          (if (= a 8)
            (recur 0 (inc b))
            (recur (inc a) b)))))

(defn solve [board]
  (if (valid-solution? board)
    board
    (let [coord (find-empty-point board)
          remaining (valid-values-for board coord)]
      (for [ths remaining solution (solve (set-value-at board coord ths))]
           solution))))
