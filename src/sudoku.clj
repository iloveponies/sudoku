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
  (set (map
         (fn [row]
           (get row (second coord)))
         board)))

(defn coord-pairs [coords]
  (vec (apply concat (for [number coords]
    (map (fn [other] (vector number other)) coords)))))

(defn top-left-corner [coord]
  (vector
    (* 3 (int (/ (first coord) 3)))
    (* 3 (int (/ (second coord) 3)))))

(defn next-in-block [coord maxX]
  (cond
    (> (inc (second coord)) maxX) (vector (inc (first coord)) (- (second coord) 2))
    :else (vector (first coord) (inc (second coord)))))

(defn get-block-values [out-set in-board in-top-left curr]
  (if (not=
        (top-left-corner curr)
        in-top-left)
    out-set
    (recur
      (conj out-set (value-at in-board curr))
      in-board
      in-top-left
      (next-in-block curr (+ 2 (second in-top-left))))))

(defn block-values [board coord]
  (get-block-values #{} board (top-left-corner coord) (top-left-corner coord)))

(defn valid-values-for [board coord]
  (if (zero? (value-at board coord))
    (clojure.set/difference all-values
                          (block-values board coord)
                          (row-values board coord)
                          (col-values board coord))
    #{}))

(defn filled? [board]
  (loop [x 8
         b board]
    (cond
      (< x 0) true
      (contains? (row-values b (vector x 0)) 0) false
      :else (recur (dec x) b)
    )))

(defn rows [board]
  (reverse (loop [y 8
         b board
         out []]
    (cond
      (< y 0) out
      :else (recur
              (dec y)
              b
              (conj out (row-values b (vector y 0))))))))

(defn cols [board]
  (reverse (loop [x 8
         b board
         out []]
    (cond
      (< x 0) out
      :else (recur
              (dec x)
              b
              (conj out (col-values b (vector 0 x))))))))

(defn next-block-coord [coord]
  (let [left (vector (first coord) (- (second coord) 3))
        up   (vector (- (first coord) 3) 8)]
    (cond
    (>= (second left) 0) left
    :else up)))

(defn blocks [board]
  (reverse (loop [coord [8 8]
         b board
         out []]
    (cond
      (< (first coord) 0) out
      :else (recur
              (next-block-coord coord)
              b
              (conj out (block-values b coord)))))))

(defn valid-rows? [board]
  (loop [row 8
         b board]
    (cond
      (< row 0) true
      (not= (set (range 1 10))
            (row-values b (vector row 0))) false
      :else (recur (dec row) b))))

(defn valid-cols? [board]
  (loop [col 8
         b board]
    (cond
      (< col 0) true
      (not= (set (range 1 10))
            (col-values b (vector 0 col))) false
      :else (recur (dec col) b))))

(defn valid-blocks? [board]
  (loop [coord [8 8]
         b board]
    (cond
      (< (first coord) 0) true
      (not= (set (range 1 10))
            (block-values b coord)) false
      :else (recur
              (next-block-coord coord)
              b))))

(defn valid-solution? [board]
  (and (valid-cols? board)
       (valid-rows? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn next-coord [coord]
  (let [left (vector (first coord) (- (second coord) 1))
        up   (vector (- (first coord) 1) 8)]
    (cond
    (>= (second left) 0) left
    :else up)))

(defn find-empty-point [board]
  (loop [coord [8 8]]
    (cond
      (< (first coord) 0) nil
      (zero? (value-at board coord)) coord
      :else (recur (next-coord coord)))))


(def sud-board
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(defn solve [board]
  (let [empty-point (find-empty-point board)]
    (if (= nil empty-point)
      board
      (let [potentials (valid-values-for board empty-point)]
        (for [elem potentials
             solution (solve (set-value-at board empty-point elem))]
        solution)))))

(solve sud-board)
