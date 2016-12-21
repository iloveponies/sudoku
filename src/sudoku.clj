(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(def su-board
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

 [#{0 3 5 6 8 9}
  #{0 1 5 7 9}
  #{0 6}
  #{0 4 7 8}
  #{0 2 3 6 8}
  #{0 1 3 6}
  #{0 6}
  #{0 1 4 8 9}
  #{0 2 5 7 8 9}]

(def sol-board
  (board [[5 3 4 6 7 8 9 1 2]
          [6 7 2 1 9 5 3 4 8]
          [1 9 8 3 4 2 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 3 7 9 1]
          [7 1 3 9 2 4 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [3 4 5 2 8 6 1 7 9]]))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (loop [acc #{}
         row 0
         col (get coord 1)
         b board]
    (if (= row 9)
      acc
      (recur (conj acc (value-at b [row col]))
             (inc row) col b))))

(defn coord-pairs [coords]
  (for [num1 coords
        num2 coords]
    [num1 num2]))

(defn get-distance [cord1 cord2]
  (let [x1 (nth cord1 0)
        y1 (nth cord1 1)
        x2 (nth cord2 0)
        y2 (nth cord2 1)
        mx (- x1 x2)
        my (- y1 y2)
        xx (* mx mx)
        yy (* my my)]
      (Math/sqrt (+ xx yy))
    ))

(def block-distance (get-distance [0 0][1 1]))

(defn in-block? [dist]
  (<= dist block-distance))

(defn check-in-block? [cord1 cord2]
  (in-block? (get-distance cord1 cord2)))

(def middle-values [[1 1]
                    [1 4]
                    [1 7]
                    [4 1]
                    [4 4]
                    [4 7]
                    [7 1]
                    [7 4]
                    [7 7]])


(defn middle-cord [cord]
  (loop [mc middle-values
         c cord]
    (let [fc (first mc)]
      (if (in-block? (get-distance c fc))
        fc
        (recur (rest mc) c))
      )
    )
  )

(defn get-block-cords [mc]
  (loop [match-cords '()
         midcord mc
         all-cords (coord-pairs (range (- (apply min midcord) 1)  (+ (apply max midcord) 2)))]
    (if (empty? all-cords)
      match-cords
      (if (check-in-block? midcord (first all-cords))
        (recur (conj match-cords (first all-cords)) midcord (rest all-cords))
        (recur match-cords midcord (rest all-cords))
      ))
  ))

(defn block-values [board coord]
  (loop [acc #{}
         c (get-block-cords (middle-cord coord))
         b board]
    (if (empty? c)
      acc
      (recur
        (conj acc (value-at b (first c)))
        (rest c)
        b))))

(defn valid-values-for [board coord]
  (let [bv (block-values board coord)
        rv (row-values board coord)
        cv (col-values board coord)
        all (set/union cv (set/union bv rv))]
    (if (has-value? board coord)
      #{}
      (set (filter (fn [n] (not (contains? all n))) all-values)))))

(valid-values-for su-board [0 0]) ;=> #{}
(valid-values-for su-board [0 2]) ;=> #{1 2 4})


(defn filled? [board]
  (loop [mv middle-values
         b board]
    (cond
      (empty? mv) true
      (contains? (block-values b (first mv)) 0) false
      :else (recur (rest mv) b)
    )))

(defn rows [board]
  (loop [sets '()
         row 8
         col 0
         b board]
    (if (= row -1)
      sets
      (recur (conj sets (row-values b [row col]))
             (dec row)
             col
             b))))

(defn cols [board]
  (loop [sets '()
         row 0
         col 8
         b board]
    (if (= col -1)
      sets
      (recur (conj sets (col-values b [row col]))
             row
             (dec col)
             b))))

(defn blocks [board]
  (loop [sets '()
         mv (reverse middle-values)
         b board]
    (if (empty? mv)
      sets
      (recur (conj sets (block-values b (first mv)))
             (rest mv)
             b)
    )))

(defn valid-set? [a-set]
  (= all-values a-set))

(defn valid-rows? [board]
  (every? identity (map valid-set? (rows board))))

(defn valid-cols? [board]
  (every? identity (map valid-set? (cols board))))

(defn valid-blocks? [board]
  (every? identity (map valid-set? (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
 (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [r 0
         c 0
         b board]
    (cond (= r 9) (recur 0 (inc c) b)
          (not (has-value? b [r c])) [r c]
          :else (recur (inc r) c b))))

(defn solver [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      [])
    (let [e (find-empty-point board)
          valids (valid-values-for board e)]
      (for [v valids
            sol (solver (set-value-at board e v))]
        sol
        ))))

(defn solve [board]
  (first (solver board)))

(def v [[0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0]])


