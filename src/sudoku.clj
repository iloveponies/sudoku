(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (== (value-at board coord) 0)
    false
    true))

(defn row-values [board coord]
  (let [[y x] coord]
    (loop [set #{}
           c 0]
      (if (< 8 c)
        set
        (recur (conj set (value-at board [y c])) (inc c))))))

(defn col-values [board coord]
  (let [[y x] coord]
    (loop [set #{}
           c 0]
      (if (< 8 c)
        set
        (recur (conj set (value-at board [c x])) (inc c))))))

(defn coord-pairs [coords]
  (reduce conj [] (for [n coords
                        m coords]
                    [n m])))

(defn block-values [board coord]
  (let [block (fn [vector v]
                (cond
                  (< v 3) (conj vector 0)
                  (< v 6) (conj vector 3)
                  :else (conj vector 6)))
        corner (reduce block [] coord)]
    (reduce conj #{}
            (for [xy (for [n (range (get corner 0) (+ (get corner 0) 3))
                           m (range (get corner 1) (+ (get corner 1) 3))]
              (value-at board xy)))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union (block-values board coord) (col-values board coord) (row-values board coord)))))

(defn filled? [board]
  (if (empty?
        (set/difference (set (reduce concat (for [r board] r))) all-values))
    true
    false))

(defn rows [board]
  (for [r board]
    (set r)))

(defn valid-rows? [board]
  (let [sset (set (for [r (rows board)]
                  (count r)))]
    (if (= 1 (count sset))
      (if (= 9 (first sset))
        true
        false)
      false)))

(defn cols [board]
  (loop [n 0
         v []]
    (if (< 8 n)
      v
      (recur (inc n) (conj v (reduce conj #{} (for [r board] (get r n))))))))

(defn valid-cols? [board]
  (let [sset (set (for [c (cols board)]
                  (count c)))]
    (if (= 1 (count sset))
      (if (= 9 (first sset))
        true
        false)
      false)))

(defn blocks [board]
  (reduce conj [] (for [y (range 3)
        x (range 3)]
    (block-values board [(* 3 y) (* 3 x)]))))

(defn valid-blocks? [board]
  (let [sset (set (for [b (blocks board)]
                  (count b)))]
    (if (= 1 (count sset))
      (if (= 9 (first sset))
        true
        false)
      false)))

(defn valid-solution? [board]
  (if (and (valid-rows? board) (valid-cols? board) (valid-blocks? board))
    true
    false))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [[row col] [0 0]
         r (first board)
         b (rest board)
         c 0]
      (if (== (get r c) 0)
        [row col]
        (if (< c 8)
          (recur [row (inc col)] r b (inc c))
          (recur [(inc row) 0] (first b) (rest b) 0)))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      '())
  (let [point (find-empty-point board)]
    (reduce conj [] (for [val (valid-values-for board point)
                          solved (solve (set-value-at board point val))]
                      solved)))))
