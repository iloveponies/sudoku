(ns sudoku
  (:require [clojure.set :as set]))

(defn segment-vec [a-vec n]
  (loop [acc   []
         xs    a-vec]
    (cond
      (empty? xs) acc
      (<= (count xs) n) (conj acc xs)
      :else             (recur (conj acc (subvec xs 0 n)) (subvec xs n)))))

(defn row->string [row]
  (let [segs  (segment-vec row 3)
        lanes (mapv (partial apply str) segs)]
    (clojure.string/join "|" lanes)))

(defn board->string [board]
  (let [rows  (mapv row->string board)
        lines (mapv #(str % \newline) rows)
        segs  (segment-vec lines 3)
        lanes (mapv clojure.string/join segs)
        sep   (str "---+---+---" \newline)]
    (clojure.string/join sep lanes)))

(defn print-board [board]
  (println (board->string board)))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (set (map #(get % (second coord)) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-coords [coord]
  (let [top  (* 3 (quot (first coord) 3))
        left (* 3 (quot (second coord) 3))]
    (for [row (range top (+ top 3))
          col (range left (+ left 3))]
      [row col])))

(defn block-values [board coord]
  (set (map #(value-at board %) (block-coords coord))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (clojure.set/difference all-values
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))))

(defn filled? [board]
  (every? #(has-value? board %) (coord-pairs (range 9))))

(defn rows [board]
  (for [row (range 9)]
    (set (row-values board [row 0]))))

(defn valid-rows? [board]
  (every? #(= all-values %) (rows board)))

(defn cols [board]
  (for [col (range 9)]
    (set (col-values board [0 col]))))

(defn valid-cols? [board]
  (every? #(= all-values %) (cols board)))

(defn blocks [board]
  (for [row (range 0 9 3)
        col (range 0 9 3)]
    (set (block-values board [row col]))))

(defn valid-blocks? [board]
  (every? #(= all-values %) (blocks board)))

(defn valid-solution? [board]
  (and
    (valid-rows? board)
    (valid-cols? board)
    (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first
    (for [coord  (coord-pairs (range 9))
          :when  (not (has-value? board coord))]
      coord)))

(defn- solve-helper [boards]
  (let [filled (filter filled? boards)
        solved (first (filter valid-solution? filled))]
    (if (not (nil? solved))
      solved
      (recur
        (for [incomplete (filter (complement filled?) boards)
              coord      [(find-empty-point incomplete)]
              value      (valid-values-for incomplete coord)
              solution   [(set-value-at incomplete coord value)]]
          solution)))))

(defn solve [board]
  (solve-helper [board]))
