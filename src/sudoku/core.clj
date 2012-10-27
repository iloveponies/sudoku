(ns sudoku.core
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board coord]
  (let [[row _] coord]
  (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (if (== col 0)
      (set (map first board))
      (recur (map rest board) [_ (dec col)]))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board coord]
  (let [[block-row block-col] (map #(* 3 (int (/ % 3))) coord)]
    (set (for [c (for [row [0 1 2] col [0 1 2]]
                   [(+ row block-row) (+ col block-col)])]
      (value-at board c)))))

(defn valid-values-for [board coord]
  (let [fs [row-values col-values block-values]]
    (if (has-value? board coord) #{}
      (clojure.set/difference 
       (set (range 1 10))
       (apply clojure.set/union 
              (map #(% board coord) fs))))))

(defn filled? [board]
  (empty? (filter #(= % 0) (apply concat board))))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (empty? (filter (fn [ns] (not= ns (set (range 1 10)))) (rows board))))

(defn cols [board]
  (if (empty? (first board)) []
   (cons (set (map first board)) (cols (map rest board)))))

(defn valid-cols? [board]
  (empty? (filter #(not= % (set (range 1 10))) (cols board))))

(defn blocks [board]
  (for [block-row [0 3 6]
        block-col [0 3 6]]
    (block-values board [block-row block-col])))

(defn valid-blocks? [board]
  (empty? (filter #(not= % (set (range 1 10))) (blocks board))))

(defn valid-solution? [board]
  (let [checks [valid-blocks? valid-cols? valid-rows?]]
    (empty? (filter #(not (% board)) checks))))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [ri 0
         ci 0
         row (first board)
         b (rest board)]
    (cond (= 0 (first row)) [ci ri]
          (empty? (rest row)) (if (empty? b) 
                                nil
                                (recur 0 (inc ci) (first b) (rest b)))
          :else (recur (inc ri) ci (rest row) b))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board) board [])
    (let [point (find-empty-point board)]
       (for [value (valid-values-for board point)
             solution (solve (set-value-at board point value))]
          solution))))