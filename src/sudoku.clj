(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board coord]
  (let [[row col] coord]
    (set (nth board row))))

(defn col-values [board coord]
  (let [[row col] coord]
    (set (map #(nth % col) board))))

(defn coord-pairs [coords]
  (for [row coords, col coords]
    [row col]))

(defn block-values [board coord]
  (let [[row col] coord
        top (* 3 (quot row 3))
        left (* 3 (quot col 3))]
    (set (for [r (range top (+ top 3))
              c (range left (+ left 3))]
          (value-at board [r c])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))))

(defn filled? [board]
  (-> (mapcat identity board)
      set
      (contains? 0)
      not
      ))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? #(= % all-values) (rows board)))

(defn cols [board]
  (->> (apply map vector board)
       (map set)))

(defn valid-cols? [board]
  (every? #(= % all-values) (cols board)))

(defn blocks [board]
  (->> (map #(partition 3 %) board)
       (partition 3)
       (mapcat #(apply map concat %))
       (map set)))

(defn valid-blocks? [board]
  (every? #(= % all-values) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (->> (for [r (range 9)
             c (range 9)]
         [r c])
       (filter #(not (has-value? board %)))
       first
       ))

(defn solve-helper [board]
  (if-let [ep (find-empty-point board)]
    (->> (valid-values-for board ep)
         (mapcat #(solve-helper (set-value-at board ep %))))
    (if (valid-solution? board)
      [board]
      nil
      )))

(defn solve [board]
  (first (solve-helper board)))
