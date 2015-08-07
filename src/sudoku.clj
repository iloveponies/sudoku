(ns sudoku
  (:require [clojure.set :as set]))

(def all-values (set (range 1 10)))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn- nth-row [board row]
  (get board row))

(defn- nth-col [board col]
  (map #(get % col) board))

(defn has-value? [board coord]
  (contains? all-values (value-at board coord)))

(defn row-values [board coord]
  (let [[row col] coord]
    (set (nth-row board row))))

(defn col-values [board coord]
  (let [[row col] coord]
    (set (nth-col board col))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board coord]
  ; Perhaps there is a more elegant way to this..?
  (let [block-top-left (fn [coord] (map (fn [value] (* (quot value 3) 3)) coord))
        [top left] (block-top-left coord)]
    (set (for [row (range top (+ top 3))
               col (range left (+ left 3))]
           (value-at board [row col])))))

(defn valid-values-for [board coord]
  (if (zero? (value-at board coord))
    (set/difference all-values
                (row-values board coord)
                (col-values board coord)
                (block-values board coord))
    ; This might be overkill depending of how the solver actually operates :)
    #{}))

(defn filled? [board]
  (not-any? zero? (flatten board)))

(defn rows [board]
  (map set board))

(defn- valid-set? [set]
  (empty? (set/difference all-values set)))

(defn valid-rows? [board]
  (every? valid-set? (rows board)))

(defn cols [board]
  (map set (for [col (range 9)]
             (nth-col board col))))

(defn valid-cols? [board]
  (every? valid-set? (cols board)))

(defn blocks [board]
  (for [top (range 0 9 3)
        left (range 0 9 3)]
    (block-values board [top left])))

(defn valid-blocks? [board]
  (every? valid-set? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [coords (for [row (range 9)
                      col (range 9)]
                  [row col])]
    (let [coord (first coords)]
      (if (or (empty? coords) (zero? (value-at board coord)))
        coord
        (recur (rest coords))))))

(defn solve [board]
  nil)
