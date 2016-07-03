(ns sudoku)
  ;;(:require [clojure.set :as set]))

(def board identity)

(def all-values
  (into #{} (range 1 10)))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= 0 (value-at board coord))
    false
    true))

(defn row-values [board [row col]]
  (set (value-at board [row])))

(defn col-values [board [row col]]
  (reduce #(conj %1 (get %2 col)) #{} board))

(defn coord-pairs
  ([rng]
   (coord-pairs rng rng))
  ([row-rng col-rng]
   (for [i row-rng
         j col-rng]
        [i j])))

(defn board-len [board]
  (count (first board)))

(defn block-size [board]
  (int (Math/sqrt (count (first board)))))

(defn sec-srch [board part-of-coord]
  (as-> (block-size board) _
        (partition _ _ (range (board-len board)))
        (keep #(if (some #{part-of-coord} %) %) _)
        (first _)
        (first _)))

(defn sector [board coords]
  (map #(sec-srch board %) coords))

(defn block-coords [board coord]
  (let [bs (block-size board)
        sec (sector board coord)
        sec-start-r (first sec)
        sec-start-c (second sec)]
    (coord-pairs (range sec-start-r (+ bs sec-start-r))
                 (range sec-start-c (+ bs sec-start-c)))))

(defn block-values [board coord]
  (->> (block-coords board coord)
       (reduce #(conj %1 (value-at board %2)) #{})))

(defn valid-values-for [board coord]
  (let [cell-val (value-at board coord)
        sec-vals (block-values board coord)
        row-vals (row-values board coord)
        col-vals (col-values board coord)]
    (if (pos? cell-val)
      #{}
      (clojure.set/difference all-values sec-vals row-vals col-vals))))

(defn filled? [board]
  (->> (map #(some #{0} %) board)
       (some #{0})
       (nil?)))

(defn- all-rows [board]
  (for [row (range (board-len board))]
       [row 0]))

(defn valid-group? [board group]
  (->> group
       (map #(clojure.set/difference all-values %))
       (every? empty?)))

(defn rows [board]
  (map #(row-values board %) (all-rows board)))

(defn valid-rows? [board]
  (->> (rows board)
       (valid-group? board)))

(defn- all-cols [board]
  (for [col (range (board-len board))]
       [0 col]))

(defn cols [board]
  (map #(col-values board %) (all-cols board)))

(defn valid-cols? [board]
  (->> (cols board)
       (valid-group? board)))

(defn blocks [board]
  (let [bs (block-size board)
        blen (board-len board)
        sectors (coord-pairs (range 0 blen bs))]
    (map #(block-values board %) sectors)))

(defn valid-blocks? [board]
  (->> (blocks board)
       (valid-group? board)))

(defn valid-solution? [board]
  (->> board
       ((juxt valid-rows? valid-cols? valid-blocks?))
       (every? true?)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [coords (coord-pairs (range 0 (board-len board)))]
    (->> coords
         (drop-while #(pos? (value-at board %)))
         (first))))

(defn solve [board]
  (let [loc (find-empty-point board)]
    (if (nil? loc)
      (if (valid-solution? board)
        board
        [])
      (for [valid-val (valid-values-for board loc)
            solution (solve (set-value-at board loc valid-val))]
           solution))))

