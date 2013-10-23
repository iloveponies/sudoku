(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (contains? all-values (value-at board coord)))

(defn row-values [board coord]
  (let [[row col] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[row col] coord]
    (set (map (fn [rowvals] (get rowvals col))
              board))))

(defn cartesian-product [aset bset]
  (reduce (fn [acc a] (concat acc (map (fn [b] [a b]) bset)))
          () aset))

(defn coord-pairs [coords]
  (cartesian-product coords coords))

(defn block-values [board coord]
  (let [[row col] (map #(* 3 (int (/ % 3))) coord)]
    (set (reduce (fn [acc rowvals] (concat acc (subvec rowvals col (+ col 3))))
                 () (subvec board row (+ row 3))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
      #{}
      (set/difference all-values
        (reduce (fn [acc f] (set/union acc (f board coord)))
                #{} [block-values row-values col-values]))))

(defn filled? [board]
  (every? (fn [rowvals] (every? #(contains? all-values %) rowvals))
          board))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? #(= % all-values) (rows board)))

(defn cols [board]
  (map (fn [col] (set (map #(get % col) board)))
       (range 0 (count (first board)))))

(defn valid-cols? [board]
  (every? #(= % all-values) (cols board)))

(defn blocks [board]
  (let [all (vec (reduce concat board))]
    (reduce (fn [acc i]
              (let [row (int (/ i 9)) col (mod i 9)
                    rowbk (int (/ row 3))
                    colbk (int (/ col 3))
                    allbk (+ colbk (* 3 rowbk))]
                (assoc acc allbk (conj (get acc allbk) (get all i)))))
            (vec (repeat (/ (count all) 9) #{}))
            (range 0 (count all)))))

(defn valid-blocks? [board]
  (every? #(= % all-values) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (let [[row col] coord]
    (assoc board row (assoc (get board row) col new-value))))

(defn find-empty-point [board]
  (some (fn [row] (some (fn [col]
                          (let [coord [row col]]
                            (when (= 0 (value-at board coord))
                              coord)))
                        (range 0 9)))
        (range 0 9)))

(defn solve [board]
  (let [coord (find-empty-point board)]
    (cond coord
            (some (fn [value] (solve (set-value-at board coord value)))
              (valid-values-for board coord))
          (valid-solution? board)
            board
          :else
            nil)))
