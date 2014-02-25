(ns sudoku
  (:require [clojure.set :as set])
  (:require [clojure.math.combinatorics :as combo]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn transpose [board]
  (apply mapv vector board))
(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= (value-at board coord) 0)
    false
    true))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (set (map #(get % col) board)))

(defn coord-pairs [coord]
  (for [r coord
        c coord]
    [r c]))

;; 0, 1, 2 => 0
;; 3, 4, 5 => 1
;; 6, 7, 8 => 2

;; 1, 2, 3 => 1
;; 4, 5, 6 => 2
;; 7, 8, 9 => 3

(defn block-values [board coord]
  (let [topleft (map #(* 3 (quot % 3)) coord)
        indices (for [c topleft] (map #(+ c %) (range 3)))
        block   (for [i (first indices), j (second indices)] [i j])]
    (into #{} (map #(value-at board %) block))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [used-values
          (reduce set/union #{}
                  (map #(% board coord)
                       [row-values col-values block-values]))]
      (set/difference all-values used-values))))

(defn filled? [board]
  (= (count (filter zero? (flatten board))) 0))

(defn rows [board]
  (map #(into #{} (get board %)) (range 9)))

(defn valid? [board vals]
  (every? #(= % all-values) vals))
(defn valid-rows? [board]
  (valid? board (rows board)))

(defn cols [board]
  (let [transposed (transpose board)]
    (rows transposed)))

(defn valid-cols? [board]
  (valid? board (cols board)))

(defn blocks [board]
  (let [block-coords [[0 0] [0 3] [0 6]
                      [3 0] [3 3] [3 6]
                      [6 0] [6 3] [6 6]]]
    (map (partial block-values board) block-coords)))

(defn valid-blocks? [board]
  (valid? board (blocks board)))

(defn valid-solution? [board]
  (every? true? (map #(% board) [valid-rows? valid-cols? valid-blocks?])))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [inc-coord (fn inc-coord [coord]
                    (cond
                     (and (= (first coord) 8) (= (second coord) 8)) [0 0]
                     (= (first coord) 8) [0 (second coord)]
                     (= (second coord) 8) [(first coord) 0]
                     :else [(first coord) (inc (second coord))]))]
    (loop [coord [0 0]]
      (cond
       (filled? board) nil
       (has-value? board coord) (recur (inc-coord coord))
       :else coord))))

(defn find-empty-points [board]
  (let [all-coords (coord-pairs (range 9))]
    (filter (complement (partial has-value? board)) all-coords)))

;; (defn solve-helper [board empty-points]
;;   (if (filled? board)
;;     (if (valid-solution? board) [board] [])
;;     (for [vv (valid-values-for board (first empty-point))
;;           solution (solve-helper (set-value-at board empty-point vv))]
;;       solution)))

(defn solve [board]
  nil)

;; (defn solve [board]
;;   (let [empty-points (find-empty-points board)
;;         valid-values (map (partial valid-values-for board) empty-points)
;;         potential-solutions (apply combo/cartesian-product valid-values)
;;         with-coords (for [ps potential-solutions]
;;                       (map-indexed #(vector (nth empty-points %1) %2) ps))]
;;     (loop [tries with-coords]
;;       (let [solution
;;             (reduce (fn [board [coord value]]
;;                       (set-value-at board coord value))
;;                     board (first tries))]
;;         (if (valid-solution? solution)
;;           solution
;;           (recur (rest tries)))))))

;;;;;;;;;;;;;;;;;


