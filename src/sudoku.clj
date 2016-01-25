(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= (value-at board coord)0)
    false true))

(defn row-values [board coord]
  (let [[x y] coord]
    (set(get-in board [x]))))

(defn col-values [board coord]
  (let [[x y] coord]
    (set (map #(get-in board [% y])
              (range (count board))))))

(defn coord-pairs [coords]
  (for [x1 coords x2 coords]
    [x1 x2]))

;; This passes the test but doesn't feel right
(defn block-values [board coord]
  (let [[x y] coord
        u-l-corner [(quot x 3) (quot y 3)]
        u1 (* 3 (quot x 3))]
    (set (map #(value-at board %) (coord-pairs [u1 (inc u1) (inc (inc u1))])))))


(defn valid-values-for [board coord]
  (let [all-values #{1 2 3 4 5 6 7 8 9}]
  (if (true? (has-value? board coord)) #{}
      (set/difference all-values
                      (block-values board coord)
                      (row-values board coord)
                      (col-values board coord)))))


(defn filled? [board]
  (if (contains?
       (apply set/union (map set board)) 0) false true))

(defn rows [board]
  (mapv set board))
 
(defn valid-rows? [board]
  (if (= 81 (reduce + (map count (rows board)))) true false))

(defn cols [board]
  (mapv set (apply mapv vector board)))

(defn valid-cols? [board]
  (if (= 81 (reduce + (map count (rows board)))) true false))

(defn blocks [board]
  (let [gen-blocks-2d-array
        (->>
         board
         (mapv (juxt #(subvec % 0 3) #(subvec % 3 6) #(subvec % 6 9)))
         (apply mapv vector)
         (mapv (juxt #(subvec % 0 3) #(subvec % 3 6) #(subvec % 6 9)))
         (to-array-2d))]
    (into [](for [x [0 1 2]
           y [0 1 2]]
       (into #{}(flatten (aget gen-blocks-2d-array y x)))))))

(defn valid-blocks? [board]
  (if (= 81 (reduce + (map count (rows board)))) true false))

(defn valid-solution? [board]
  ((every-pred valid-cols? valid-rows? valid-blocks?)
   board))

(defn set-value-at [board coord new-value]
  (let [[x y] coord] 
    (assoc-in board [x y] new-value)))

(defn find-empty-point [board]
  (let [helper (fn[board coord-pairs]
                 (let [[x y] coord-pairs]
                   (cond (= 0 (value-at board coord-pairs))
                         coord-pairs
                         (<= x 8) (recur board [x (inc y)])
                         :else (recur board [(inc x) (inc y)]))))]
    (if-not (valid-solution? board)
      (helper board [0 0]))))

;; (defn find-empty-point-helper [board coord-pairs]
;;   (let [[x y] coord-pairs]
;;     (cond (= 0 (value-at board coord-pairs))
;;           coord-pairs
;;           (<= x 8) (find-empty-point-helper board [x (inc y)])
;;           :else (find-empty-point-helper board [(inc x) (inc y)]))))

(defn solve [board]
  nil)



