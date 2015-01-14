(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def id identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board [r c]]
  (set (get board r)))

(defn col-values [board [r c]]
  (set (map (fn [row] (get row c)) board)))

(defn coord-pairs [coords]
  (for [r coords
        c coords]
    [r c]))

(defn d [x]
  (cond (<= x 2) 0
        (<= x 5) 3
        :else 6 ))

(defn block-values [board [x y]]
  (set (apply concat (map (fn [row] (take 3 (drop (d y) row))) 
                    (take 3 (drop (d x) board))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union 
                                 (row-values board coord)
                                 (col-values board coord)
                                 (block-values board coord)))))

(defn filled? [board]
  (every? id (map (fn [row] (not-any? zero? row)) board)))

(defn rows [board]
  (map set board))

(defn d? [list] (apply distinct? (remove zero? list)))

(defn valid-rows? [board]
  (every? id (map d? board)))

(defn cols [board]
  (map set (apply map vector board)))

(defn valid-cols? [board]
  (every? id (map d? (apply map vector board))))

(defn szip [as bs]
    (loop [sa-seq as
                    b-seq bs
                    have []]
          (if (or (empty? sa-seq) (empty? b-seq))
                  have
                  (recur (rest sa-seq)
                                      (rest b-seq)
                                      (conj have (conj (first sa-seq) (first b-seq)))))))

(defn encaps [as]
    (reduce (fn [have i] (conj have [i])) [] as))

(defn zip [a-seq & more]
    (reduce szip (encaps a-seq) more))

(defn azip [list] (apply zip list))

(defn strands [board]
  (apply concat (map azip [(take 3 board) (take 3 (drop 3 board)) (drop 6 board)])))

(defn li-blocks [board]
  (loop [trp (strands board)
         have []]
    (if (empty? trp)
      have
      (recur (drop 3 trp) 
             (conj have (apply concat (take 3 trp)))))))

(defn blocks [board]
  (map set (li-blocks board)))
       
(defn valid-blocks? [board]
  (every? id (map d? (li-blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [var-board]
  (loop [board var-board
         row 0]
    (if (empty? board)
      nil
      (let [col (.indexOf (first board) 0)]
        (if (not= -1 col)
          [row col]
          (recur (rest board) (inc row)))))))


(defn solve-helper [board]
  (if (filled? board)
    [board]
    (let [emp (find-empty-point board)]
      (for [nbr (valid-values-for board emp)
            brd (solve-helper (set-value-at board emp nbr))]
        brd))))

(defn solve [board]
  (first (solve-helper board)))
