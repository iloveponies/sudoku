(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board [(get coord 0) (get coord 1)]))

(defn has-value? [board coord]
  (pos? (value-at board coord)))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (set (map (fn [x] (value-at board [x col])) (range 0 9)))))

(defn coord-pairs [coords]
  (for [arr1 coords arr2 coords] (vec [arr1 arr2])))

(defn block-values [board coord]
  (let [[row col] coord
        [block-top block-left] [(- row (mod row 3))(- col (mod col 3))]]
   (set (map (fn [x] (value-at board x)) 
      (for [x (range block-top  (+ block-top  3))
            y (range block-left (+ block-left 3))]
         (vec [x y]))))))

; valid values are calculated as follows:
; all values - block values - row values - block values
; todo: is clojure.set/difference allowed?
(defn valid-values-for [board coord]
 (cond
  (has-value? board coord)
   #{}
  :else
   (clojure.set/difference
     all-values
     (block-values board coord)
     (row-values board coord)
     (col-values board coord))))

(defn filled? [board]
  (zero? (count (filter zero? (flatten board)))))

(defn rows [board]
  (map (fn [x] (row-values board [x 0])) (range 0 9)))

; todo: this is a bit of a hack maybe
(defn valid-rows? [board]
  (= 9 (count (filter (fn [row] (= row all-values)) (rows board)))))

(defn cols [board]
  (map (fn [x] (col-values board [0 x])) (range 0 9)))

; todo: this is a bit of a hack maybe
(defn valid-cols? [board]
  (= 9 (count (filter (fn [col] (= col all-values)) (cols board)))))

(defn blocks [board]
  (let [corner-values [0 3 6]
        corner-coords (for [x corner-values
                            y corner-values] 
                        (vec [x y]))]
    (map (fn [x] (block-values board x)) corner-coords)))

; todo: this is a bit of a hack maybe
(defn valid-blocks? [board]
  (= 9 (count (filter (fn [block] (= block all-values)) (blocks board)))))

(defn valid-solution? [board]
  (and
    (valid-cols? board)
    (valid-rows? board)
    (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [first-zero-index 
        (count (take-while (complement zero?) (flatten board)))]
     (vec 
       ; [(mod first-zero-index 9)
       ; (int (/ first-zero-index 9))])))

       [(int (/ first-zero-index 9))
        (mod first-zero-index 9)])))


(defn solve [board]
  ;check if board is full
  (if (filled? board)
  ; is the solution valid?
     (if (valid-solution? board)
  ;  yes: return solution
       board
  ;  no: return empty sequence
       '())
  ; board not full: select empty location
    (let [empty-point (find-empty-point board)
          valid-values (valid-values-for board empty-point)]
    ; no valid values? return empty sequence 
    (if (zero? (count valid-values))
      '()
      ; solve with each valid value, return first not empty
      (first (filter (complement empty?) (map (fn [value]
              (solve (set-value-at board empty-point value)))
          valid-values)))))))

