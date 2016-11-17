(ns sudoku
  (:require [clojure.set :as set]))

;; Board = Vector[Vector[Int]]
;; Coord = [Int, Int]
(def board identity)

;; The sudoku-brd hand-made :D
(def sudoku-brd
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(def solved-brd
(board [[5 3 4 6 7 8 9 1 2]
        [6 7 2 1 9 5 3 4 8]
        [1 9 8 3 4 2 5 6 7]
        [8 5 9 7 6 1 4 2 3]
        [4 2 6 8 5 3 7 9 1]
        [7 1 3 9 2 4 8 5 6]
        [9 6 1 5 3 7 2 8 4]
        [2 8 7 4 1 9 6 3 5]
        [3 4 5 2 8 6 1 7 9]]))

(def all-vals #{1 2 3 4 5 6 7 8 9})


;; value-at :: (Board, Coord) -> Int
;; Just use get-in.
(defn value-at [board coord]
  (get-in board coord))

;; Test cases
(value-at sudoku-brd [0 1]) ;;=> 3
(value-at sudoku-brd [0 0]) ;;=> 5


;; has-value? :: (Board, Coord) -> Boolean
;; Check the value returned by value-at with pos?.
(defn has-value? [board coord]
  (pos? (value-at board coord)))

;; Test cases
(has-value? sudoku-brd [0 0]) ;;=> true
(has-value? sudoku-brd [0 2]) ;;=> false


;; row-values :: (Board, Coord) -> Set[Int]
;; Destructure the row and use get.
(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

;; Test cases
(row-values sudoku-brd [0 2]) ;;=> #{0 5 3 7}
(row-values sudoku-brd [3 2]) ;;=> #{0 8 6 3}


;; col-values :: (Board, Coord) -> Set[Int]
;; Get the set with map by getting every col's value
(defn col-values [board coord]
  (let [[_ col] coord]
    (set (map (fn [x] (get x col)) board))))

;; Test cases
(col-values sudoku-brd [0 2]) ;;=> #{0 8}
(col-values sudoku-brd [4 8]) ;;=> #{3 1 6 0 5 9}


;; coord-pairs :: Vector[Int] -> Board
;; Vec the pairings made by for clause. The for clause makes pairs by joining
;; every two elements in a vector.
(defn coord-pairs [coords]
  (vec (for [fst coords
             snd coords]
    (vector fst snd))))

;; Test cases
(coord-pairs [0 1])   ;;=> [[0 0] [0 1]
                      ;;    [1 0] [1 1]]
(coord-pairs [0 1 2]) ;;=> [[0 0] [0 1] [0 2]
                      ;;    [1 0] [1 1] [1 2]
                      ;;    [2 0] [2 1] [2 2]]


;; block-values :: (Board, Coord) -> Set[Int]
;; First find the block corner with corresponding function. Then get all the
;; block coordinates with summing the corner into pairings. Finally get the
;; values into a set.
(defn block-values [board coord]
  (let [corner (map (fn [x] (- x (mod x 3))) coord)
        corner-coords (map (fn [x] (map + corner x)) (coord-pairs [0 1 2]))]
    (set (map (fn [x] (value-at board x)) corner-coords))))

;; Test cases
(block-values sudoku-brd [0 2]) ;;=> #{0 5 3 6 8 9}
(block-values sudoku-brd [4 5]) ;;=> #{0 6 8 3 2}


;; valid-values-for :: (Board, Coord) -> Set[Int]
;; Collect the functions with juxt. Then apply union to juxted board and coords.
;; Finally compute the difference with all-vals.
(defn valid-values-for [board coord]
  (let [funs (juxt row-values col-values block-values)
        used (apply set/union (funs board coord))]
    (if (has-value? board coord)
      #{}
      (set/difference all-vals used))))

;; Test cases
(valid-values-for sudoku-brd [0 0]) ;;=> #{}
(valid-values-for sudoku-brd [0 2]) ;;=> #{1 2 4}


;; filled? :: (Board, Coord) -> Boolean
;; Just sum the rows and then the sums and check whether it is 405.
(defn filled? [board]
  (let [sum (fn [x] (reduce + x))]
    (== 405 (sum (map sum board)))))

;; Test cases
(filled? sudoku-brd) ;;=> false
(filled? solved-brd) ;;=> true


(defn valifier [board fun]
  (let [valid? (fn [x] (= all-vals x))]
    (every? true? (map valid? (fun board)))))

;; rows :: Board -> Vector[Set[Int]]
;; Map every row to a set.
(defn rows [board]
  (vec (map set board)))

;; Test cases
(rows sudoku-brd) ;;=> [#{5 3 0 7}
                    ;;    #{6 0 1 9 5}
                    ;;    #{0 9 8 6}
                    ;;    #{8 0 6 3}
                    ;;    #{4 0 8 3 1}
                    ;;    #{7 0 2 6}
                    ;;    #{0 6 2 8}
                    ;;    #{0 4 1 9 5}
                    ;;    #{0 8 7 9}]

(rows solved-brd) ;;=> [#{1 2 3 4 5 6 7 8 9}
                    ;;    #{1 2 3 4 5 6 7 8 9}
                    ;;    #{1 2 3 4 5 6 7 8 9}
                    ;;    #{1 2 3 4 5 6 7 8 9}
                    ;;    #{1 2 3 4 5 6 7 8 9}
                    ;;    #{1 2 3 4 5 6 7 8 9}
                    ;;    #{1 2 3 4 5 6 7 8 9}
                    ;;    #{1 2 3 4 5 6 7 8 9}


;; valid-rows? :: Board -> Boolean
;; Check whether each row is valid and then check are all true (valid).
(defn valid-rows? [board]
  (valifier board rows))

;; Test Cases
(valid-rows? solved-brd)  ;;=> truthy
(valid-rows? sudoku-brd) ;;=> falsey


;; cols :: Board -> Vector[Set[Int]]
;; Map the rows of the transpose to set.
(defn cols [board]
  (let [transpose (partition (count board) (apply interleave board))]
    (vec (map set transpose))))

;; Test cases
(cols sudoku-brd) ;;=> [#{5 6 0 8 4 7}
                    ;;    #{3 0 9 6}
                    ;;    #{0 8}
                    ;;    #{0 1 8 4}
                    ;;    #{7 9 0 6 2 1 8}
                    ;;    #{0 5 3 9}
                    ;;    #{0 2}
                    ;;    #{0 6 8 7}
                    ;;    #{0 3 1 6 5 9}]

(cols solved-brd) ;;=> [#{1 2 3 4 5 6 7 8 9}
                    ;;    #{1 2 3 4 5 6 7 8 9}
                    ;;    #{1 2 3 4 5 6 7 8 9}
                    ;;    #{1 2 3 4 5 6 7 8 9}
                    ;;    #{1 2 3 4 5 6 7 8 9}
                    ;;    #{1 2 3 4 5 6 7 8 9}
                    ;;    #{1 2 3 4 5 6 7 8 9}
                    ;;    #{1 2 3 4 5 6 7 8 9}
                    ;;    #{1 2 3 4 5 6 7 8 9}]


;; valid-cols? :: Board -> Boolean
;; Check whether each column is valid and then check are all true (valid).
(defn valid-cols? [board]
  (valifier board cols))

;; Test cases
(valid-cols? solved-brd)  ;;=> truthy
(valid-cols? sudoku-brd) ;;=> falsey


;; blocks : Board -> Vector[Set[Int]]
;; Map block-values to every corner of blocks.
(defn blocks [board]
  (let [corners (coord-pairs [0 3 6])]
    (map (partial block-values board) corners)))

;; Test cases
(blocks sudoku-brd) ;;=> [#{5 3 0 6 9 8}
                      ;;    #{0 7 1 9 5}
                      ;;    #{0 6}
                      ;;    #{8 0 4 7}
                      ;;    #{0 6 8 3 2}
                      ;;    #{0 3 1 6}
                      ;;    #{0 6}
                      ;;    #{0 4 1 9 8}
                      ;;    #{2 8 0 5 7 9}]

(blocks solved-brd) ;;=> [#{1 2 3 4 5 6 7 8 9}
                      ;;    #{1 2 3 4 5 6 7 8 9}
                      ;;    #{1 2 3 4 5 6 7 8 9}
                      ;;    #{1 2 3 4 5 6 7 8 9}
                      ;;    #{1 2 3 4 5 6 7 8 9}
                      ;;    #{1 2 3 4 5 6 7 8 9}
                      ;;    #{1 2 3 4 5 6 7 8 9}
                      ;;    #{1 2 3 4 5 6 7 8 9}
                      ;;    #{1 2 3 4 5 6 7 8 9}])


;; valid-blocks? :: Board -> Boolean
;; Use valifier with blocks.
(defn valid-blocks? [board]
  (valifier board blocks))

;; Test cases
(valid-blocks? solved-brd)  ;;=> truthy
(valid-blocks? sudoku-brd) ;;=> falsey


;; valid-solution? :: Board -> Boolean
;; Apply all validy functions to board and check all are true.
(defn valid-solution? [board]
  (every? true? ((juxt valid-rows? valid-cols? valid-blocks?) board)))

;; Test cases
(valid-solution? solved-brd)  ;;=> truthy
(valid-solution? sudoku-brd) ;;=> falsey


;; set-value-at :: (Board, Coord, Int) -> Board
;; Just use assoc in with given parameters.
(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))


;; find-empty-point :: Board -> Board
;; First get all possible coordinate pairs with coord-pairs function. Then the
;; empty locations by removing filled ones. Finally just take the first one.
(defn find-empty-point [board]
  (let [coords (coord-pairs [0 1 2 3 4 5 6 7 8])
        empties (remove (partial has-value? board) coords)]
    (first empties)))


;; solve-helper :: Board -> Sequence[Board]
;; First check whether the board is filled. In the case of valid solutions,
;; return it. If not filled start filling empty points with suitable values
;; and with brute force find the correct combination.
(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board) [board] [])
    (let [point (find-empty-point board)
          values (valid-values-for board point)]
      (for [elem values
            solution (solve-helper (set-value-at board point elem))]
        solution))))

;; solve :: Board -> Board
;; Take the first solution given by solve-helper.
(defn solve [board]
  (first (solve-helper board)))
