(ns clj-chess-engine.core-test
  (:require [clojure.test :refer :all]
            [clj-chess-engine.core :refer :all]))

(def could-become-in-check-board ;; it's white turn
  [\r \- \b \q \k \b \n \r
   \p \p \p \p \- \p \p \p
   \- \- \n \- \- \- \- \-
   \- \- \- \- \p \- \- \Q
   \- \- \B \- \P \- \- \-
   \- \- \- \- \- \- \- \-
   \P \P \P \P \- \P \P \P
   \R \N \B \- \K \- \N \R])


(def in-check-board  ;; it's blacks turn, king is in check.
  [\r \- \b \q \- \b \n \r
   \p \p \p \p \k \Q \p \p
   \- \- \n \- \- \p \- \-
   \- \- \- \- \p \- \- \-
   \- \- \B \- \P \- \- \-
   \- \- \- \- \- \- \- \-
   \P \P \P \P \- \P \P \P
   \R \N \B \- \K \- \N \R])


(def check-mate-board  ;; it's blacks turn, king is in check. no move will save him => check mate
  [\r \- \b \q \k \b \n \r
   \p \p \p \p \- \Q \p \p
   \- \- \n \- \- \p \- \-
   \- \- \- \- \p \- \- \-
   \- \- \B \- \P \- \- \-
   \- \- \- \- \- \- \- \-
   \P \P \P \P \- \P \P \P
   \R \N \B \- \K \- \N \R])

(deftest test-filing
  (testing ""
    (is (= (file-component \b)
           1))))

(deftest test-rank
  (testing ""
    (is (= (rank-component \1)
           56))))

(deftest test-pos2coord
  (testing "convertion from algebraic notation to xy coordinates"
    (is (= (pos2coord "h3")
           [7 5]))))
(deftest test-pos2coord
  (testing "convertion from algebraic notation to xy coordinates"
    (is (= (pos2coord "a1")
           [0 7]))))
(deftest test-pos2coord
  (testing "convertion from algebraic notation to xy coordinates"
    (is (= (pos2coord "a7")
           [0 0]))))
(deftest test-pos2coord
  (testing "convertion from algebraic notation to xy coordinates"
    (is (= (pos2coord "e5")
           [4 3]))))
(deftest test-pos2coord
  (testing "convertion from algebraic notation to xy coordinates"
    (is (= (pos2coord "h8")
           [7 0]))))
(deftest test-pos2coord
  (testing "convertion from algebraic notation to xy coordinates"
    (is (= (pos2coord "h1")
           [7 7]))))

(deftest test-pos2coord
  (testing "convertion xy coordinates to algebraic notation"
    (is (= (coord2pos [7 7])
           "h1"))))
(deftest test-pos2coord
  (testing "convertion xy coordinates to algebraic notation"
    (is (= (coord2pos [0 7])
           "a1"))))
(deftest test-pos2coord
  (testing "convertion xy coordinates to algebraic notation"
    (is (= (coord2pos [7 0])
           "h8"))))
(deftest test-pos2coord
  (testing "convertion xy coordinates to algebraic notation"
    (is (= (coord2pos [4 3])
           "e5"))))

(deftest test-color
  (testing "test color"
    (is (= (is-white? \K)
           true))))
(deftest test-color
  (testing "test color"
    (is (= (is-white? \k)
           false))))
(deftest test-color
  (testing "test color"
    (is (= (is-white? \-)
           false))))
(deftest test-color
  (testing "test color"
    (is (= (is-black? \K)
           false))))
(deftest test-color
  (testing "test color"
    (is (= (is-black? \k)
           true))))
(deftest test-color
  (testing "test color"
    (is (= (is-black? \-)
           false))))

(deftest test-color
  (testing "test color"
    (is (= (is-piece? \-)
           false))))
(deftest test-color
  (testing "test color"
    (is (= (is-piece? \P)
           true))))
(deftest test-color
  (testing "test color"
    (is (= (is-piece? \k)
           true))))

(deftest test-opposite-collision
  (testing "test collision - white piece is about to move onto c1"
    (is (= (collid-oposite? (initial-board) true [2 7])
           false))))
(deftest test-opposite-collision
  (testing "test collision - black piece is about to move onto c1"
    (is (= (collid-oposite? (initial-board) false [2 7])
           true))))
(deftest test-self-collision
  (testing "test collision - white piece is about to move onto c8"
    (is (= (collid-self? (initial-board) true [2 0])
           false))))
(deftest test-self-collision
  (testing "test collision - black piece is about to move onto c8"
    (is (= (collid-self? (initial-board) false [2 0])
           true))))
(deftest test-self-collision
  (testing "test collision - black piece is about to move onto c3"
    (is (= (collid-self? (initial-board) false [2 2])
           false))))


(deftest test-self-collision
  (testing "test collision - white piece is about to move onto c1"
    (is (= (collid-self? (initial-board) true [2 7])
           true))))
(deftest test-self-collision
  (testing "test collision - black piece is about to move onto c1"
    (is (= (collid-self? (initial-board) false [2 7])
           false))))
(deftest test-self-collision
  (testing "test collision - white piece is about to move onto c8"
    (is (= (collid-self? (initial-board) true [2 0])
           false))))
(deftest test-self-collision
  (testing "test collision - black piece is about to move onto c8"
    (is (= (collid-self? (initial-board) false [2 0])
           false))))
(deftest test-self-collision
  (testing "test collision - black piece is about to move onto c3"
    (is (= (collid-self? (initial-board) false [2 5])
           false))))

(deftest test-any-collision
  (testing "test collision - piece is about to move onto"
    (is (= (collid? (initial-board) [2 5])
           false))))
(deftest test-any-collision
  (testing "test collision - piece is about to move onto"
    (is (= (collid? (initial-board) [2 6])
           true))))
(deftest test-any-collision
  (testing "test collision - piece is about to move onto"
    (is (= (collid? (initial-board) [2 1])
           true))))


(deftest test-lookup
  (testing "return piece Character from an algebraic notation given a board state - White Rook"
    (is (= (lookup (initial-board) "a1")
           \R))))
(deftest test-lookup
  (testing "White Queen"
    (is (= (lookup (initial-board) "d1")
           \Q))))
(deftest test-lookup
  (testing "White Pawn"
    (is (= (lookup (initial-board) "d2")
           \P))))
(deftest test-lookup
  (testing "Black King "
    (is (= (lookup (initial-board) "e8")
           \k))))
(deftest test-lookup
  (testing "empty square"
    (is (= (lookup (initial-board) "e3")
           \-))))

(deftest test-find-king-position
  (testing "find king"
    (is (= (king-pos check-mate-board false))
        "e8")))

(deftest test-all-possible-move-init
  (testing "that all the 20 possibilities (8 pawn x2 moves + 2 knights x2 moves) are found for the first move"
    (is (= (all-possible-moves (initial-board) true false)
           '(["h2" "h3"] ["h2" "h4"] ["g2" "g3"] ["g2" "g4"] ["f2" "f3"] ["f2" "f4"] ["g1" "f3"] ["g1" "h3"] ["e2" "e3"] ["e2" "e4"] ["d2" "d3"] ["d2" "d4"] ["c2" "c3"] ["c2" "c4"] ["b2" "b3"] ["b2" "b4"] ["a2" "a3"] ["a2" "a4"] ["b1" "c3"] ["b1" "a3"])))))

(deftest test-all-possible-move-with-in-check-init
  (testing "that all the 20 possibilities (8 pawn x2 moves + 2 knights x2 moves) are found for the first move"
    (is (= (all-possible-moves-with-in-check (initial-board) true false)
           '(["h2" "h3"] ["h2" "h4"] ["g2" "g3"] ["g2" "g4"] ["f2" "f3"] ["f2" "f4"] ["g1" "f3"] ["g1" "h3"] ["e2" "e3"] ["e2" "e4"] ["d2" "d3"] ["d2" "d4"] ["c2" "c3"] ["c2" "c4"] ["b2" "b3"] ["b2" "b4"] ["a2" "a3"] ["a2" "a4"] ["b1" "c3"] ["b1" "a3"])))))
(deftest test-all-possible-move-with-in-check
  (testing "black turn, only one move is allowed"
    (is (= (all-possible-moves-with-in-check in-check-board false false)
           '(["e7" "d6"])))))
(deftest test-all-possible-move-with-in-check
  (testing "black's turn, shouldn't be able to move pawn on f7 because it would put itself into check"
    (is (= (filter (fn [[from to]] (= from "f7")) (all-possible-moves-with-in-check could-become-in-check-test false false))
           '()))))
(deftest test-all-possible-move-with-in-check
  (testing "black's turn, no possibility"
    (is (= (all-possible-moves-with-in-check check-mate-board false false)
           '()))))

(deftest test-check-detection
  (testing "that a check is not detected"
    (is (= (check? (initial-board) false false)
           false))))
(deftest test-check-detection
  (testing "that a check is detected"
    (is (= (check? in-check-board false false)
           true))))
(deftest test-check-detection
  (testing "that a check is detected"
    (is (= (check? check-mate-board false false)
           true))))

(deftest test-check-mate-detection
  (testing "check-mate not detected"
    (is (= (check-mate? (initial-board) false false)
           false))))
(deftest test-check-mate-detection
  (testing "in check but check-mate not detected"
    (is (= (check-mate? in-check-board false false)
           false))))
(deftest test-check-mate-detection
  (testing "check-mate is detected"
    (is (= (check-mate? check-mate-board false false)
           true))))


(deftest a-check-mate-game
  (testing "playing a check mate scenario where white wins"
    (is (=
         (play-scenario  [["e2" "e4"] ["e7" "e5"]
                          ["d1" "h5"] ["d7" "d6"]
                          ["f1" "c4"] ["b8" "c6"]
                          ["h5" "f7"] ["e8" "e7"]])
         [[1 0] [["e2" "e4"] ["e7" "e5"]
                 ["d1" "h5"] ["d7" "d6"]
                 ["f1" "c4"] ["b8" "c6"]
                 ["h5" "f7"] ["e8" "e7"]]
          [\r \- \b \q \k \b \n \r
           \p \p \p \- \- \Q \p \p
           \- \- \n \p \- \- \- \-
           \- \- \- \- \p \- \- \-
           \- \- \B \- \P \- \- \-
           \- \- \- \- \- \- \- \-
           \P \P \P \P \- \P \P \P
           \R \N \B \- \K \- \N \R]
          false true]))))

(deftest an-invalid-move-game
  (testing "invalid move from black."
    (is (=
         (play-scenario  [["e2" "e4"] ["e7" "e5"]
                          ["d1" "h5"] ["d7" "d6"]
                          ["f1" "c4"] ["b8" "c6"]
                          ["h5" "g6"] ["e8" "d8"]])
         [[1 0] [["e2" "e4"] ["e7" "e5"]
                 ["d1" "h5"] ["d7" "d6"]
                 ["f1" "c4"] ["b8" "c6"]
                 ["h5" "g6"] ["e8" "d8"]]
          [\r \- \b \q \k \b \n \r
           \p \p \p \- \- \p \p \p
           \- \- \n \p \- \- \Q \-
           \- \- \- \- \p \- \- \-
           \- \- \B \- \P \- \- \-
           \- \- \- \- \- \- \- \-
           \P \P \P \P \- \P \P \P
           \R \N \B \- \K \- \N \R]
          true false]))))

(deftest cannot-move-into-check-case-game
  (testing "black cannot move into check mate but makes an invalid move."
    (is (=
         (play-scenario  [["e2" "e4"] ["e7" "e5"]
                          ["d1" "h5"] ["f7" "f6"]
                          ["f1" "c4"] ["b8" "c6"]
                          ["h5" "f7"] ["e8" "e7"]])
         [[1 0] [["e2" "e4"] ["e7" "e5"]
                 ["d1" "h5"] ["f7" "f6"]]
          [\r \n \b \q \k \b \n \r
           \p \p \p \p \- \p \p \p
           \- \- \- \- \- \- \- \-
           \- \- \- \- \p \- \- \Q
           \- \- \- \- \P \- \- \-
           \- \- \- \- \- \- \- \-
           \P \P \P \P \- \P \P \P
           \R \N \B \- \K \B \N \R]
          true false]))))
