(ns clj-chess-engine.core-test
  (:require [clojure.test :refer :all]
            [clj-chess-engine.core :refer :all]))

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

(deftest a-check-mate-test
  (testing "playing a check mate scenario."
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

(deftest a-invalid-move-test
  (testing "invalid move."
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

(deftest cannot-move-into-check-case-test
  (testing "cannot move into check mate."
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
