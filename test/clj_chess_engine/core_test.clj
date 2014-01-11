(ns clj-chess-engine.core-test
  (:require [clojure.test :refer :all]
            [clj-chess-engine.core :refer :all]))

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
          true true]))))

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
