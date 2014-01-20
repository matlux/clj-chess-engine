(ns clj-chess-engine.core
  (:require [clojure.math.numeric-tower :as math])
  (:import clojure.lang.PersistentVector))



(defn initial-board []
  [\r \n \b \q \k \b \n \r
   \p \p \p \p \p \p \p \p
   \- \- \- \- \- \- \- \-
   \- \- \- \- \- \- \- \-
   \- \- \- \- \- \- \- \-
   \- \- \- \- \- \- \- \-
   \P \P \P \P \P \P \P \P
   \R \N \B \Q \K \B \N \R])

(defn test-board1 []
  [\r \n \b \q \k \b \n \r
   \p \p \p \p \p \p \p \p
   \- \- \- \- \- \- \- \-
   \- \- \- \- \- \- \- \-
   \- \- \- \- \- \- \- \-
   \- \- \N \- \- \- \- \-
   \P \P \P \P \P \P \P \P
   \R \- \B \Q \K \B \N \R])

(defn test-board2 []
  [\r \n \- \q \k \b \n \r
   \p \- \p \p \p \p \p \p
   \- \- \- \- \- \- \- \-
   \- \K \- \- \- \- \- \-
   \- \p \- \- \- \- \- \-
   \- \- \- \b \- \- \- \-
   \P \- \P \- \- \P \P \P
   \R \- \B \Q \K \B \N \R])

(defn check-mate-test []  ;; it's blacks turn, king is in check. no move will save him => check mate
  [\r \- \b \q \k \b \n \r
   \p \p \p \p \- \Q \p \p
   \- \- \n \- \- \p \- \-
   \- \- \- \- \p \- \- \-
   \- \- \B \- \P \- \- \-
   \- \- \- \- \- \- \- \-
   \P \P \P \P \- \P \P \P
   \R \N \B \- \K \- \N \R])

(defn in-check-test []  ;; it's blacks turn, king is in check. no move will save him => check mate
  [\r \- \b \q \- \b \n \r
   \p \p \p \p \k \Q \p \p
   \- \- \n \- \- \p \- \-
   \- \- \- \- \p \- \- \-
   \- \- \B \- \P \- \- \-
   \- \- \- \- \- \- \- \-
   \P \P \P \P \- \P \P \P
   \R \N \B \- \K \- \N \R])

(defn could-become-in-check-test []  ;; it's blacks turn, king is in check. no move will save him => check mate
  [\r \- \b \q \k \b \n \r
   \p \p \p \p \- \p \p \p
   \- \- \n \- \- \- \- \-
   \- \- \- \- \p \- \- \Q
   \- \- \B \- \P \- \- \-
   \- \- \- \- \- \- \- \-
   \P \P \P \P \- \P \P \P
   \R \N \B \- \K \- \N \R])

(defn bug-test []  ;; it's blacks turn
  [\r \n \b \q \k \b \n \r
   \p \p \p \p \- \p \p \p
   \- \- \- \- \- \- \- \-
   \- \- \- \- \p \- \- \Q
   \- \- \B \- \P \- \- \-
   \- \- \- \- \- \- \- \-
   \P \P \P \P \- \P \P \P
   \R \N \B \- \K \- \N \R])



(def ^:dynamic *file-key* \a)
(def ^:dynamic *rank-key* \0)

(def white-turn true)
(def black-turn false)
(def black black-turn)
(def white white-turn)
(def last-move-was-invalid true)
(def last-move-was-valid false)
(def check-mate true)


(defn file-component [file]
  (- (int file) (int *file-key*)))

(defn rank-component [rank]
  (->> (int *rank-key*)
       (- (int rank))
       (- 8)
       (* 8)))

(defn- file2coord [file]
  {:post [(and (< % 8) (>= % 0))]}
  (file-component file))

;(file-coord \a)

(defn- rank2coord [rank]
  {:post [(and (< % 8) (>= % 0))]}
  (->> (int *rank-key*)
       (- (int rank))
       (- 8)))

;(rank-coord \1)

(defn- coord2file [^long x]
  {:pre [(and (< x 8) (>= x 0))]}
  (->> (int *file-key*)
       (+ x)
       char))

(defn- coord2rank [^long y]
  {:pre [(and (< y 8) (>= y 0))]}
  (->> (- (int *rank-key*) y)
       (+ 8)
       char))

;(coord2rank 5)

;(rank-coord \1)
(defn pos2coord [^String pos]
  (let [[file rank] pos
        x (file2coord file)
        y (rank2coord rank)]
    [x y]))

(defn coord2pos [[x y]]
  (let [
        file (coord2file x)
        rank (coord2rank y)]
    (str file rank)))

;(coord2pos [4 6])

(defn- index [file rank]
  (+ (file-component file) (rank-component rank)))

(defn lookup [^PersistentVector board ^String pos]
  (let [[file rank] pos]
    (board (index file rank))))
(defn lookup-xy [^PersistentVector board ^PersistentVector pos]
  (lookup board (coord2pos pos)))

;; (file-component \b)
;;(rank-component \1)
;; (lookup (initial-board) "e5")
;;=> \R
;;(lookup-xy (initial-board) [0 7])

;; ------------- all possible moves

(defn is-white? [^Character piece]
  (Character/isUpperCase piece))
(defn is-black? [^Character piece]
  (Character/isLowerCase piece))
(defn is-piece? [^Character piece]
  (Character/isLetter piece))


(defprotocol Piece
  (getMoves [this]))

;;(pos2coord "e5")

(defn- valid-move? [[x y]]
  (and (< x 8)
       (>= x 0)
       (< y 8)
       (>= y 0)
       ))

(defn collid-self? [board white-turn? coord]
  (if white-turn?
    (is-white? (lookup board (coord2pos coord)))
    (is-black? (lookup board (coord2pos coord)))))
(defn collid-oposite? [board white-turn? coord]
  (if white-turn?
    (is-black? (lookup board (coord2pos coord)))
    (is-white? (lookup board (coord2pos coord)))
    ))

(collid-oposite? (initial-board) true [2 7])


;(collid-self? (initial-board) true [0 7])

(defn- is-vertical? [[x1 y1] [x2 y2]]
  (zero? (- x1 x2)))



(defn- pos-between-vertical [[x1 y1] [x2 y2]]
  (let [[b1 b2] (if (= (.compareTo y2 y1) 1) [y1 y2] [y2 y1])]
      (for [a (range (inc b1) b2)] [x1 a])))

(defn- pos-between-xy [[x1 y1] [x2 y2]]
  {:pre [(let [absslop (math/abs (/ (- y2 y1) (- x2 x1)))]
           ;(println absslop)
           (or (= absslop 1)
               (= absslop 0)))]}
  (let [forward? (> (- x2 x1) 0)
          slop (/ (- y2 y1) (- x2 x1))
          [step a b] (if forward? [1 x1 y1] [-1 x2 y2])
         f (fn [x] [(+ a (* 1 x)) (+ b (* slop x))])]
      (map f (range 1 (math/abs(- x2 x1))))))

(defn- pos-between [p1 p2]
  (if (is-vertical? p1 p2)
    (pos-between-vertical p1 p2)
    (pos-between-xy p1 p2)))


(pos-between [0 0] [7 7])
(pos-between [0 0] [0 7])
(pos-between [2 2] [7 7])
(pos-between [0 0] [7 0])
(pos-between [7 7] [0 0])
(pos-between [0 7] [0 0])
(pos-between [7 0] [0 0])
(pos-between [7 7] [1 1])



(defn- nothing-between [board p1 p2]
  (not-any? is-piece? (map #(lookup-xy board %) (pos-between p1 p2))))

(nothing-between (initial-board) [0 0] [7 7])
(nothing-between (initial-board) [1 1] [6 6])
(nothing-between (initial-board) [2 0] [6 0])
(nothing-between (initial-board) [0 1] [0 6])
(nothing-between (initial-board) [0 0] [7 7])
(nothing-between (initial-board) [0 0] [7 7])
(nothing-between (initial-board) [0 0] [7 7])
(nothing-between (test-board2) [0 7] [1 7])

(defn collid? [board pos] (not (= (lookup-xy board pos) \-)))

(defn c2dto1d [v]
  (let [[x y] v]
    (clojure.core/+ x (clojure.core/* 8 y))))

(defn c1dto2d [i]
  (vector (int (/ i 8)) (mod i 8)))

(defn c1dto2d-xy [i]
  (vector (mod i 8) (int (/ i 8))))

(defn char2state [pieces-list]
  (into {} (filter #(not= \- (second %)) (map #(vector (c1dto2d %1) %2 ) (range 64) pieces-list))))

(defn board2xy-map-piece [pieces-list]
  (into {} (filter #(not= \- (second %)) (map #(vector (c1dto2d-xy %1) %2 ) (range 64) pieces-list))))


;; ((fn [pieces-list]
;;    (into {} (filter #(not= \- (second %)) (map #(vector (c1dto2d %1) %2 ) (range 64) pieces-list)))) (initial-board))




;;(collid? (initial-board) [1 6])

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

;;---- pawn moves
(defn- pawn-moves [board white? x y]
  (let [[op start-rank] (if white? [- 6] [+ 1])
        right-diag [(+ x 1) (op y 1)]
        left-diag [(- x 1) (op y 1)]
        front [x (op y 1)]
        front2 [x (op y 2)]
        moves [
               (when (and (valid-move? right-diag)
                          (collid-oposite? board white? right-diag)) right-diag)
               (when (and (valid-move? left-diag)
                          (collid-oposite? board white? left-diag)) left-diag)
               (when (not (collid? board front)) front)
               (when (and
                      (not (collid? board front))
                      (not (collid? board front2))
                      (= y start-rank)) front2)
               ]]
    (filter (comp not nil?) moves)))

;;(not (= (lookup-xy (initial-board) [2 1]) \-))
(pawn-moves (initial-board) false 1 1)




(count (pawn-moves (initial-board) true 2 2))
;(nothing-between (initial-board) )

(defrecord Pawn [^clojure.lang.PersistentVector board ^String pos ^boolean white?]
  Piece
  (getMoves [this]
    (let [[x y] (pos2coord pos)
          moves (pawn-moves board white? x y)
          no-self-collision? (comp not (partial collid-self? board white?))]
      (map coord2pos (filter no-self-collision? (filter valid-move? moves))))))


;;(getMoves (Pawn. (test-board2) "b3" false))
;; => ("c2" "a2" "b2")
;;(getMoves (Pawn. (test-board2) "a7" false))
;; =>("a6" "a5")
;;(getMoves (Pawn. (test-board2) "a6" false))
;; =>("b5" "a5")

;;---- King moves

(defn- king-moves [board x y]
  (for [a (range -1 2)
        b (range -1 2)
        :when (and
               (or (and (= (+ a b) 0)
                        (not (= a 0))
                        (not (= b 0))
                        )
                   (and (= (- a b) 0)
                        (not (= a 0))
                        (not (= b 0))
                        )
                   (and (= a 0) (not (= b 0)))
                   (and (= b 0) (not (= a 0))))
               (valid-move? [(+ a x) (+ b y)])
               ;(not (collid-self? board true [x y]))
               (nothing-between board [(+ a x) (+ b y)] [x y])
               )] [(+ a x) (+ b y)]))

(count (king-moves (initial-board) 2 2))
;(nothing-between (initial-board) )

(defrecord King [^PersistentVector board ^String pos ^boolean white?]
  Piece
  (getMoves [this]
    (let [[x y] (pos2coord pos)
          moves (king-moves board x y)
          no-self-collision? (comp not (partial collid-self? board white?))]
      (map coord2pos (filter no-self-collision? (filter valid-move? moves))))))


(getMoves (King. (test-board2) "d3" true))


;; --------- queen moves

(defn- queen-moves [board x y]
  (for [a (range -7 8)
        b (range -7 8)
        :when (and
               (or (and (= (+ a b) 0)
                        (not (= a 0))
                        (not (= b 0))
                        )
                   (and (= (- a b) 0)
                        (not (= a 0))
                        (not (= b 0))
                        )
                   (and (= a 0) (not (= b 0)))
                   (and (= b 0) (not (= a 0))))
               (valid-move? [(+ a x) (+ b y)])
               ;(not (collid-self? board true [x y]))
               (nothing-between board [(+ a x) (+ b y)] [x y])
               )] [(+ a x) (+ b y)]))

(count (queen-moves (initial-board) 2 2))
;(nothing-between (initial-board) )

(defrecord Queen [^PersistentVector board ^String pos ^boolean white?]
  Piece
  (getMoves [this]
    (let [[x y] (pos2coord pos)
          moves (queen-moves board x y)
          no-self-collision? (comp not (partial collid-self? board white?))]
      (map coord2pos (filter no-self-collision? (filter valid-move? moves))))))

(getMoves (Queen. (test-board2) "d3" true))
;;=> ("a3" "b3" "c4" "c3" "d7" "d6" "d5" "d4" "d2" "e4" "e3" "e2" "f5" "f3" "g6" "g3" "h7" "h3")

;;---- Rook

(defn- rook-moves [board x y]
  (for [a (range -7 8)
        b (range -7 8)
        :when (and
               (or (and (= a 0) (not (= b 0))) (and (= b 0) (not (= a 0))))
               (valid-move? [(+ a x) (+ b y)])
               ;(not (collid-self? board true [x y]))
               (nothing-between board [(+ a x) (+ b y)] [x y])
               )] [(+ a x) (+ b y)]))

(count (rook-moves (initial-board) 2 2))
;(nothing-between (initial-board) )

(defrecord Rook [^PersistentVector board ^String pos ^boolean white?]
  Piece
  (getMoves [this]
    (let [[x y] (pos2coord pos)
          moves (rook-moves board x y)
          no-self-collision? (comp not (partial collid-self? board white?))]
      (map coord2pos (filter no-self-collision? (filter valid-move? moves))))))


(getMoves (Rook. (test-board2) "c2" true))
(pos2coord "a1")
(rook-moves (test-board2) 0 7)
(nothing-between (test-board2) [1 7] [0 7])
(valid-move? [1 7])

;;---- bishop


(defn- bishop-moves [board x y]
  (for [a (range -7 8)
        b (range -7 8)
        :when (and
               (or (= (+ a b) 0) (= (- a b) 0))
               (not (= a 0))
               (not (= b 0))
               (valid-move? [(+ a x) (+ b y)])
               ;(not (collid-self? board true [x y]))
               (nothing-between board [(+ a x) (+ b y)] [x y])
               )] [(+ a x) (+ b y)]))

(count (bishop-moves (initial-board) 1 2))
;(nothing-between (initial-board) )

(defrecord Bishop [^PersistentVector board ^String pos ^boolean white?]
  Piece
  (getMoves [this]
    (let [[x y] (pos2coord pos)
          moves (bishop-moves board x y)
          no-self-collision? (comp not (partial collid-self? board white?))]
      (map coord2pos (filter no-self-collision? (filter valid-move? moves))))))


(getMoves (Bishop. (initial-board) "a3" true))
;;("b4" "c5" "d6" "e7")
(getMoves (Bishop. (test-board2) "c1" true))
;;=> ("a3" "b2" "d2" "e3" "f4" "g5" "h6")
(getMoves (Bishop. (test-board2) "a5" true))
;;=> ("b6" "b4" "c7")
(getMoves (Bishop. (test-board2) "e2" true))
;;("d3" "f3" "g4" "h5")




;; ---- knight stuff
(defn- knight-moves [x y]
  #{[(+ x 2) (+ y 1)]
    [(+ x 1) (+ y 2)]
    [(- x 1) (+ y 2)]
    [(- x 2) (+ y 1)]
    [(- x 2) (- y 1)]
    [(- x 1) (- y 2)]
    [(+ x 1) (- y 2)]
    [(+ x 2) (- y 1)]})

;;(knight-moves 0 0)



(defrecord Knight [^PersistentVector board ^String pos ^boolean white?]
  Piece
  (getMoves [this]
    (let [[x y] (pos2coord pos)
          kmoves (knight-moves x y)
          no-self-collision? (comp not (partial collid-self? board white?))]
      (map coord2pos (filter no-self-collision? (filter valid-move? kmoves))))))

;;(getMoves (Knight. (initial-board) "g1" true))
;; f3 g3
;;(pos2coord "g1")

;;(source comp)




(defn- is-knight? [ piece]
  (or (= piece \N)
      (= piece \n)))
(defn- is-bishop? [ piece]
  (or (= piece \B)
      (= piece \b)))
(defn- is-queen? [ piece]
  (or (= piece \Q)
      (= piece \q)))
(defn- is-king? [ piece]
  (or (= piece \K)
      (= piece \k)))
(defn- is-rook? [ piece]
  (or (= piece \R)
      (= piece \r)))
(defn- is-pawn? [ piece]
  (or (= piece \P)
      (= piece \p)))

(defn- one-color [^PersistentVector board ^Boolean white?]
  (let [color? (if white? is-white? is-black?)]
    (map #(if (color? %) % \- ) board)))

(one-color (initial-board) false)

(defn convert2obj [^PersistentVector board ^String pos]
  (let [piece (lookup board pos)
        color (is-white? piece)]
    (cond
     (is-knight? piece) (Knight. board pos color)
     (is-bishop? piece) (Bishop. board pos color)
     (is-queen? piece) (Queen. board pos color)
     (is-king? piece) (King. board pos color)
     (is-rook? piece) (Rook. board pos color)
     (is-pawn? piece) (Pawn. board pos color))))

(convert2obj (initial-board) "h1")

;;---- move validation



(defn possible-moves [^PersistentVector board ^String pos]
  (getMoves (convert2obj board pos)))
;; => #{"e4" "e3"}
(possible-moves (initial-board) "a2")

;; (defn all-possible-moves [board white-turn? castle?]
;;   (->> (one-color board white-turn?) board2xy-map-piece (map (fn [[pos-xy c]] (coord2pos pos-xy)))))

(defn all-possible-moves [^PersistentVector board ^Boolean white-turn? ^Boolean castle?]
  (->> (one-color board white-turn?)
       board2xy-map-piece
       (mapcat
        (fn [[pos-xy c]]
          (let [pos (coord2pos pos-xy)]
            (map
             (fn [move] [pos move])
             (possible-moves board pos)))))))

;;(filter #(is-piece? %))
;;(map (fn [[pos c]] (possible-moves board pos)))
;(all-possible-moves (initial-board) true false)
;(count (all-possible-moves (initial-board) true false))

(defn king-pos [board king-white?]
  (let [king (if king-white? \K \k)]
    (->> ((->>  (group-by #(second %) (board2xy-map-piece board)) (into {})) king) ffirst coord2pos)))

(king-pos (check-mate-test) false)

(defn check? [board white-king? castled?]
  (let [opposite-moves (all-possible-moves board (not white-king?) castled?)
        king (king-pos board white-king?)]
    (not (not (some #(= % king) (map #(second %) opposite-moves))))))

;;(check? (check-mate-test) false false)
(check? (initial-board) false false)
(check? (bug-test) false false)


;; -------------- rendering

;;(def ^:const board (vec (range 8)))


;;(c2dto1d [1 1])
;;(c1dto2d 63)

(defn render-board [board-state]
  (let [line "+---+---+---+---+---+---+---+---+"
        pieces-pos board-state ;(into {} board-state)
        ]
    (apply str "\n" line "\n"
           (map #(let [pos (c1dto2d (dec %))
                       c (get pieces-pos pos " ")]
                   (if (zero? (mod % 8))
                           (format "| %s |\n%s\n" c line)
                           (format "| %s " c))) (range 1 65)))))


(def init-board-state (char2state (initial-board)))


(defn display-board [board]
  (print (render-board (char2state board))))

;(display-board init-board-state)

;(display-board '([[1 0] "p"] [[2 2] "*"] [[0 1] "&"]))

;;----- change state of board (make moves)

(defn apply-move [^PersistentVector board ^PersistentVector [from to]]
  (let [piece (lookup board from)]
    (-> (assoc board (apply index from) \-)
        (assoc (apply index to) piece)
        )))

;;(apply index "b2")


;;(display-board (apply-move (initial-board) ["b2" "b3"]))
;;(display-board (-> (apply-move (initial-board) ["b2" "b3"]) (apply-move ["b1" "c3"]) (apply-move ["e2" "e4"])))
;;(display-board (apply-move (initial-board) ["b2" "b3"]))

(defn all-possible-moves-with-in-check [board white-turn? castle?]
  (let [possible-moves (all-possible-moves board white-turn? castle?)
        f (fn [move] (let [possible-new-board (apply-move board move)]
                      (not (check? possible-new-board white-turn? castle?))))]
    (filter f possible-moves)))

;(all-possible-moves-with-in-check (check-mate-test) false false)
;(all-possible-moves-with-in-check (in-check-test) false false)
;(filter (fn [[from to]] (= from "f7")) (all-possible-moves-with-in-check (could-become-in-check-test) false false))

;; todo: catch any exception
;; todo: check that any none valid input returns nil
(defn is-move-valid? [^PersistentVector board ^Boolean white-turn? ^Boolean castle? ^PersistentVector move]
  (let [;in-check? (check? board (not white-turn?) false)
        moves (all-possible-moves-with-in-check board white-turn? castle?)]
    (some #(= move %) moves)))

;;(is-move-valid? (initial-board) true false ["b2" "b3"])
;; => true
;;(is-move-valid? (initial-board) true false nil)
;; => false
;;(is-move-valid? (bug-test) false false ["b8" "a6"])
;;(check? (bug-test) true false)
;; (defmacro --> [m firstkey & keys]
;;   (let [a (map #(list 'get %) keys)]
;;     `(-> (~m ~firstkey) ~@a)))

(defn check-mate? [^PersistentVector board ^Boolean white-turn? ^Boolean castle?]
  (->> (all-possible-moves-with-in-check board white-turn? castle?) count zero?))
;(check-mate? (check-mate-test) false false)
;(check-mate? (in-check-test) false false)




(defn apply-move-safe  [^PersistentVector board ^Boolean white-turn? ^Boolean castle? ^PersistentVector move]
  (if (is-move-valid? board white-turn? castle? move) (apply-move (initial-board) move)))


(defn forfeit [white-turn?]
  (if white-turn? [0 1] [1 0]))
(def opposite-color-wins forfeit)

;;(display-board (apply-move-safe (initial-board) true false ["a2" "b3"]))
(defn- play-game-rec [board f1 f2 white-turn? white-castled? black-castled? move-history state-f1 state-f2]
  (if (check-mate? board white-turn? false)
      (do
        (println "check-mate!")
        [(opposite-color-wins white-turn?) move-history board :check-mate])
      (let [in-check? (check? board (not white-turn?) false)
         [[move new-state] castled?] (if white-turn?
                                       [(f1 board white-turn? white-castled? in-check? (first move-history) state-f1) white-castled?]
                                       [(f2 board white-turn? black-castled? in-check? (first move-history) state-f2) black-castled?])
         valid? (is-move-valid? board white-turn? false move)
         new-history (conj move-history move)
         cmate (check-mate? board white-turn? false)]
     (display-board board)
     (if (not valid?)
       [(forfeit white-turn?) new-history board :invalid-move]
       (if white-turn?
         (recur (apply-move board move) f1 f2 (not white-turn?) false false new-history new-state state-f2)
         (recur (apply-move board move) f1 f2 (not white-turn?) false false new-history state-f1 new-state))
       ))))

(defn play-game [board f1 f2]
  (play-game-rec board f1 f2 true false false [] nil nil)
  )
;; => [score move-history last-board invalid-move? check-mate?]
;;example => [[1 0] [["e2" "e4"] ["e7" "e5"]] [\- \- \- \k \- ....]]



(defn- every-nth [coll n]
  (map (fn [[i e]] e) (filter (fn [[i e]] (zero? (mod i n))) (map-indexed (fn [i e] [i e]) coll))))

(defn- create-fn [moves]
  (fn [board am-i-white? have-i-castled? in-check? last-move option-state]
      (let [move-seq (if (nil? option-state)
                       moves
                       option-state)]
        [(first move-seq) (next move-seq)])))

(defn create-fns-from-scenario [moves]
  (let [white-moves (every-nth moves 2)
        black-moves (every-nth (drop 1 moves) 2)]
    [(create-fn white-moves)
     (create-fn black-moves)]))



(defn play-scenario [scenario] (let [[f1 f2] (create-fns-from-scenario scenario)]
   (play-game (initial-board) f1 f2)))


;;(play-scenario  [["e2" "e4"] ["e7" "e5"] ["d1" "h5"] ["d7" "d6"] ["f1" "c4"] ["b8" "c6"] ["h5" "f7"] ["e8" "e7"]])
;; => check-mate
;;(play-scenario  [["e2" "e4"] ["e7" "e5"] ["d1" "h5"] ["d7" "d6"] ["f1" "c4"] ["b8" "c6"] ["h5" "g6"] ["e8" "e7"]])
;; => invalid move

(defn f1 [board am-i-white? have-i-castled? in-check? last-move option-state]
                            (let [move-seq (if (nil? option-state)
                                             (list ["e2" "e4"] ["d1" "h5"] ["f1" "c4"] ["h5" "f7"])
                                             option-state)]
                              [(first move-seq) (next move-seq)]))

(defn f2 [board am-i-white? have-i-castled? in-check? last-move option-state]
  (let [b board
        move-seq (if (nil? option-state)
           (list ["e7" "e5"] ["d7" "d6"] ["b8" "c6"] ["e8" "e7"])
           option-state)]
    [(first move-seq) (next move-seq)]))

;;study this case cause it's bugged
 ;; (let [[score move-history last-board invalid-move?] (play-game (initial-board) f1 f2)]
 ;;   (display-board last-board))
;; => [move option-state]
