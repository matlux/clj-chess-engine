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


(def ^:dynamic *file-key* \a)
(def ^:dynamic *rank-key* \0)

(defn- file-component [file]
  (- (int file) (int *file-key*)))

(defn- rank-component [rank]
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
(defn- pos2coord [^String pos]
  (let [[file rank] pos
        x (file2coord file)
        y (rank2coord rank)]
    [x y]))

(defn- coord2pos [[x y]]
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

;; (file-component \a)
;;(rank-component \1)
;; (lookup (initial-board) "a1")
;;=> \R
;;(lookup-xy (initial-board) [0 7])

;; ------------- all possible moves

(defn- is-white? [^Character piece]
  (Character/isUpperCase piece))
(defn- is-black? [^Character piece]
  (Character/isLowerCase piece))
(defn- is-piece? [^Character piece]
  (Character/isLetter piece))


(defprotocol Piece
  (getMoves [this]))

;;(pos2coord "h3")

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


;(collid-self? (initial-board) false [0 7])
;(Character/isLowerCase \-)

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


;;----- change state of board (make moves)

;;todo: convert map back to vector of char or manipulate board directly
(defn apply-move [^PersistentVector board ^PersistentVector [from to]]
  (let [piece (lookup board from)]
    (-> (assoc board (apply index from) \-)
        (assoc (apply index to) piece)
        )))

;;(apply index "b2")

(display-board (apply-move (initial-board) ["b2" "b3"]))
(display-board (-> (apply-move (initial-board) ["b2" "b3"]) (apply-move ["b1" "c3"]) (apply-move ["e2" "e4"])))
;;(display-board (apply-move (initial-board) ["b2" "b3"]))


;; -------------- rendering

(def ^:const board (vec (range 8)))


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
