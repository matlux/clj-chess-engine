(ns clj-chess-engine.core
  (:require [clojure.math.numeric-tower :as math]))



(defn initial-board []
  [\r \n \b \q \k \b \n \r
   \p \p \p \p \p \p \p \p
   \- \- \- \- \- \- \- \-
   \- \- \- \- \- \- \- \-
   \- \- \- \- \- \- \- \-
   \- \- \- \- \- \- \- \-
   \P \P \P \P \P \P \P \P
   \R \N \B \Q \K \B \N \R])

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

(defn lookup [^clojure.lang.PersistentVector board ^String pos]
  (let [[file rank] pos]
    (board (index file rank))))
(defn lookup-xy [^clojure.lang.PersistentVector board ^clojure.lang.PersistentVector pos]
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
       (>= x 0)
       ))

(defn collid-self? [board white-turn? coord]
  (if white-turn?
    (is-white? (lookup board (coord2pos coord)))
    (is-black? (lookup board (coord2pos coord)))))


;(collid-self? (initial-board) false [0 7])
;(Character/isLowerCase \-)

(defn- is-vertical? [[x1 y1] [x2 y2]]
  (zero? (- x1 x2)))

;;---- bishop

(defn- nothing-between-vertical [board [x1 y1] [x2 y2]]
  (not-any? is-piece? (for [a (range (inc y1) y2)] (lookup-xy board [x1 a]))))
;; (defn- nothing-between-vertical [board [x1 y1] [x2 y2]]
;;   (for [a (range (inc y1) y2)
;;         piece (lookup-xy board [x1 a])
;;         :when (is-piece? piece)] piece))

;; (defn- nothing-between-xy [board [x1 y1] [x2 y2]]
;;   (let [slop (/ (- y2 y1) (- x2 x1))
;;         step (if (> (- x2 x1) 0) 1 -1)]
;;     (map #([(+ x1 1) (+ y1 slop)]) (range 0 8))))

(defn- pos-between-vertical [[x1 y1] [x2 y2]]
  (let [[b1 b2] (if (= (.compareTo y2 y1) 1) [y1 y2] [y2 y1])]
      (for [a (range (inc b1) b2)] [x1 a])))

(defn- pos-between-xy [[x1 y1] [x2 y2]]
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


(defn- nothing-between-xy [board [x1 y1] [x2 y2]]
  {:pre [(let [absslop (math/abs (/ (- y2 y1) (- x2 x1)))]
           (println absslop)
           (or (= absslop 1)
               (= absslop 0)))]}
  (let [slop (/ (- y2 y1) (- x2 x1))
        step (if (> (- x2 x1) 0) 1 -1)
        f (fn [x] [(+ x1 (* step x)) (+ y1 (* slop x))])]
    (map f (range (inc x1) x2))))


(nothing-between-xy (initial-board) [0 0] [7 7])

(nothing-between-vertical (initial-board) [0 1] [0 6])

(defn- nothing-between [^clojure.lang.PersistentVector board ^clojure.lang.PersistentVector c1 ^clojure.lang.PersistentVector c2]
  (cond
   (is-vertical? c1 c2) (nothing-between-vertical board c1 c2)
   :else (nothing-between-xy board c1 c2)))

(defn- bishop-moves1 [board x y]
  (for [a (range -7 8)
        b (range -7 8)
        :when (and
               (or (= (+ a b) 0) (= (- a b) 0))
               (not (= a 0))
               (not (= b 0)))] [a b]))

(defn- bishop-moves [board x y]
  (for [a (range -7 8)
        b (range -7 8)
        :when (and
               (or (= (+ a b) 0) (= (- a b) 0))
               (not (= a 0))
               (not (= b 0))
               (nothing-between board [a b] [x y]))] [a b]))

(count (bishop-moves (initial-board) 1 2))

(defrecord Bishop [^clojure.lang.PersistentVector board ^String pos ^boolean white?]
  Piece
  (getMoves [this]
    (let [[x y] (pos2coord pos)
          moves (bishop-moves x y)]
      (map coord2pos (filter (partial collid-self? board white?) (filter valid-move? moves))))))

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



(defrecord Knight [^clojure.lang.PersistentVector board ^String pos ^boolean white?]
  Piece
  (getMoves [this]
    (let [[x y] (pos2coord pos)
          kmoves (knight-moves x y)
          no-self-collision? (comp not (partial collid-self? board white?))]
      (map coord2pos (filter no-self-collision? (filter valid-move? kmoves))))))

;;(getMoves (Knight. (initial-board) "g1" true))
;;(pos2coord "g1")

;;(source comp)

(defn- is-knight? [ piece]
  (or (= piece \N)
      (= piece \n)))

(defn convert2obj [board pos piece]
  (cond (is-knight? piece) (Knight. board "g1" true)))


(defn possible-moves [board pos]
  (getMoves (convert2obj board pos (lookup board pos))))
;; => #{"e4" "e3"}

(defn all-possible-moves [board white-turn? castle?]
  #{["e2" "e4"] ["g1" "f3"]})

;(all-possible-moves nil nil nil)



;; -------------- rendering

(def ^:const board (vec (range 8)))

(defn c2dto1d [v]
  (let [[x y] v]
    (clojure.core/+ x (clojure.core/* 8 y))))

(defn c1dto2d [i]
  (vector (int (/ i 8)) (mod i 8)))

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

(defn display-board [board-state]
  (print (render-board board-state)))

(defn char2state [pieces-list]
  (into {} (filter #(not= \- (second %)) (map #(vector (c1dto2d %1) %2 ) (range 64) pieces-list))))

 ((fn [pieces-list]
    (into {} (filter #(not= \- (second %)) (map #(vector (c1dto2d %1) %2 ) (range 64) pieces-list)))) (initial-board))






(def init-board-state (char2state (initial-board)))


;(display-board init-board-state)

;(display-board '([[1 0] "p"] [[2 2] "*"] [[0 1] "&"]))
