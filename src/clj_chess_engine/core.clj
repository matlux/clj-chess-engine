(ns clj-chess-engine.core)



(defn initial-board []
  [\r \n \b \q \k \b \n \r
   \p \p \p \p \p \p \p \p
   \- \- \- \- \- \- \- \-
   \- \- \- \- \- \- \- \-
   \- \- \- \- \- \- \- \-
   \- \- \- \- \- \- \- \-
   \P \P \P \P \P \P \P \P
   \R \N \B \Q \K \B \N \R])

(def *file-key* \a)
(def *rank-key* \0)

(defn- file-component [file]
  (- (int file) (int *file-key*)))

(defn- rank-component [rank]
  (* 8 (- 8 (- (int rank) (int *rank-key*)))))

(defn- index [file rank]
  (+ (file-component file) (rank-component rank)))

(defn lookup [board pos]
  (let [[file rank] pos]
    (board (index file rank))))

(defn lookup2 [board pos]
  (let [[file rank] (map int pos)
        [fc rc]        (map int [\a \0])
        f (- file fc)
        r (* 8 (- 8 (- rank rc)))
        index (+ f r)]
    (board index)))


;; fluent move example from section 9.4.2

(defrecord Move [from to castle? promotion]
  Object
  (toString [this]
    (str "Move " (:from this)
         " to " (:to this)
         (if (:castle? this) " castle"
             (if-let [p (:promotion this)]
               (str " promote to " p)
               "")))))

(defn build-move [& {:keys [from to castle? promotion]}]
  {:pre [from to]}
  (Move. from to castle? promotion))


;; --------------

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
