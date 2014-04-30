# clj-chess-engine

A Clojure application designed to play chess and get functions to compete against one another.

## Usage

### Quick Demo

clone the repo

and launch

    lein run

### Play interactively against a random algorithm

clone the repo
start repl

    lein repl

enter the following commands

    (use 'clj-chess-engine.core)
    (play-game {:f1 interactive-f :f2 random-f})

Enter your moves as follow in the stdin:
    [:e2 :e4]

### Play two random algorithms against one another

    (use 'clj-chess-engine.core)
    (play-game {:f1 random-f :f2 random-f})

Sit back and enjoy the game. Watch out the CPU is going to get hot and the function will return after maximum 500 moves.

### The Chess Engine

play-game is a function which execute the Chess game. In other words play-game is a state engine which executes a game of chess in sequence. It takes 2 functions are parameters. Its aim is to

* maintain the modification of game state throughout the game
* work out the list of valid move for the next step
* call :f1 i.e. the white function or :f2 i.e. the black function to know which move to make on every step
* detects check and check-mates to stop the game
* stop after 500 moves with a draw if no-one wins
* stop if a function is making an invalid move

**Here are two function examples**

Interactive function:
```clojure
(defn interactive-f [{board :board am-i-white? :white-turn? valid-moves :valid-moves ic :in-check? h :history s :state}]
  (do
    (display-board board)
    (println (if am-i-white? "white: " "black: "))
    (println "valid moves:" valid-moves)
    (println "enter next move (in format [\"a2\" \"a3\"]):")
    (let [move (read-string (read-line))]
     move)))
```

Ramdom function
```clojure
(defn random-f [{board :board am-i-white? :white-turn? valid-moves :valid-moves ic :in-check? h :history s :state}]
  (let [v (into [] valid-moves)
        iteration (if (nil? s) (+ 1 (if am-i-white? 0 1)) (+ 2 s))]
    (display-board board)
    (println (if am-i-white? "white: " "black: "))
    (println "valid moves:" valid-moves)
    (println "iteration:" iteration)
    (let [move (rand-int (count valid-moves))]
      (println "choosen move:" (get v move))
      {:move (get v move) :state iteration})) )
```

### The function return value

Any game function need to return the datastructure (it's a map which contains at list a move):

```clojure
    {:move ["e2" "e4"], :state "arbitrary data structure of your choice and is optional"}
```

#### Format of a move

A move is pair of string or a pair of keywords.  

For example the following return value is also valid:

```clojure
    {:move [:e2 :e4]}
```

Valid move examples:
```clojure
    ["e2" "e4"]
```
or
```clojure
    [:e2 :e4]
```

The move's first element is the position of the piece to move; the second element is the coordinate of the destination to move the piece onto. So if you look at the following board, you'll see **[:e2 :e4]** moves the white pawn in front of the white king forward by 2 square (this is a classic chess first move).
![Chess Board with coordinates](http://www.eddins.net/steve/chess/ChessImager/ChessImager.php?fen=rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR/&coordinates=on&nonsense=foobar.png&ust=1398625534794231) 

You might have noticed that the coordinates follow a naming convention derived from the [algebraic notation](http://en.wikipedia.org/wiki/Algebraic_notation_(chess)) which is a standard used amongst chess players.


### The function input parameters

The input is a map which contains the following information:
```clojure
{board :board
 am-i-white? :white-turn
 valid-moves :valid-moves
 in-check? :in-check?
 history :history
 state :state}
```

**board** - This is what the initial board looks like:

```clojure
 [\r \n \b \q \k \b \n \r
   \p \p \p \p \p \p \p \p
   \- \- \- \- \- \- \- \-
   \- \- \- \- \- \- \- \-
   \- \- \- \- \- \- \- \-
   \- \- \- \- \- \- \- \-
   \P \P \P \P \P \P \P \P
   \R \N \B \Q \K \B \N \R]
```

**am-i-white?** is a boolean

    true = white
    false = black

**valid-moves** - a vector of move

```clojure
[["a2" "a3"] ["f7" "f5"] ["c2" "c4"] ... ]
```

**in-check?** - a boolean

    true = your king is in check
    false = your king is not in check

**history** - a vector of moves of the previous moves in this game - vector of pairs

```clojure
[["a2" "a3"] ["f7" "f5"] ["c2" "c4"] ... ]
```

**state** - this field is for the function implementor to keep any state of their choice between moves


## License

Copyright © 2014 Matlux

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
