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
    (play-game (initial-board) interactive-f random-f)

Enter your moves as follow in the stdin:
    [:e2 :e4]

### Play two random algorithms against one another

    (play-game (initial-board) random-f random-f)

Sit back and enjoy the game. Watch out the CPU is going to get hot and the function might never return...
Unless you're lucky enough that one of the random algorithm acheives a check-mate over its adversary. :)

## License

Copyright © 2014 Matlux

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
