# clj-chess-engine

A Clojure application designed to play chess and get functions to compete against one another.

## Usage

### Play against a dumb algorithm

* clone the repo
* lein repl
* (play-game (initial-board) interactive-f random-f)

Enter your moves as follow in the stdin:
    [:e2 :e4]

### Play two ramdom algorithms against one another

    (play-game (initial-board) random-f random-f)

Sit back and enjoy the game. Whatch out the CPU is going to get hot and the function will never return... :)

## License

Copyright © 2014 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
