# embang

embang, or m!, is an implementation of [Anglican](http://www.robots.ox.ac.uk/~fwood/anglican/),
inside Clojure. An Anglican program is translated into Clojure using macros, converted into
CPS, and executed with different inference algorithms. Some understanding of using CPS transformation
for probabilistic program can be obtained from the online book[The Design and Implementation
Of Probabilistic Programming Languages](http://dippl.org/). However, the implementation of m!
deviates in many ways from the approach outlined in the book.

## Installation

* Download from https://bitbucket.org/dtolpin/mappp,
* Use lein to manipulate the code in code/embang.

## Usage


	$ cd code/embang
    $ lein repl

## Options

TODO

## Examples

TODO

## License

Copyright © 2014 Wood group

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
