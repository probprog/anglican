# embang

embang, or __m!__, is an implementation of
[Anglican](http://www.robots.ox.ac.uk/~fwood/anglican/), inside
Clojure. An Anglican program is translated into Clojure using
macros, converted into CPS, and executed with different
inference algorithms. Some understanding of using CPS
transformation for probabilistic program can be obtained from
the online book[The Design and Implementation Of Probabilistic
Programming Languages](http://dippl.org/). However, the
implementation of __m!__  deviates in many ways from the approach
outlined in the book.

## Installation

Use git to download from https://bitbucket.org/dtolpin/mappp:

	$ git checkout https://bitbucket.org/dtolpin/mappp

embang is in `code/embang`.

## Usage

### Writing programs

Programmatically, Anglican is implemented as a Clojure macro.
Namespaces `ebmang.emit` and `embang.runtime` are required
to compile a program. The translation is performed by
`defanglican` macro, which receives the program name and
the program text as the macro body. Hence, an __m!__ program
would look like in the following example:

    (ns angsrc.example
	  (:use [embang emit runtime])

    (defanglican example
	  [assume x (sample (uniform-discrete 0 9))]
	  [observe (normal x 1) 5]
	  [predict x])

Multiple programs may reside within the same namespace.  By
convention, the default program (whatever this means) has the
same name as the last component in the namespace name.  Place
the program in an appopriate location in Clojure directory
layout; the example above should go into `src/angsrc`
subdirectory of __m!__ tree.

The language is essentially compatible with Anglican, with 
the following 'restrictions', or enhancements, depending on the
point of view. 

  1. `recur` is not recognized as a special instruction. __m!__
  is fully tail-recursive  through trampolining of all calls of
  compound procedures, thus `recur` is not needed, and is
  actually misleading.

  2. Distribution names --- `normal`, `gamma`, `flip` etc --- 
  return distribution objects. To obtain a sample from the
  object, `sample` must be called explicitly on the object.

### Running programs

Anglican programs can be run either from the command line
or in the REPL. The simples way to run the programs in the
REPL is to use the `-main` function and pass it the command-line
arguments.


## Options


## Examples

TODO

## License

Copyright © 2014 Wood group

Distributed under the Eclipse Public License either version 1.0
or (at your option) any later version.
