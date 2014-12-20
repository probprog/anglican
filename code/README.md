# embang

embang, or __m!__, is an implementation of
[Anglican](http://www.robots.ox.ac.uk/~fwood/anglican/) inside
Clojure. An Anglican program is translated into Clojure using
macros, converted into CPS, and executed with different
inference algorithms. Some understanding of using CPS
transformation for probabilistic programs can be obtained from
the online book [The Design and Implementation Of Probabilistic
Programming Languages](http://dippl.org/). However, the
implementation of __m!__  deviates in many ways from the approach
outlined in the book.

## Installation

Use git to download from https://bitbucket.org/dtolpin/embang:

	$ git clone https://bitbucket.org/dtolpin/embang

__m!__ is in `code`.

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
layout; the example above should go into [`src/angsrc`](src/angsrc/)
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

A program in __m!__  has access to a store through special
forms `store` and `retrieve`. These forms are useful for
implementing stochastic processes. Both forms access a
possibly empty list of keys, which can be arbitrary
expressions; in addition, `store` accepts a value arguments.
Values stored for a particular key sequence can be retrieved
by the same key sequence. New values for the same key
sequence can be stored by recurring invocation of `store`.
The last stored value is retrieved.

Functions can be defined outside Anglican programs, and
implemented either in Clojure or in Anglican. See 
[`src/angsrc/branching.clj`](src/angsrc/branching.clj)
for examples.

### Running programs

Anglican programs can be run either from the command line
or in the REPL. The simplest way to run the programs in the
REPL is to use the `-main` function and pass it the command-line
arguments:

     lein run namespace [program] [option ...]

from the command line, or:

     (-main "namespace" ["program"] ["option" ...])

in the REPL, where 'namespace' is the namespace containing the
embedded Anglican program to run, for example:

	  bash$ lein run angsrc.branching -a pgibbs \
				-o ":number-of-sweeps 10 :number-of-particles 50"

	  embang.core=> (-main "angsrc.branching" "-a" "pgibbs"
			   "-o" ":number-of-sweeps 10 :number-of-particles 50")
			   
'program' is the first argument of 'defanglican'. The namespace
may contain multiple programs. If 'program' is omitted, it defaults
to the last component of the namespace (hmm for anglican.hmm,
logi for anglican.logi).

#### Options:

	-a, --inference-algorithm NAME   importance  Inference algorithm
	-d, --debug                                  Print debugging information
	-o, --algorithm-options OPTIONS  []          Algorithm options
	-h, --help                                   print usage summary and exit


## Examples

Some program examples are in [`src/angsrc`](src/angsrc/).
If you want to run programs from the repl:

	(-main "--help")

will print the command-line options.

	(-main "-a" "pgibbs" "angsrc.branching" "-o"
     	":number-of-sweeps 50 :number-of-particles 100 :output-format :anglican")

will run the inference on the 'simple branching' example. The
example itself is in 'src/angsrc/branching.clj'. You can also
run the same example from the command line:

	lein run -- -a pgibbs \\
	  -o ":number-of-sweeps 50 :number-of-particles 100" angsrc.hmm

## License

Copyright © 2014 Wood group

Distributed under the Eclipse Public License either version 1.0
or (at your option) any later version.
