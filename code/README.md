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

    (ns example
      (:use [embang emit runtime])

    (defanglican example
      [assume x (sample (uniform-discrete 0 9))]
      [observe (normal x 1) 5]
      [predict x])

Multiple programs may reside within the same namespace.  By
convention, the default program has the same name as the last
component in the namespace name.  Place the program in an
appopriate location in Clojure directory layout; the example
above should go into [`../examples`](../examples/) subdirectory
of __m!__ tree, referenced from `project.clj` as a resource
path.

The language is essentially compatible with Anglican, with 
the following 'restrictions', or enhancements, depending on the
point of view. 

  1. `recur` for functions is not implemented. __m!__ is fully
  tail-recursive through trampolining of all calls of compound
  procedures, thus `recur` is not needed, and is actually
  misleading. `recur` with `loop` works (by transforming `loop`
  into a function and calling the function recursively).

  2. Distribution names --- `normal`, `gamma`, `flip` etc --- 
  return distribution objects. To obtain a sample from the
  object, `sample` must be called explicitly on the object.

  3. Random processes are implemented functionally. `produce`
  produces a static random source from the random process.
  Depending on the process, the returned value can be a 
  distribution, or a function returning a distribution. 
  To get the next state of the random process, `absorb` must
  be called on the current state and a sample.

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
[`../examples/neural_net.clj`](../examples/neural_net.clj),
[`../examples/ctp_pp.clj`](../examples/ctp_pp.clj)
for inspiring examples.

### Running programs

Anglican programs can be run either from the command line
or in the REPL. The simplest way to run the programs in the
REPL is to use the `-main` function and pass it the command-line
arguments:

    lein run namespace [program] [option ...]

from the command line, or:

    (m! namespace [program] [option ...])

in the REPL, where 'namespace' is the namespace containing the
embedded Anglican program to run, for example:

    bash$ lein run branching -a gibbs -n 100 \
               -o ":number-of-particles 50"

    embang.core=> (m! branching -a gibbs -n 100
                      -o ":number-of-particles 50")
               
'program' is the first argument of 'defanglican'. The namespace
may contain multiple programs. If 'program' is omitted, it defaults
to the last component of the namespace (hmm for anglican.hmm,
logi for anglican.logi).

#### Options:

    -a, --inference-algorithm NAME   :lmh       Inference algorithm
    -b, --burn N                     0          Skip first N samples
    -d, --debug                                 Print debugging information
    -f, --output-format FORMAT       :anglican  output format
    -n, --number-of-samples N                   Output predicts for N samples
    -o, --algorithm-options OPTIONS  []         Algorithm options
    -t, --thin N                     1          Retain each Nth sample
    -v, --value V                               Initial value to pass to the program
    -h, --help                                  print usage summary and exit

#### Inference algorithms

Currently implemented inference algorithms are:

 * **lmh** --- Lightweight Metropolis-Hastings (random
   databaset)
 * **pgibbs** --- Iterative Conditional SMC (Partical
   Gibbs)
 * **pimh** --- Particle Independent Metropolis-Hastings

As well as a bunch of more esoteric algorithms. If you are
curious, consult the [code map](doc/codemap.md) and/or look
for modules with implementations of `embang.inference/infer`.
  
## Examples

Some program examples are in [`../examples`](../examples/).
If you want to run programs from the repl:

    (m! --help)

will print the command-line options.

    (m! -a gibbs branching -n 10000
        -o "number-of-particles 100")

will run the inference on the 'simple branching' example. The
example itself is in '../examples/branching.clj'. You can also
run the same example from the command line:

    lein run -- -a gibbs -n 10000 \\
         -o ":number-of-particles 100" hmm

## License

Copyright © 2014, 2015 Wood group

This file is part of Anglican, a probabilistic programming system.

Anglican is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Anglican is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the [GNU General Public
License](gpl-3.0.txt) along with Anglican.  If not, see
<http://www.gnu.org/licenses/>.
