# Introduction to __m!__

__m!__ (pronounced em-bang) is a probabilistic programming
system, built in tradition and as evolutionary development
of [Anglican](http://www.robots.ox.ac.uk/~fwood/anglican/).

__m!__ is based on Clojure. The programming language of __m!__
is a subset of Clojure, extended with a few special form
introducing randomness into programs. The forms are `sample`
for drawing a sample from distribution, `observe` for
conditioning the posterior distribution on a value, and
`predict`, for including a value into the output sample.
There are other special forms --- `mem`, `store`, and `retrieve`
--- which make writing probabilistic programs easier.

## First program

### Writing a program

__m!__ programs reside in Clojure source code modules, 
and are defined by the `defquery` macro. In order to
enable the __m!__ language in the namespace, namespaces
`embang.runtime` and `embang.emit` must be used. A simple way to
do this is to write

    (ns example
      (:use [embang emit runtime]))

in the beginning of file 'example.clj'. The rest of the module
may contain one or more __m!__ programs, or _queries_. The query
is introduced by keyword `defquery`, followed by the query name
and the program text. If there is only one program in a
namespace, it customarily bears the same name as the namespace:

    (defquery example
      (let [bet (sample (beta 5 3))]
        (observe (flip bet) true)
        (predict (> bet 0.7))))

Put the namespace declaration and the query definition
in a file `example.clj` to create your first __m!__ program.

### Running the program

__m!__ can be run from the command line, in the Clojure REPL,
as well in [other ways](#markdown-header-m-in-gorilla-repl). To run __m!__ from the
command line or in the REPL, use git to download from 
https://bitbucket.org/dtolpin/embang:

    $ git clone https://bitbucket.org/dtolpin/embang

__m!__ is in `code`. Change the current directory to `code`,
open `project.clj` and add the path to the directory in which
`example.clj` resides to `:resource-paths`.
For example, if example.clj is in '/home/user/embang', the 
`:resource-paths` line might look like:

    :resource-paths ["/home/user/embang" "../examples"]

You can now run

   lein run -- -n 10 example

from the command line, or

    (m! -n 10 example)

in the REPL. In both cases, the output will look like:

    (> bet 0.7),true,0.0
    (> bet 0.7),true,0.0
    (> bet 0.7),false,0.0
    (> bet 0.7),true,0.0
    (> bet 0.7),true,0.0
    (> bet 0.7),false,0.0
    (> bet 0.7),true,0.0
    (> bet 0.7),false,0.0
    (> bet 0.7),false,0.0
    (> bet 0.7),true,0.0
    (> bet 0.7),true,0.0

There will be also a bunch of lines starting with ';' at the
beginning of the output. These lines are 'comment lines'
providing general information about the program, the inference
algorithm, and the invocation options.

Run 

    lein run -- --help

from the command line, or 

    (m! --help)

in the REPL to get the full command line syntax and the list of
options.

## Examples

Some program examples are in [`../examples`](../examples/).
Since "../examples" is already in `:resource-paths`, you can run
any example (a file with `.clj` extension) by simply supplying
its name to `m!` in the REPL or to `lein run` on the command
line. For example, the following commands run `branching` in the
REPL and on the command line, correspondingly:

    (m! -n 1000 branching)
    lein run -- -n 1000 branching

## Language

### Core syntax

The __m!__ language is a subset of Clojure. Within `defquery`,
`let`, `if`, `cond`, `and`, `or`, `case`, `fn` forms are
supported. In `let` bindings and `fn` argument lists,
vector destructuring (but not hash map destructuring) is
supported. Compound literals for vectors, hash maps, and
sets are supported just like in Clojure.

### `recur`

__m!__ is stackless, therefore `recur` is
unnecessary, no recursive call can lead to stack overflow;
Recursive calls to functions should be used instead. However,
`loop`/`recur` is provided for convenience as a way to express
loops. `recur` outside of `loop` will lead to unpredictable
behaviour and hard-to-catch errors.

### Core library

All of Clojure core library, except for higher-order functions
(functions that accept other functions as arguments) is
available in __m!__. In addition, the following higher-order
functions are available: `map`, `reduce`, `filter`, `some`,
`repeatedly`, `comp`, `partial`.

### Definitions outside of `defquery`

Data variables may be defined outside of `defquery` using
`def` and used inside `defquery`. __m!__ functions outside 
of `defquery` may be defined using `defm` (with the same syntax
as `defn`). Their bodies may use the same subset of Clojure
as `defquery`, as well as probabilistic and state access forms.
`defm`-defined functions can be called from __m!__ without
restrictions.

Functions defined outside of `defquery` using `defn` may use the
full Clojure syntax but none of __m!__ extensions, and must be
declared primitive using `with-primitive-procedures`:

    (with-primitive-procedures [name ...]
       body)

Where `name ...` is the  list of primitive procedures. The names
can be namespace-qualified, but will be seen unqualified in the
lexical scope of the form. For example,

    (with-primitive-procedures [clojure.string/capitalize]
       (defquery foo
          (predict :hello (capitalize "hello"))))

Will predict `:hello` as `Hello` (capitalized). 

### Probabilistic forms

There are three probabilistic forms: `sample`, `observe`, and
`predict`.

* `(sample distribution)` returns a sample from 
  `distribution`.
* `(observe distribution value)` conditions the posterior
  distribution  by observing `value` from  `distribution`.
* `(predict [label] value)` adds `value` as a prediction for
  `label`  to the sample. `label` is optional; if omitted, the
  symbolic form of the `value` argument is used.

### State access and modification

Functions can be memoized using `mem`, which accepts a function
object as its argument. If the argument is a named `fn` form,
self-recursive calls will call the memoized version of the
function. For example, every `predict` in the following code

    (defquery fact
        (let [fact (mem (fn fact [n]
                            (if (= n 1) 1
                                * n (fact (- n 1))))))]
          (predict (fact 1))
          (predict (fact 2))
          (predict (fact 3))
          (predict (fact 4))))

will call `fact` once per program invocation.

Values can be stored in the state using `store`, values stored
during the same run of the program can be retrieved using
`retrieve`. The syntax is

* `(store key ... value)` stores `value` at `key ...` in the state.
* `(retrieve key ...)` retrieves and returns the value stored at
  `key ...`. `key ...` can be a sequence of any length.

For example:  

    (defquery customer
      (store :customer 4 :age 18)
      (predict :age (retrieve :customer 4 :age)))

will predict ':age' to be 18.

### Distributions

Distributions are defined in
[../src/embang/runtime.clj](../src/embang/runtime.clj) and
include `bernoulli`, `beta`, `binomial`, `discrete`,
`dirichlet`, `exponential`, `gamma`, `normal`, `poisson`,
`uniform-continuous`, `uniform-discrete`, `mvn`, `wishart`.
In addition, so-called random processes are provided by the
runtime, including CRP (Chinese Restaurant Process), DP
(Dirichlet Process), and GP (Gaussian Process). Random processes
implement the `random-process` protocol.

Other distributions and processes can be defined by the user.
The definition can be placed into Clojure modules containing
__m!__ programs. The constructors of user-defined distributions
and processes must be declared primitive using
`with-primitive-procedures`.

## __m!__ in Gorilla REPL

Yet another way to run __m!__ programs is [Gorilla
REPL](http://gorilla-repl.org). __m!__ distribution contains
a wrapper for gorilla REPL, in `mrepl` directory of the
repository. Go `mrepl` and run `lein gorilla`. 
Open the REPL URL in the browser, and either start a new
worksheet, or load a sample worksheet from `worksheets/`
subfolder. `worksheets/tworoads.clj` is provided as a starting
point.

A worksheet should start with a namespace declaration that
imports necessary and useful symbols into the worksheet's
namespace:

    (ns tworoads
      (:require [gorilla-plot.core :as plot])
        (:use [mrepl core]
                [embang emit runtime])) 

An __m!__ program is defined in the usual way using `defquery`.
The inference is initiated by a call to `doquery` which accepts
the algorithm (for example, :lmh), the program name, and the
initial value (which will be `nil` for basic examples).
`doquery` returns a lazy sequence of samples.

    (def samples (doquery :lmh example nil))

Take a look at
[worksheets/tworoads.clj](../../mrepl/worksheets/tworoads.clj),
as well as at [Gorilla REPL
documentation](http://gorilla-repl.org/start.html) to get
inspired.
