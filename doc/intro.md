# Introduction to Anglican

Anglican is a probabilistic programming system.
Anglican is based on Clojure. The programming language of Anglican
is a subset of Clojure, extended with a few special forms
introducing randomness into programs. The forms are `sample`
for drawing a sample from distribution, `observe` for
conditioning the posterior distribution on a value, and
`predict`, for including a value into the output sample.
There are other special forms — `mem`, `store`, and `retrieve`
— which make writing probabilistic programs easier.

## First program

### Writing a program

Anglican programs reside in Clojure source code modules,
and are defined by the `defquery` macro. In order to
enable the Anglican language in the namespace, namespaces
`anglican.runtime` and `anglican.emit` must be used. A simple way to
do this is to write

    (ns example
      (:use [anglican emit runtime]))

in the beginning of file 'example.clj'. The rest of the module
may contain one or more Anglican programs, or _queries_. The query
is introduced by keyword `defquery`, followed by the query name
and the program text. If there is only one program in a
namespace, it customarily bears the same name as the namespace:

    (defquery example
      (let [bet (sample (beta 5 3))]
        (observe (flip bet) true)
        (predict (> bet 0.7))))

Put the namespace declaration and the query definition
in a file `example.clj` to create your first Anglican program.

### Running the program

Anglican can be run from the command line, in the Clojure REPL,
as well in [other ways](#markdown-header-m-in-gorilla-repl). To run Anglican from the
command line or in the REPL, use git to download from
https://bitbucket.org/dtolpin/anglican:

    $ git clone https://bitbucket.org/dtolpin/anglican

Open `project.clj` and add the path to the directory in which
`example.clj` resides to `:resource-paths`.
For example, if example.clj is in '/home/user/anglican', the
`:resource-paths` that needs to be added would look like:

    :resource-paths ["/home/user/anglican"]

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

## Language

### Core syntax

The Anglican language is a subset of Clojure. Within `defquery`,
`let`, `if`, `cond`, `and`, `or`, `case`, `fn` forms are
supported. In `let` bindings and `fn` argument lists,
vector destructuring (but not hash map destructuring) is
supported. Compound literals for vectors, hash maps, and
sets are supported just like in Clojure.

### `recur`

Anglican is stackless, therefore `recur` is
unnecessary, no recursive call can lead to stack overflow;
Recursive calls to functions should be used instead. However,
`loop`/`recur` is provided for convenience as a way to express
loops. `recur` outside of `loop` will lead to unpredictable
behaviour and hard-to-catch errors.

### Core library

All of Clojure core library, except for higher-order functions
(functions that accept other functions as arguments) is
available in Anglican. In addition, the following higher-order
functions are available: `map`, `reduce`, `filter`, `some`,
`repeatedly`, `comp`, `partial`.

### Definitions outside of `defquery`

Data variables may be defined outside of `defquery` using
`def` and used inside `defquery`. Anglican functions outside
of `defquery` may be defined using `defm` (with the same syntax
as `defn`). Their bodies may use the same subset of Clojure
as `defquery`, as well as probabilistic and state access forms.
`defm`-defined functions can be called from Anglican without
restrictions.

Functions defined outside of `defquery` using `defn` may use the
full Clojure syntax but none of Anglican extensions, and must be
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
                                (* n (fact (- n 1))))))]
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
[../src/anglican/runtime.clj](../src/anglican/runtime.clj) and
include `bernoulli`, `beta`, `binomial`, `discrete`,
`dirichlet`, `exponential`, `gamma`, `normal`, `poisson`,
`uniform-continuous`, `uniform-discrete`, `mvn`, `wishart`.
In addition, so-called random processes are provided by the
runtime, including CRP (Chinese Restaurant Process), DP
(Dirichlet Process), and GP (Gaussian Process). Random processes
implement the `random-process` protocol.

Other distributions and processes can be defined by the user.
The definition can be placed into Clojure modules containing
Anglican programs. The constructors of user-defined distributions
and processes must be declared primitive using
`with-primitive-procedures`.

## Inference Algorithms

Anglican provides a range of inference algorithms, and the list
is growing. The most up-to-date list of available algorithms
is in the [code map](codemap.md); algorithm options are keyword
arguments of their `infer` methods.

Below are some algorithms of general interest:

lmh
:   Lightweight Metropolis-Hastings. No options.

pgibbs
:   Particle Gibbs.
Options:

      * `:number-of-particles` (2 by default) — number of
	    particles per sweep.

pcascade
:   Particle Cascade. Options:

      * `:number-of-threads` (16 by default) — number of threads.
      * `:number-of-particles` (number-of-threads/2 by default)
	   — number of initial particles.

## Anglican in Gorilla REPL

Yet another way to run Anglican programs is [Gorilla
REPL](http://gorilla-repl.org). Anglican distribution contains
a wrapper for gorilla REPL, in `mrepl` directory of the
repository. Go `mrepl` and run `lein gorilla`.
Open the REPL URL in the browser, which will open a new worksheet.

A worksheet should start with a namespace declaration that
imports necessary and useful symbols into the worksheet's
namespace:

    (ns example-worksheet
      (:require [gorilla-plot.core :as plot])
        (:use [mrepl core]
                [anglican emit runtime]))

An Anglican program is defined in the usual way using `defquery`.
The inference is initiated by a call to `doquery` which accepts
the algorithm (for example, :lmh), the program name, and the
initial value (which will be `nil` for basic examples).
`doquery` returns a lazy sequence of samples.

    (def samples (doquery :lmh example nil))

Optionally, `doquery` accepts algorithm options as keyword
arguments, as well as the following additional options:

* `:warmup`, boolean and `true` by default. When `false`, the
program is not pre-evaluated. May be useful for tricks and
debugging. 
* `:debug`, boolean and `false` by default. When `true`, the
stack trace of exception thrown during inference is printed
on the standard output.

Take a look at the [Gorilla REPL
documentation](http://gorilla-repl.org/start.html) for further
information.
