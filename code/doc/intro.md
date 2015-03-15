# Introduction to __m!__

__m!__ (pronounced em-bang) is a probabilistic programming
system, built in tradition and as evolutionary development
of [Anglican](http://www.robots.ox.ac.uk/~fwood/anglican/).

__m!__ is based on Clojure. The programmming language of __m!__
is a subset of Clojure, extended with a few special form
introducing randomness into programs. The forms are `sample`
for drawing a sample from distribution, `observe` for
conditioning the posterior distribution on a value, and
`predict`, for including a value into the output sample.
There are other special forms --- `mem`, `store`, and `retrieve`
--- which make writing probabilistic programs easier.

[TOC]

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
