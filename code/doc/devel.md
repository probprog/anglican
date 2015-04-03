# Developer Guide

## Developing with __m!__

* Using __m!__ as a library: __m!__ is on
  [clojars](https://clojars.org/embang). If you want to use __m!__
  to develop your algorithms and applications, include [embang
  "X.Y.Z"] (with a recent version instead of "X.Y.Z") into your project.

* Proposing patches:
  1. [Fork embang](https://bitbucket.org/dtolpin/embang/fork).
  1. Make changes in the fork. The [code map](codemap.md)
     explains the source tree layout and module contents.
  1. Create a pull request.
  1. If the pull request resolves an issue, refer to the issue
     in the comment.

## Reporting bugs

* Use [issue tracker](https://bitbucket.org/dtolpin/embang/issues) to
  report bugs and suggest features.

## Style guide

When suggesting fixes/changes/improvements, stick to the following
rules, or discuss before breaking them knowingly.

### General Formatting

* Keep the line width within the limit of 80 characters
  strictly, below 70 characters whenever possible.
* Use consistent indentation. Whatever your editor (Vim, Emacs, 
  LightTable) suggests is most probably good enough, but do
  not override the indentation manually on a case-by-case
  basis.
* In Lisp, a closing bracket or parenthesis does not
  traditionally start a line. Put closing brackets/parentheses
  at the end of expressions they close.

### Documenting the code

* Every namespace must have a documentation string describing
  the purpose and essential functionality of the namespace.
* Every function must have a documentation string explaining
  what the function does and returns.

### Comments

* Do not leave dead code (commented out code fragments) in the
  committed source code. Comments are for humans. Use timbre
  (https://github.com/ptaoussanis/timbre) if you need debugging
  printouts in the code.
* Comments that take up their own line (or start after an
  opening square bracket) should start with a
  double semicolon (three, four, five for headers).
* Inline comments should use a single semicolon.

### Unit testing

* Prepare enough tests to ensure that the code works correctly,
  and changes that break the code are immediately identified.
  Place unit tests for module embang.foo into
  test/embang/foo_test.clj (namespace embang.foo-test).
* All tests much pass (lein test) before a change to the public
  repository.

## Implementation Guides

### Distributions and random processes

Two abstractions of random sources are used in Anglican, a
_distribution_ and a _random process_, the former corresponding
to 'elementary random procedure' (ERP), the latter 
related to 'exchangeable random procedure' (XRP).

Distributions and random processes are defined through
implementation of protocols `embang.runtime/distribution` and
`embang.runtime/random-process`. In addition, a multivariate
distribution may optionally implement protocol
`embang.runtime/multivariate-distribution`. Several
distributions are defined in `embang.runtime`, and other
distributions may be defined in terms of the 'basic'
distributions. 

For example, the Bernoulli distribution can be defined in terms
of uniform-continuous distribution:

	(defn bernoulli
	  "Bernoulli distribution"
	  [p]
	  (let [dist (uniform-continuous 0. 1.)]
		(reify
		  distribution
		  (sample [this] (if (< (sample dist) p) 1 0))
		  (observe [this value]
				   (Math/log (case value
							   1 p
							   0 (- 1. p)
							   0.))))))

where `sample` and `observe` are two methods of the
`distribution` protocol that must be provided.

A better and easier way to implement a distribution is macro
`defdist`.  In addition to defining the distribution function,
`defdist` assigns each distribution a record type, as well
as arranges for pretty-printing of the distribution instances.
The above declaration using `defdist` is:

	(defdist bernoulli
	  "Bernoulli distribution"
	  [p] [dist (uniform-continuous 0. 1.)]
	  (sample [this] (if (< (sample dist) p) 1 0))
	  (observe [this value]
			   (Math/log (case value
						   1 p
						   0 (- 1. p)
						   0.))))))

The first square brackets define the parameter list of the
function that creates the distribution instance. The second square
brackets define additional bindings (which may depend on the
parameters) used by the methods. Behind the scenes, `defdist`
does more than the `reify`-based definition above: it also 
defines a record type `bernoulli-distribution`, and instantiates
`print-method` for the type so that the distribution instance is
printed nicely. Consult the source code in
[`src/embang/runtime.clj`](../src/embang/runtime.clj) for the 
implementation of `defdist`.

Likewise, `defproc` is the macro for implementing random
processes. The two methods that must be implemented are
`produce` and `absorb`. `produce` returns a distribution
corresponding to the current state of the process instance.
`absorb` receives a sample and returns a new process instance
updated with the sample. For example, the Chinese Restaurant
process can be defined in the following way:

	(defproc CRP
	  "Chinese Restaurant process"
	  [alpha] [counts []]
	  (produce [this] 
		(let [dist (discrete (conj counts alpha))]
		  (reify 
			distribution
			(sample [this] (sample dist))
			(observe [this value]
			  (observe dist (min (count counts) value))))))
	  (absorb [this sample] 
		(CRP alpha
			 (-> counts
				 ;; Fill the counts with alpha (corresponding to
				 ;; the zero count) until the new sample.
				 (into (repeat (+ (- sample (count counts)) 1) alpha))
				 (update-in [sample] inc)))))

Of course, instead of reifying the distribution inside the
`produce` method, one can define a new distribution using
`defdist` (as in the implementation of CRP in
[src/embang/runtime.clj]('../src/embang/runtime.clj')).

### Inference algorithms

An inference algorithm must implement the
`embang.inference/infer` multimethod. The method dispatches
on a keyword. If the algorithm is defined in a namespace
`embang.foo`, and the keyword is `:foo`, the algorithm's
namespace will be loaded automatically by either
`embang.core/m!` or `mrepl.core/doquery`.  However, an algorithm
may be implemented in any namespace and loaded explicitly before
infer is called.

The simplest algorithm to implement is importance sampling:

	(ns embang.importance
	  (:refer-clojure :exclude [rand rand-int rand-nth])
	  (:use [embang state inference]))

	(derive ::algorithm :embang.inference/algorithm)

	(defmethod infer :importance [_ prog value & {}]
	  (letfn [(sample-seq []
				(lazy-seq
				  (cons (:state (exec ::algorithm
				                      prog value initial-state))
						(sample-seq))))]
				(sample-seq)))

For more examples, look at implementations of SMC, Particle
Gibbs, Lightweight Metropolis-Hastings. The [code
map](codemap.md) points at the Clojure modules containing
the implementations.

Although not required, a convenient function for implementing
an inference algorithm is `embang.inference/exec`. This function
runs the probabilistic program until a so-called checkpoint,
a point in execution that requires intervention of the inference
algorithm. There are three types of checkpoints:

    embang.trap.sample
	embang.trap.observe
	embang.trap.result

corresponding to `sample` and `observe` probabilistic forms, as
well as to returning the final result of a program execution,
which encapsulates, among other things, the list of predicts
and the log weight of the sample. The multimethod
`embang.inference/checkpoint` should be used together with
`exec`. Default implementations of the multimethod for each type
of checkpoint are provided by the `embang.inference` namespace,
and correspond to actions performed during importance sampling.
