# README #

## MAP for Probabilistic Programming ##

I am looking into finding the MAP of a probabilistic model
faster than using `general' probabilistic inference, that is,
sampling from the posterior distribution.  Sampling-based
search techniques should work.

## Embedded Anglican ##

As a tool for exploring MAP search algorithms, I have
re-implemented Anglican as an embedded language. An Anglican
program is translated into Clojure through a Clojure macro,
and then the code (converted into CPS) is executed natively.
The points where inference algorithms must intervene and
affect sampling are exposed by stopping the execution and
returning continuation, along with auxiliary information.

The implementation, documentation, and examples are in
`code/embang`. 
