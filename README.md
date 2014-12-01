# Embedded Anglican #

I have re-implemented Anglican as an embedded language. An
Anglican program is translated into Clojure through a Clojure
macro, and then the code (converted into CPS) is executed
natively. The points where inference algorithms must intervene
and affect sampling are exposed by stopping the execution and
returning continuation, along with auxiliary information.

The implementation, documentation, and examples are in the
leiningen project tree rooted at [`code`](https://bitbucket.org/dtolpin/embang/src/HEAD/code/).
