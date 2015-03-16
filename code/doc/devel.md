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
  1. Create pull request to the *candidate* branch.
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
* In Lisp, a closing bracket or parentheses does not
  traditionally start a line. Put closing brackets/parentheses
  at the end of expressions they close.

### Documenting the code

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
