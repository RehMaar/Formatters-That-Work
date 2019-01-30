# Formatters-That-Work
Trees-That-Grow Haskell Parser 

# Plan
* Implement parser of some simple language that uses Trees That Grow ideas.
* See how to implement AST to hold printing info.
* Probably, implement it for this language.
* Implement parser of simple subset of Haskell.
* ...


# Build'n'Run

To build, type  `stack build`.

To run, type `stack exec ftw-exe file.hs`.

# Bugs


* Operators like `(+)` has the same data contructor as literals, so it's hard to handle them separetely (both in `()` or not).

