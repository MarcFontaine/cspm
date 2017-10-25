# CSPM-cspm

This package provides the _cspm_ executable.
The _cspm_ executable serves as a command line interface to the other CSPM-packages.
It can be used to parse, translate and interpret cspm specifications and to
compute the labeled transition system (LTS) of a specification.

## Modes
_cspm_ has several modes of operation:

* 'cspm --help'      -> print a help message.
* 'cspm eval '3+4''  -> evaluate an expression.
* 'cspm trace spec.csp' -> interactively trace a process.
* 'cspm assert spec.csp' -> check the assertions of a specification (only some assertions are supported).
* 'cspm lts spec.csp --dotOut spec.csp.dot' -> compute the labeled transition system of a process and dump it as dot-file.
* 'cspm lts spec.csp --fdrOut spec.csp.fdr'   -> compute the LTS
    and dump it a fdr script suitable for refinement checking.


## Lua Interface
_cspm_ comes with a built-in lua interpreter.

## Multi Core Support
LTS computation can demonstrate nice speed-ups on multi-core machines.
Try for example 'cspm +RTS -N7 -RTS fdr spec.csp' to use 7 cores.

## [Haddock documentation](http://hackage.haskell.org/package/CSPM-cspm)


