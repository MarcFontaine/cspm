# CSPM Interpreter and Animator

## CSP (Communicating Sequential Processes)
CSP is a formalism for concurrency.
A concurrent system is described as a number of processes that communicate
and synchronize via events.
CSP allows to precisely specify and analyze concurrent systems.
For example, with CSP one can prove the there are no deadlocks in a system
or that a system satisfies some safety properties.

## Erlang and Go
The concepts from CSP have influenced many real world designs,
for example the event-based synchronization in languages like Go and Erlang.

## CSPM and tool support
Several tools are available for checking CSP specifications.
FDR, ProB and this Haskell tools use an input language called CSPM.
CSPM (the machine-readable syntax for CSP) is a small programming language
that combines the core CSP constructs like processes and events with a small
functional programming language.
CSPM is a specification language not a general purpose programming language.
Tools can automatically check CSPM code for properties
like absense of deadlock or safety properties.

## Haskell CSPM tools.
The Repository contains several Haskell cabal packages:

* [![Hackage](https://img.shields.io/hackage/v/CSPM-Frontend.svg)](http://hackage.haskell.org/package/CSPM-Frontend) CSPM-Frontend  (parser for CSPM-syntax)
* [![Hackage](https://img.shields.io/hackage/v/CSPM-CoreLanguage.svg)](http://hackage.haskell.org/package/CSPM-CoreLanguage) CSPM-CoreLanguage (definition of processes and events)
* [![Hackage](https://img.shields.io/hackage/v/CSPM-FiringRules.svg)](http://hackage.haskell.org/package/CSPM-FiringRules) CSPM-FiringRules (semantics for Processes and events)
*  [![Hackage](https://img.shields.io/hackage/v/CSPM-Interpreter.svg)](http://hackage.haskell.org/package/CSPM-Interpreter) CSPM-Interpreter (functional sub-language)
*  [![Hackage](https://img.shields.io/hackage/v/CSPM-ToProlog.svg)](http://hackage.haskell.org/package/CSPM-ToProlog) CSPM-ToProlog (dump CSPM syntax trees as Prolog)
*  [![Hackage](https://img.shields.io/hackage/v/CSPM-cspm.svg)](http://hackage.haskell.org/package/CSPM-cspm) CSPM-cspm (main executable)

## Executable
The CSPM-cspm package provides the _cspm_ executable.
The _cspm_ executable serves as a command line interface to the other CSPM libraries.
It can be used to parse, translate and interpret CSPM specifications and to
compute the labeled transition system (LTS) of a specification.
