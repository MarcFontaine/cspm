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

* CSPM-Frontend (parser for CSPM-syntax)
* CSPM-CoreLanguage (definition of processes and events)
* CSPM-FiringRules (semantics for Processes and events)
* CSPM-Interpreter (functional sub-language)
* CSPM-ToProlog (dump CSPM syntax trees as Prolog)
* CSPM-cspm (main executable)

## Executable
The CSPM-cspm package provides the _cspm_ executable.
The _cspm_ executable serves as a command line interface to the other CSPM libraries.
It can be used to parse, translate and interpret CSPM specifications and to
compute the labeled transition system (LTS) of a specification.
