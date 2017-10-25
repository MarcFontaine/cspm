# CSPM Interpreter and Animator

This repository contains the Haskell based CSPM tools Dusseldorf.
CSPM is the machine-readable syntax for CSP specifications.
CSP (Communicating Sequential Processes) is a formalism for specifying
and analyzing concurrent systems that communicate via events.

## Libraries
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
