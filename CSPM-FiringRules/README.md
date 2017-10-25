# CSPM-FiringRules

This package contains functions for computing the transitions of a CSP process
based on the standard CSP firing rule semantic
(see The Theory and Practice of Concurrency A.W. Roscoe 1999.)
It also contains a rudimentary tracer for executing transitions,
some QuickCheck tests, and a data type for proof trees.
To use this package one has to provide instances for the classes and type families,
defined in the CSPM-CoreLanguage package.
The package contains two mock-implementations that provide these instances.
The CSPM-Interpreter package contains an other implementation.
