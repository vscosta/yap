#  Core Prolog {#Builtins}


@defgroup Builtins Core Prolog YAP
@ingroup mainpage 

 The manual describes the core features of the Prolog engine.

Prolog programs require features such as control and pruning
primitives, term utilities, data-base management, answer collection,
and list sorting. They also must perform stream based Input/Output and
the string and character manipulation primitives.

The predicates covered here largely corresponds to the Prolog standard
defined as the ISO/IEC 13211-1:1995 language standard~\cite{},
although YAP does not provide compliance.  The standard provides a
minimal Prolog such that compliant programs run in most existing
Prolog systems, both commercial and shared. In pratice, most programs
require functionality outside the standard, such as Operating System
Integration. In these cases, YAP follows  a reference system, currently SWI-Prolog.




 @defgroup InputOutput YAP  support for InputOutput
@ingroup Builtins




