
@page builtins Core Built-ins

This chapter describes the core built-in predicates that control the
execution of Prolog programs, provide fundamental functionality such
as term manipulation or arithmetic, and support interaction with
external resources.


- Execution Control
  + @ref YAPControl
  + @ref TopLevel
  + @ref CatchThrow
    + @ref Undefined_Procedures
  + @ref Sets
  + @ref Profiling
    + @ref Call_Counting
  + @ref Hacks

- Meta-programming and Term Manipulation:
  + @ref Predicates_on_Atoms
  + @ref Comparing_Terms
  + @ref YAPStyleChecker

+ @ref arithmetic
  + @ref CompiledExpression

- Data-Base and Global Status
  + @ref Database
  + @ref YAPPredDecls
  + @ref Internal_Database
  + @ref YAPFlags
  + @ref Statistics

- Input/Output and OS
  + @ref YAPOS
  + @ref absf
    + @ref pathconf

- Other
  + @ref Dialects
  + @ref SWI-error
  + @ref Grammars
  + @ref MixBag
  + @ref Ypp

Many of the predicates described here have been standardised by the International Standard Organization.
 The corresponding standartised subset of Prolog also known as ISO-Prolog.    

In the description of the arguments of predicates the following
notation will be used:

+ a preceding plus sign will denote an argument as an "input argument" - the argument is read, not written, and it cannot be a free variable at the time of the call;

+ a preceding minus sign will denote an "output argument";

+ an argument with no preceding symbol can be used in both ways.


