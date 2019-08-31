		
## [Core Built-ins](builtins)

The YAP core-builtions support the following key features:

- Execution Control
  + [YAPControl](YAPControl.md)
  + [ Top-Level and Boot Predicates](TopLevel.md)
  + [ Catch and Throw](CathThrow.md)
    + [ Handling Undefined Procedures](Undefined_Procedures.md)
  + [ Collecting Solutions to a Goal](Sets.md)
  + [ Profiling Prolog Programs](Profiling.md)
    + [ Counting Calls](Call_Counting.md)
  + [ Low-level access](Hacks.md)

- Meta-programming and Term Manipulation:
  + [atoms](Predicates_on_Atoms.md)
  + [Comparing_Terms](Comparing_Terms.md)
  + [ Style Checker](YAPStyleChecker.md)

+ [arith](arithmetic_preds.md)
  + [ A Compiler for Arithmetic](CompiledExpression.md)

- Data-Base and Global Status
  + [ The Clausal Data Base](Database.md)
  + [ Declaring Properties of Predicates](YAPPredDecls.md)
  + [Internal DB](Internal_Database.md)
  + [ Yap Flags](YAPFlags.md)
  + [ Loading files into YAP](YAPConsulting.md)
  + [ Creating and Using a saved state](QLY.md)
    + [ Freeze System Configuration](ProtectCore.md)
+ [ System Status](Statistics.md)

- Input/Output and OS
  + [ Access to Operating System Functionality](YAPOS.md)
  + [ File Name Resolution](absf.md)
    + [ Configuration of the Prolog file search path](pathconf.md)

- Other
  + [ Compatibility with other Prolog dialects](Dialects.md)
  + [ High-level error testing.](SWI-error.md)
  + [ Grammar Rules](Grammars.md)
  + [ Diverse Utilities](MixBag.md)
  + [ YAP PreProcessing](Ypp.md)


This chapter describes the core built-in predicates  that control the execution of
Prolog programs, provide fundamental functionality such as term manipulation or arithmetic, and support interaction with external
resources.

<!---- @{ ---->

Many of the predicates described here have been standardised by the International Standard Organization.
 The corresponding standartised subset of Prolog also known as ISO-Prolog.    

In the description of the arguments of predicates the following
notation will be used:

+ a preceding plus sign will denote an argument as an "input argument" - the argument is read, not written, and it cannot be a free variable at the time of the call;

+ a preceding minus sign will denote an "output argument";

+ an argument with no preceding symbol can be used in both ways.

<!---- @} ---->
