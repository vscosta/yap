# Programming With YAP and Programming YAP

This group will store information on how best to use YAP, including
information about the YAP internals:

+ @subpage syntax.md

+ @ref Indexing

+ @ref Deb_Interaction

+ @ref YAPCompilerSettings

+ @ref YAPStyl

+ @ref YAPImplementation

/**

   @defgroup YAPStyl Making Prolog programs run (fast)
   @ingroup programming

   @{

   We next discuss several issues on trying to make Prolog programs run
   fast in YAP. We assume two different programming styles:

   + Evaluation of <em>deterministic</em> programs often
   boils down to a recursive loop of the form:

   ~~~~~
   loop(Done).
   loop(Env) :-
   do_something(Env,NewEnv),
   loop(NewEnv).
   ~~~~~

   or to the repeat-fail loop:

   ~~~~~
   loop(Inp) :-
   do_something(Inp,Out),
   out_and_fail(Out).
   ~~~~~




