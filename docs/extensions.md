
## [Extensions to core Prolog.](extensions)

<!-- @{ -->


YAP includes a number of extensions over the original Prolog
language. Next, we discuss how to use the most important ones.


+ [Attributed Variables](AttributedVariables) use Holzbaur´s extension
to implement co-routinining.
   * [ SICStus style attribute declarations](sicsatts.md)

+ [ Tabling](Tabling) includes the support for suspending and
restarting searches and for storing solutions to a goal so that they
can be reused by a variant goal.

+ [ Threads](Threads.md) allow sharing the same data-base between multiple
co
+ [ Extended Abstract Machine](EAM.md)
ntrol units, in a way that is compatible with other Prolog systems.

+ [ User Defined Extensions](UDI.md) allow the user to create code that can
be integrated as an extension to the engine.

+ [ Loading Large Tables](YAPBigLoad.md) improves time and space needed to run large programs.

+ [ Depth Limited Search](DepthLimited.md) imposes boundaries on the search.

+ [Global_Variables](Global_Variables.md)

  