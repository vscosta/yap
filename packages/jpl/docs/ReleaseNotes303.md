# Release Notes - 3.0.3

## Changes within the distribution

The demo folder has been renamed examples (more idiomatic)(?) and its contents have been moved into a new java folder, which is accompanied by a new prolog folder for Prolog examples

## Java API changes

To simplify the construction of queries, the Query(java.lang.String) constructor now parses its string arg as if it were Prolog source text, and constructs a new query whose goal is the resulting term.  This is backwards compatible with (all but very unusual) previous usage, e.g.
```prolog
new Query("statistics")
```
and allows arbitrarily complex goals to be created textually, e.g.
```prolog
new Query("setof(_A,current_atom(_A),_As),length(_As,N)")
```
NB _A and _As are dont-tell-me variables (this property is determined by their initial underscore), whose bindings are by default not returned when the query is called (saving computational time and space).  This behaviour can be overridden (globally) with
```prolog
jpl.JPL.setDTMMode( false)
```
to allow Java+JPL+Prolog implementation of a Prolog IDE which emulates the behaviour of the traditional top-level interpreter. 

To further simplify construction of queries, the `Query(java.lang.String text, jpl.Term[] args)` constructor now parses its text argument as a Prolog source text fragment; if it represents an atom, the constructor behaves as before (building a Compound goal from the given name and args),
but if it represents a compound term with one or more atomic subterms whose names are a single questionmark character, e.g.
```prolog
"setof(P,mypred(?,P,?),Ps), length(Ps,?)"
```
and the args comprise as many terms as there are questionmarks, then the new query's goal is a rewriting of text's term,
with each questionmark replaced by the corresponding element of args.

This is designed to mimic the established and useful idiom of passing parameters into SQL prepared statements.
It allows all the constant parts of a parameterised query to be defined textually.

Paul Singleton

Friday 12th March 2004
