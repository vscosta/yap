## [WWW Reader/Writers for YAP](#raptor)


This provides YAP a rdf reader using
[raptor](http://librdf.org/raptor/). The library is available for
Windows, Linux/Unix and MacOS machines.

### Example Usage

~~~~{.prolog}
?- use_module(rdf).
?- rdf_load('example.rdf',user,example).
../example.rdf : 3 triples

?- example(Subject,Predicate,Object).
Object = 'http://www.example.org/tv_show',
Predicate = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
Subject = 'http://www.example.org/law_and_order_ci' ?

~~~~

The code also includes a library under developent to connect Yap and libXML2.
