
<center>
![The YAP Logo](docs/icons/yap_128x128x32.png)
</center>

NOTE: this version of YAP is still experimental, documentation may be out of date.

## Introduction

This document provides User information on version 6.3.4 of
YAP (<em>Yet Another Prolog</em>). The YAP Prolog System is a
high-performance Prolog compiler developed at Universidade do
Porto.  YAP supports stream Input/Output, sockets, modules,
  exceptions, Prolog debugger, C-interface, dynamic code, internal
  database, DCGs, saved states, co-routining, arrays, threads.

We explicitly allow both commercial and non-commercial use of YAP.


YAP is based on the David H. D. Warren's WAM (Warren Abstract Machine),
with several optimizations for better performance. YAP follows the
Edinburgh tradition, and was originally designed to be largely
compatible with DEC-10 Prolog, Quintus Prolog, and especially with
C-Prolog. More recently, we have worked on being compatible with SICStus Prolog and with SWI-Prolog.

YAP implements most of the ISO-Prolog standard. We are striving at
full compatibility, and the manual describes what is still
missing.
The document is intended neither as an introduction to Prolog nor to the
implementation aspects of the compiler. A good introduction to
programming in Prolog is the book @cite TheArtOfProlog , by
L. Sterling and E. Shapiro, published by "The MIT Press, Cambridge
MA". Other references should include the classical @cite ProgrammingInProlog , by W.F. Clocksin and C.S. Mellish, published by
Springer-Verlag.

YAP 6.3.4 has been built with the gcc and clang compilers on Linux and OSX machines. We expect to recover support for  WIN32 machines and
Android next.

We are happy to include in YAP several excellent packages developed
under separate licenses. Our thanks to the authors for their kind
authorization to include these packages.

The overall copyright and permission notice for YAP4.3 can be found in
the Artistic file in this directory. YAP follows the Perl Artistic
license, and it is thus non-copylefted freeware. Some components of YAP have been obtained from SWI Prolog and ciao, and have
different licenses.

If you have a question about this software, desire to add code, found a
bug, want to request a feature, or wonder how to get further assistance,
please send e-mail to <yap-users AT lists.sourceforge.net>.  To
subscribe to the mailing list, visit the page
<https://lists.sourceforge.net/lists/listinfo/yap-users>.

On-line documentation is available for [YAP](http://www.dcc.fp.pt/~vsc/yap/)



The packages are, in alphabetical order:

+ The CHR package developed by Tom Schrijvers,
Christian Holzbaur, and Jan Wielemaker.

+ The CLP(BN) package and Horus toolkit developed by Tiago Gomes, and Vítor Santos Costa.

+ The CLP(R) package developed by Leslie De Koninck, Bart Demoen, Tom
Schrijvers, and Jan Wielemaker, based on the CLP(Q,R) implementation
by Christian Holzbaur.

+ The CPLint package developed by Fabrizio Riguzzi's research
laboratory at the [University of Ferrara](http://www.ing.unife.it/Docenti/FabrizioRiguzzi/)

+ The CUDA interface package developed by Carlos Martínez, Jorge
Buenabad, Inês Dutra and Vítor Santos Costa.

+ The [GECODE](http://www.gecode.org) interface package developed by  Denys Duchier and Vítor Santos Costa.

+ The [JPL](http://www.swi-prolog.org/packages/jpl/) (Java-Prolog Library) package developed by .

 The minisat SAT solver interface developed by Michael Codish,
 Vitaly Lagoon, and Peter J. Stuckey.

+ The MYDDAS relational data-base interface developed at the
 Universidade do Porto by Tiago Soares, Michel Ferreira, and Ricardo Rocha.

+ The [PRISM](http://rjida.meijo-u.ac.jp/prism/) logic-based
programming system for statistical modeling developed at the Sato
Research Laboratory, TITECH, Japan.

+ The ProbLog 1 system developed by the [ProbLog](https://dtai.cs.kuleuven.be/problog) team in the
DTAI group of KULeuven.

+ The [R](http://stoics.org.uk/~nicos/sware/packs/real/) interface package developed by 	Nicos Angelopoulos,
Vítor Santos Costa, João Azevedo, Jan Wielemaker, and Rui Camacho.
