Main Page {#mainpage}
=========

<center>
![The YAP Logo](docs/icons/yap_128x128x32.png)
</center>

NOTE: this version of YAP is still experimental, documentation may be missing or out of date.


The manual is organised as follows:

+ @subpage #Introduction
+ @subpage #install
+ @subpage #run
+ @ref #builtins
+ @ref #extensions
+ @ref #library
+ @ref #packages
+ @ref #YAPProgramming
+ @subpage #fli_c_cxx


##  Introduction {#Introduction}

This document provides User information on version 6.3.4 of
YAP (<em>Yet Another Prolog</em>). The YAP Prolog System is a
high-performance Prolog compiler developed at Universidade do
Porto.

\author Vitor Santos Costa,
\author Luís Damas,
\author Rogério Reis
\author Rúben Azevedo


© 1989-201 L. Damas, V. Santos Costa and Universidade
do Porto.
Permission is granted to make and distribute verbatim copies of this manual provided the copyright notice and this permission notice are preserved on all copies.
Permission is granted to copy and distribute modified versions of this manual under the conditions for verbatim copying, provided that the entire resulting derived work is distributed under the terms of a permission notice identical to this one.
Permission is granted to copy and distribute translations of this manual into another language, under the above conditions for modified versions.

This manual was written by Vítor Santos Costa,
Luís Damas, Rogério Reis, and Rúben Azevedo. The
manual is largely based on the DECsystem-10 Prolog User's Manual by
D.L. Bowen, L. Byrd, F. C. N. Pereira, L. M. Pereira, and
D. H. D. Warren. We have  used comments from the Edinburgh Prolog
library written by R. O'Keefe. Documentation from many built-ins is
originally from the SWI-Prolog manual, with the gracious authorization
from
Jan Wielemaker. We would also like to gratefully
acknowledge the contributions from Ashwin Srinivasian.


@defgroup 	builtins Core	Builtins
@ingroup mainpage
@{

This chapter describes the core built-in predicates  that control the execution of
Prolog programs, provide fundamental functionality such as termm manipulation or arithmetic, and support interaction with external
resources.

[TOC]

@}			

@defgroup  extensions Extensions to core Prolog
@ingroup mainpage
@{

YAP includes a number of extensions over the original Prolog
language. Next, we discuss how to use the most important ones.

[TOC]

@}


@defgroup AttributedVariables Attributed Variables
@ingroup extensions

@{

YAP includes a number of extensions over the original Prolog
language. Next, we discuss how to use the most important ones.

[TOC]

@}

extensions Extensions to core Prolog

@{

YAP includes a number of extensions over the original Prolog
language. Next, we discuss how to use the most important ones.

[TOC]

@}


@defgroup library YAP Prolog Library
@ingroup mainpage
	  @{

## The YAP Library {#library}}

 YAP supports
 the library_directory path (set by the
  `LIBDIR` variable in the Makefile for YAP). Several files in the
  library are originally from the public-domain Edinburgh Prolog library.

[TOC]

@}


@defgroup packages Packages
@ingroup mainpage
@{

Packages are Prolog libraries that implement major extensions ti YAP
functionality eg, new languages or interfaces.

[TOC]

@}


@}

@defgroup YAPProgramming Programming in YA
@ingroup mainpage

We present the main predicates and directives available to load
    files and to program the Prolog environment.

[TOC]

@}

@defgroup fli_c_cxx The Foreign Code Interface
@ingroup mainpage
@{

YAP provides the user with three facilities for writing
predicates in a language other than Prolog. Under Unix systems,
most language implementations were linkable to `C`, and the first interface exported  the YAP machinery to the C language. YAP also implements most of the SWI-Prolog foreign language interface.
This gives portability with a number of SWI-Prolog packages and avoids garnage collection by using @ref slotInterface. Last, a new C++ based interface is
being designed to work with the swig (www.swig.orgv) interface compiler.


[TOC]


@{

YAP provides the user with three facilities for writing
predicates in a language other than Prolog. Under Unix systems,
most language implementations were linkable to `C`, and the first interface exported  the YAP machinery to the C language. YAP also implements most of the SWI-Prolog foreign language interface.
This gives portability with a number of SWI-Prolog packages and avoids garnage collection by using @ref slotInterface. Last, a new C++ based interface is
being designed to work with the swig (www.swig.orgv) interface compiler.


[TOC]

@}