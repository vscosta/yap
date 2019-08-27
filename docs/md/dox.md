#  YAP 6-3.5 Manual {#mainpage}

@defgroup mainpage YAP 6-3.5 Manual
@{

<center>
![The YAP Logo](docs/icons/yap_128x128x32.png)
</center>

NOTE: this version of YAP is still experimental, documentation may be missing or out of date.


The manual is organised as follows:

[TOC]

@}

@defgroup  Introduction Introduction
@ingroup mainpage

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


@}

@defgroup builtins Core Built-ins
@ingroup mainpage

@{



This chapter describes the core built-in predicates  that control the execution of
Prolog programs, provide fundamental functionality such as termm manipulation or arithmetic, and support interaction with external
resources.

[TOC]

@}

@defgroup extensions Extensions to core Prolog
@ingroup maipage

@{

YAP includes a number of extensions over the original Prolog
language. Next, we discuss how to use the most important ones.

[TOC]

@}


defgroup library YAP Prolog Library

@{

 YAP supports
 the library_directory path (set by the
  `LIBDIR` variable in the Makefile for YAP). Several files in the
  library are originally from the public-domain Edinburgh Prolog library.

[TOC]

 @}


