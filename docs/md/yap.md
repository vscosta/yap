 YAP 6-3.5 Manual                         {#mainpage}
====================

<center>
![The YAP Logo](docs/icons/yap_128x128x32.png)
</center>

NOTE: this version of YAP is still experimental, documentation may be missing or brout of date.

## Introduction

This document provides User information on version 6.3.4 of
YAP (<em>Yet Another Prolog</em>). The YAP Prolog System is a
high-performance Prolog compiler developed at Universidade do
Porto.

The manual is organised as follows:


+ @subpage  install

+ @subpage run

+ @subpage load_files

+ @subpage builtins

+ @subpage extensions

+ @subpage library

+ @subpage packages

+ @subpage YAPProgramming

+ @subpage fli_c_cxx


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


@page Library YAP Library


 the library_directory path (set by the
  `LIBDIR` variable in the Makefile for YAP). Several files in the
  library are originally from the public-domain Edinburgh Prolog library.

 

@page Extensions  YAP Extensions

YAP includes a number of extensions over the original Prolog
language.

  + @subpage atts

  + @ref Rational_Trees
  
  + @ref CohYroutining

  + @ref Attributed_Variables

  + @ref  DepthLimited

  + @ref  Tabling

  + @ref Threads

  + @ref Profiling

  + @ref YAPArrays

  + @ref Parallelism


@page YAPProgramming Programming in YAP

    @subpage yapsyntax.md


@page packages Packages for YAP
YAP includes a number of packages.
 
 @subpage real.md

 @subpage chr.md


  
