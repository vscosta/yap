/**
@defgroup mainpage YAP User Manual

*/

/**

@defgroup Builtins Core Prolog YAP
@ingroup mainpage
@brief The main core features of the Prolog engine

*/

/**

 @defgroup InputOutput YAP  support for InputOutput
@ingroup Builtins

@brief The main core features of the Prolog engine

*/

/**

 @defgroup YAPLibrary The YAP Library
@ingroup mainpage

@brief Describes a number of useful modules.

*/


/**


 @defgroup YAPProgramming Developing Programs in YAP
@ingroup mainpage

we present the main predicates and directives available to load
  files and to control the Prolog environment.

*/



/**


 @defgroup YAPConsulting Loading and Organising YAP Programs
@brief we present the main predicates and directives available to load
   files and to control the Prolog environment.

*/


/**

@defgroup YapExtensions YAP Supported Extensions to Prolog
@ingroup mainpage

@brief Describes major changes to the original Prolog engine

*/

/**


 @defgroup YAPPackages The Prolog packages
@ingroup mainpage

@brief Describes a number of packages that can be used from YAP.

  + @subpage realxplxc

  + @subpage BDDs

  + @subpage  gecode

  + @subpage  myddas

  + @ref PFL

  + @subpage ProbLog1

  + @ref Python

  + @subpage raptor

  + @subpage SAT

  + @subpage XML2PL

  + @ref YAP-LBFGS

  + @subpage yap-udi-indexers

  Leuven packages ported from SWI-Prolog:

  + @subpage chr

  + @subpage clpqr


/**



 @defgroup YAPAPI The YAP Foreign Language
@ingroup mainpage

@brief The Foreign Language Interfaces


YAP provides the user with three facilities for writing
predicates in a language other than Prolog. Under Unix systems,
most language implementations were linkable to `C`, and the first interface exported  the YAP machinery to the C language. YAP also implements most of the SWI-Prolog foreign language interface.
This gives portability with a number of SWI-Prolog packages and avoids garbage collection by using @ref slotInterface. Last, a new C++ based interface is
being designed to work with the swig (www.swig.org) interface compiler.



*/

