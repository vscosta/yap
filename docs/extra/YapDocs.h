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

@brief   Describes a number of useful modules.

The module supports the library_directory path (set by the LIBDIR variable in the Makefile for YAP). Several files in the library are originally from the public-domain Edinburgh Prolog library.



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
@{
 YAP also distributes the output of number of projects that were originally built or eventually ported to YAP.
 
@}
*/

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

