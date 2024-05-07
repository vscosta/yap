# YAP Documentation                          {#mainpage}

The manual is organised as follows:

* @subpage INSTALL

* @ref Builtins

* @ref    YAPLibrary

* @ref YAPProgramming

 @ref YapExtensions

* @ref YAPPackages


@defgroup Builtins YAP  to Prolog

@ingroup mainpage

@brief Describes the main core features of the Prolog engine

@{

@}


@defgroup InputOutput YAP  support for InputOutput

@ingroup Builtins

@brief Describes how to input and output data in YAP.
@{

@}

@defgroup YAPProgramming Programming in YAP

@ingroup mainpage

@brief we present the main predicates and directives available to load
  files and to control the Prolog environment.
@{

@}

@defgroup load_files Loading and Organising YAP Programs

@ingroup YAPProgramming

@brief we present the main predicates and directives available to load
  files and to control the Prolog environment.

@{

@}

@defgroup YapExtensions YAP Supported Extensions to Prolog

@ingroup mainpage

@brief Describes major changes to the original Prolog engine
@{

@}
@defgroup YAPLibrary The YAP Library

@ingroup mainpage

@brief Describes a number of useful modules.
@{

@}

@defgroup YAPPackages YAP Packages

@ingroup mainpage

@brief Describes a number of packages that can be used from YAP.
@{

@}


@defgroup YAPAPI YAP Foreign Language API

@ingroup mainpage

@brief The Foreign Language Interfaces
@{
YAP provides the user with three facilities for writing
predicates in a language other than Prolog. Under Unix systems,
most language implementations were linkable to `C`, and the first interface exported  the YAP machinery to the C language. YAP also implements most of the SWI-Prolog foreign language interface.
This gives portability with a number of SWI-Prolog packages and avoids garnage collection by using @ref slotInterface. Last, a new C++ based interface is
being designed to work with the swig (www.swig.orgv) interface compiler.

- @subpage fli_c

- @ref  FLI_swi-c-interface

-  @ref FLI_YAP-cplus-interface


@}

