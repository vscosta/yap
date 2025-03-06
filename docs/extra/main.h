

/**

#mainpage YAP Documentation

The manual is organised as follows:

- @page INSTALL.md

- @page CALLING_YAP.md

- @ref Builtins

- @ref    YAPLibrary

- @ref YAPProgramming

- @ref YapExtensions

- @ref YAPPackages

*/
@{
   

   
@}
   

   

   

   
*/
   

   
/**
   

   

   

   

   

   
@defgroup YAPAPI YAP Foreign Language API
   

   
@ingroup mainpage
   

   
@brief The Foreign Language Interfaces
   

   
@{
   
YAP provides the user with three facilities for writing
   
predicates in a language other than Prolog. Under Unix systems,
   
most language implementations were linkable to `C`, and the first interface exported  the YAP machinery to the C language. YAP also implements most of the SWI-Prolog foreign language interface.
   
This gives portability with a number of SWI-Prolog packages and avoids garbage collection by using @ref slotInterface. Last, a new C++ based interface is
   
being designed to work with the swig (www.swig.orgv) interface compiler.
   

   

   
@}
    
*/


