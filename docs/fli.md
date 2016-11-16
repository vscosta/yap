The Foreign Code Interface    {#fli}
===========================

YAP provides the user with three facilities for writing
predicates in a language other than Prolog. Under Unix systems,
most language implementations were linkable to `C`, and the first interface exported  the YAP machinery to the C language. YAP also implements most of the SWI-Prolog foreign language interface.
This gives portability with a number of SWI-Prolog packages and avoids garnage collection by using @ref slotInterface. Last, a new C++ based interface is
being designed to work with the swig (www.swig.orgv) interface compiler.

+ The @ref c-interface exports the YAP engine.

+ The @ref swi-c-interface emulates Jan Wielemaker's SWI foreign language interface.

+ The @ref  yap-cplus-interface is desiged to interface with the SWIG package by using Object-Oriented concepts

+ The @ref LoadInterface handles the setup of foreign files
