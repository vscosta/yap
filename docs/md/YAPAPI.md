#  The YAP Foreign Language Interface {#YAPAPI}


@defgroup YAPAPI The YAP Foreign Language
@ingroup mainpage

@brief The Foreign Language Interfaces


YAP provides the user with three facilities for writing
predicates in a language other than Prolog.

Under Unix systems,
most language implementations were linkable to `C`, and the first interface exported  the YAP machinery to the C language.

2. YAP also implements most of the SWI-Prolog foreign language interface.
This gives portability with a number of SWI-Prolog packages and avoids garbage collection by using @ref slotInterface.

3. A  C++ based interface, @ref supports access to complex data, such as the XML interface, and providees elegant communications with object-oriented languages.	 The classes and methods have been influenced by Singleton's JPL.




@defgroup Implementation  YAP Implementation nodes
@ingroup  YAPAPI

@brief The Implementation Section includes extra information on how YAP works.






