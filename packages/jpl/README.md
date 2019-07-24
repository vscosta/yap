# JPL - Java <-> SWI Prolog Interface

JPL is a set of Java classes and C functions providing a **bidirectional interface between Java and Prolog**.  JPL uses the Java Native Interface (JNI) to connect to a Prolog engine through the Prolog Foreign Language Interface (FLI).  JPL is not a pure Java implementation of Prolog; it makes extensive use of native implementations of Prolog on supported platforms.

In its current version, JPL supports the _embedding of a Prolog engine within the Java VM_ as well as the _embedding of a Java VM within Prolog_, so that, for example, one could take advantage of the rich class structure of the Java environment from within Prolog.

JPL is designed in two layers, a low-level interface to the Prolog FLI and a high-level Java interface for the Java programmer who is not concerned with the details of the Prolog FLI.  The low-level interface is provided for C programmers who may wish to port their C implementations which use the FLI to Java with minimal fuss. The current version of JPL only works with [SWI-Prolog](http://www.swi-prolog.org/).

JPL has been integrated into the full [SWI-Prolog](http://www.swi-prolog.org/) distribution starting with version 5.4.x and is included in the binary packages provided by swi-prolog.org.  Binary packages provided by 3rd parties may differ, not providing JPL or providing it as a separate package.

## Further documentation

- Overview documentation is maintained at this **[JPL Wiki](https://github.com/ssardina-research/packages-jpl/wiki).**
- The Prolog API reference is provided from the **[SWI-Prolog manual](http://www.swi-prolog.org/pldoc/doc_for?object=section%28%27packages/jpl.html%27%29)**

----------------------

## OBJECTIVES

The objectives of JPL are to:

1. enable Prolog applications to exploit any Java classes, instances, methods etc. (without requiring any wrappers, metadata etc. to be set up first);
2. enable Java applications to manipulate any Standard Prolog libraries, predicates, etc. (without requiring any wrappers, metadata etc. to be set up first); and
3. enable hybrid Prolog+Java applications to be designed and implemented so as to take best advantage of both language systems, and to be testable, debuggable, maintainable.

.. while also aim for:

* minimum impact deployability: runtime support for Prolog+Java apps must be a position-independent, self-sufficient filestore tree, requiring no changes to registries, system libraries, system configuration files etc.
* minimum dependency deployability: as with JVMs, the Prolog+Java runtime support must depend upon nothing which cannot be taken for granted in healthy OS installations
* minimum vulnerability deployability: the Prolog+Java runtime support must be immune to legitimate variations in its environment (PATH settings, other applications and libraries including other Prolog+Java apps, etc.)

## LICENSE

JPL is released under the terms of the Simplified BSD License. See [LICENSE](LICENSE) file.




