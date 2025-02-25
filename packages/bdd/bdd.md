
# YAP2BDD Boolean Decision Making in YAP

@brief low-level support for using BDDs from BDDs

This is an experimental interface to BDD libraries. It is not as
sophisticated as simplecudd, but it provides a way to play around with bdds.

It currently works with cudd only, although it should be possible to
port to other libraries. Binary instances of CUDD are available in Fedora Linux an Homebrew.The sources for CUDD can be found at the [unofficial git mirror](https://github.com/ivmai/cudd.git). 


It requires the ability to dynamically link
with cudd binaries. This works:

- in fedora with standard package
- in osx with hand-compiled and ports package

In ubuntu, you may want to install the fedora rpm, or just download the package from the original
 and compile it.


