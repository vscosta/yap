# The YAP4Py YAP to Prolog interface. {#YAP4py}

@defgroup YAP4Py Calling Prolog from Python
@ingroup Python
@{
YAP4Py is an object oriented extension of Python. It includes three main components
- it uses the swig C/C++ translator to export YAP's C++ interface into a set of Python
classes and methods.
- it uses the YAP Python-Prolog translation library to implement the main objects in Python, reducing overheads and avoiding SWIG programming.
- it supports Python classes specific to Python, namely named records.


You can run a basic demo of the YAP4Py by calling the `yapi` package:
```
import yap4py
```

It includes the following modules:

1. [Shell Example](namespaceyap4py_1_1yapi.md)

supported by:

1. [Entry Point](namespaceyap4py_1_1____main____.md)

2. [Predicate Support](namespaceyap4py_1_1predicates.md)

3. [Query Support](namespaceyap4py_1_1queries.md)

4. [Pre defined tuples](namespaceyap4py_1_1systuples.md)

All of them rely on the SWIG generated:

1. [Base Interface](namespaceyap4py_1_1yap.md)



#}