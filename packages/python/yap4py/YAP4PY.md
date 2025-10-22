# The YAP4Py YAP to Prolog interface. {#YAP4py}

@defgroup YAP4Py Calling Prolog from Python
@ingroup Python

YAP4Py is an object oriented extension of Python. It includes three main components
- it uses the swig
C/C++ translator to export YAP's C++ interface into a set of Python
classes and methods.

- it uses the YAP Python-Prolog translation library to implement the main objects in Python, reducing overheads and avoiding SWIG programming.

- it supports Python classes specific to Python, namely named records.

You can run a basic demo of the YAP4Py by calling the `yapi` package:
```
import yap4py
```

