This is an experimental port of Yuce Tekol's pyswip package. It
supports for 64 bit code, and should work in SWI-Prolog.

- YAP must be compiled as a dynamic library
- libYap.so must be in the ld path: either use --prefix=/usr or
   LD_LIBRARY_PATH

Only tested in Linux right now. Use

make install
make tests

from packages/pyswip

to see how it goes.

