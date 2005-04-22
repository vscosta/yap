=================================================================
Logtalk - Object oriented extension to Prolog
Release 2.24.0

Copyright (c) 1998-2001 Paulo Moura.  All Rights Reserved.
=================================================================


This version of Logtalk included in the YAP distribuition does not
contain configuration files for other Prolog compilers. Also, the
Logtalk compiler/pre-processor is contained in the library/logtalk 
directory.

Logtalk can be loaded from YAP either as described in the included 
Logtalk documentation or by using the call:

| ?- use_module(library(logtalk)).

This will provide you with the minimal support for compiling and 
running Logtalk programs. For improved Logtalk + YAP integration, 
please follow the instructions on the Logtalk INSTALL file.

Note that, although we load Logtalk using the use_module/1 built-in 
predicate, the system is not packaged as a module nor does it use 
modules in its implementation.
