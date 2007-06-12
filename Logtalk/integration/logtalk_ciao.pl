
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.30.1
%
%  Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- ensure_loaded('$LOGTALKUSER/configs/ciao_aux.config').
:- set_prolog_flag(multi_arity_warnings, off).
:- ensure_loaded('$LOGTALKHOME/compiler/logtalk.pl').
:- ensure_loaded('$LOGTALKUSER/libpaths/libpaths.pl').
:- op(600, xfy, ::).
:- op(600, fy, ::).
:- op(600, fy, ^^).
:- op(200, fy, +).
:- op(200, fy, ?).
:- op(200, fy, @).
:- op(200, fy, -).
