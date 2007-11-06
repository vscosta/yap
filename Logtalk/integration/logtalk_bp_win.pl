
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.30.7
%
%  Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- set_prolog_flag(redefined, off).
:- cl('$LOGTALKUSER/configs/b.config').
%:- system('del %LOGTALKUSER%\\configs\\b.config.out').
:- (expand_environment('$LOGTALKHOME/compiler/logtalk.pl.out', Expanded), exists(Expanded) -> load('$LOGTALKHOME/compiler/logtalk.pl'); cl('$LOGTALKHOME/compiler/logtalk.pl')).
:- cl('$LOGTALKUSER/libpaths/libpaths.pl').
%:- system('del %LOGTALKUSER%\\libpaths\\libpaths.pl.out').
