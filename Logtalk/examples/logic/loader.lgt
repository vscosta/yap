
:- op(10, fy, ~).
:- op(20, yfx, &).
:- op(30, yfx, v).
:- op(40, xfx, =>).
:- op(40, xfx, <=>).

:- initialization(
	logtalk_load(translator)).
