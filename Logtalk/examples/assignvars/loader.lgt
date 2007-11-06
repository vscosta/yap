
:- initialization((
	logtalk_load(library(assignvars), [reload(skip)]),	% allow for static binding
	logtalk_load([fsm3, rectangle3]))). 
