
:- initialization((
	logtalk_load(library(types_loader), [reload(skip)]),
	logtalk_load([hanoi, queens]))).
