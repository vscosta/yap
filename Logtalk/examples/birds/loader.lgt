
:- initialization((
	logtalk_load(library(hierarchies_loader), [reload(skip)]),	% allow for static binding
	logtalk_load([descriptors, birds, expert]))).
