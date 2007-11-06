
:- initialization((
	logtalk_load(library(random_loader), [reload(skip)]),	% allow for static binding
	logtalk_load(blackboard))).
