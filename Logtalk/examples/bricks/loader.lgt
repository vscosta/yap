
:- initialization((
	logtalk_load(library(all_loader), [reload(skip)]),	% allow for static binding
	logtalk_load(roots(loader), [reload(skip)]),		% allow for static binding
	logtalk_load(relations(loader), [reload(skip)]),	% allow for static binding
	logtalk_load(bricks, [events(on)]))).
