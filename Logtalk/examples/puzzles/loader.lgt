
:- initialization((
	logtalk_load(library(types_loader), [reload(skip)]),	% allow for static binding
	logtalk_load([potions, jam_thief, horses, houses, note, camp_swampy]))). 
