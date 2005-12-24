
:- initialization(
	logtalk_load([
		state_space,
		water_jug,
		salt3,
		farmer,
		heuristic_state_space,
		bridge,
		eight_puzzle,
		miss_cann,
		search_strategy,
		blind_search1,
		breadth_first1,
		depth_first1,
		heuristic_search1,
		best_first1,
		hill_climbing1,
		performance],
		[events(on)])).

/*
If you intend to use the FOP XSL:FO processor for generating PDF documenting
files, comment the directive above and uncomment the directive below

:- initialization(
	logtalk_load(
		[state_space,
		water_jug,
		salt3,
		farmer,
		heuristic_state_space,
		bridge,
		eight_puzzle,
		miss_cann,
		search_strategy,
		blind_search1,
		breadth_first1,
		depth_first1,
		heuristic_search1,
		best_first1,
		hill_climbing1,
		performance],
		[events(on), xmlsref(standalone)])).
*/
