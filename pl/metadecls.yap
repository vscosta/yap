
/**
  * @file   metadecls.yap
  * @author VITOR SANTOS COSTA <vsc@vcosta-laptop.dcc.fc.up.pt>
  * @date   Sat Apr  7 03:08:03 2018
  *
  * @brief  meta=declarations, must be run early.
  */

 /**
  *   @addtogroup Meta-Calls The Module System versus the meta-call.
  *   @ingroup YAPMetaPredicates
  *   @{
  *
*/


:- '$system_meta_predicates'([
	abolish(:),
	abolish(:,+),
	alarm(+,0,-),
	all(?,0,-),
	assert(:),
	assert(:,+),
	assert_static(:),
	asserta(:),
	asserta(:,+),
	asserta_static(:),
	assertz(:),
	assertz(:,+),
	assertz_static(:),
	at_halt(0),
	bagof(?,0,-),
	bb_get(:,-),
	bb_put(:,+),
	bb_delete(:,?),
	bb_update(:,?,?),
	call(0),
	call(1,?),
	call(2,?,?),
	call(3,?,?,?),
	call_with_args(0),
	call_with_args(1,?),
	call_with_args(2,?,?),
	call_with_args(3,?,?,?),
	call_with_args(4,?,?,?,?),
	call_with_args(5,?,?,?,?,?),
	call_with_args(6,?,?,?,?,?,?),
	call_with_args(7,?,?,?,?,?,?,?),
	call_with_args(8,?,?,?,?,?,?,?,?),
	call_with_args(9,?,?,?,?,?,?,?,?,?),
	call_cleanup(0,0),
	call_cleanup(0,?,0),
	call_residue(0,?),
	call_residue_vars(0,?),
	call_shared_object_function(:,+),
	clause(:,?),
	clause(:,?,?),
			      current_predicate(:),
			      current_predicate(?,:),
			     depth_bound_call(0,+),
	findall(?,0,-),
	findall(?,0,-,?),
	forall(0,0),
	format(+,:),
	format(+,+,:),
	freeze(?,0),
	hide_predicate(:),
	if(0,0,0),
	ignore(0),
	incore(0),
	initializon(0),
	nospy(:),
        not(0),
        notrace(0),
        once(0),
        phrase(2,?),
        phrase(2,?,+),
	predicate_property(:,?),
	predicate_statistics(:,-,-,-),
	on_exception(+,0,0),
	qsave_program(+,:),
	retract(:),
	retract(:,?),
	retractall(:),
	reconsult(:),
	setof(?,0,-),
	setup_call_cleanup(0,0,0),
	setup_call_catcher_cleanup(0,0,?,0),
	spy(:),
	stash_predicate(:),
	when(+,0),
	with_mutex(+,0),
	with_output_to(?,0),
	'->'(0 , 0),
	'*->'(0 , 0),
	';'(0 , 0),
%	','(0 , 0),
	^(+,0),
	{}(0,?,?),
	','(2,2,?,?),
	';'(2,2,?,?),
	'|'(2,2,?,?),
			      ->(2,2,?,?),
			      \+(2,?,?),
			      \+( 0 )]).

%% @}
