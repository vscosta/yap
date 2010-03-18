/*==============================================================================
 *	LPAD and CP-Logic reasoning suite
 *	File: params.pl
 *	Defines and sets parameters needed by other predicates (main: set).
 *	Copyright (c) 2009, Stefano Bragaglia
 *============================================================================*/
  
:- dynamic setting/2.

% :- source.
% :- yap_flag(single_var_warnings, on).



/* PARAMETER LIST
 * --------------
 * The following list of parameters declares a few constants used along the 
 * program.
 * The default value for each parameter can be changed using the following 
 * predicate:
 *
 *   set(Parameter, Value).
 */

/* epsilon_parsing
 * ---------------
 * This parameter shows the probability's granularity during parsing.
 *
 * Default value:	0.00001
 * Applies to:		parsing
 */
setting(epsilon_parsing, 0.00001).

/* ground_body
 * -----------
 * This parameter tells if both the head and the body of each clause must be 
 * ground, or else the head only.
 * In case the body contains variables not present in the head, it represents 
 * an existential event.
 *
 * Default value:	false
 * Applies to:		parsing
 */
setting(ground_body, false).

/* k
 * -
 * This parameter shows the amount of items of the same type to consider at once.
 *
 * Default value:	64
 * Applies to:		bestfirst, bestk, montecarlo
 */
setting(k, 64).

/* min_error
 * ---------
 * This parameter shows the threshold for the probability interval.
 *
 * Default value:	0.01
 * Applies to:		bestfirst, montecarlo
 */
setting(min_error, 0.01).

/* prob_bound
 * ----------
 * This parameter shows the initial probability bound in a probability bounded 
 * method.
 *
 * Default value:	0.01
 * Applies to:		deepit
 */
setting(prob_bound, 0.001).

/* prob_step
 * ---------
 * This parameter shows the probability deepening step in a probability bounded 
 * method.
 *
 * Default value:	0.001
 * Applies to:		bestfirst, bestk
 */
setting(prob_step, 0.001).

/* save_dot
 * --------
 * This parameter tells if a .dot file for variables has be created.
 *
 * Default value:	false
 * Applies to:		bestfirst, bestk, exact
 */
setting(save_dot, false).

/* timeout
 * -------
 * This parameter shows the time to wait before killing a bdd solving process.
 *
 * Default value:	300
 * Applies to:		bestfirst, bestk, exact
 */
setting(timeout, 300).





/* PREDICATES
 * ----------
 * The predicates in this section interact with the parameters defined above.
 */

/* set(Parameter, Value)
 * ---------------------
 * This predicate drops any previously saved value for the given parameter and 
 * associates it to the new given value.
 * 
 * INPUT
 *  - Parameter: target parameter for the given new value.
 *  - Value: new value to assign to the given parameter.
 */
set(Parameter, Value) :- 
	retract(setting(Parameter, _)), 
	assert(setting(Parameter, Value)).
