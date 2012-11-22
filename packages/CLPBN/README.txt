Prolog Factor Language (PFL)

Prolog Factor Language (PFL) is a extension of the Prolog language that
allows a natural representation of this first-order probabilistic models
(either directed or undirected). PFL is also capable of solving probabilistic 
queries on this models through the implementation of several inference
techniques: variable elimination, belief propagation, lifted variable
elimination and lifted belief propagation.

Language
-------------------------------------------------------------------------------
A graphical model in PFL is represented using parfactors. A PFL parfactor
has the following four components:

Type ; Formulas ; Phi ; Constraint .

- Type refers the type of the network over which the parfactor is defined.
It can be bayes for directed networks, or markov for undirected ones.
- Formulas is a sequence of Prolog terms that define sets of random variables
under the constraint.
- Phi is either a list of parameters or a call to a Prolog goal that will
unify its last argument with a list of parameters.
- Constraint is a list (possible empty) of Prolog goals that will impose
bindings on the logical variables that appear in the formulas.

The "examples" directory contains some popular graphical models described
using PFL.

Querying
-------------------------------------------------------------------------------
Now we show how to use PFL to solve probabilistic queries. We will
use the burlgary alarm network as an example. First, we load the model:

$ yap -l examples/burglary-alarm.yap

Now let's suppose that we want to estimate the probability of a earthquake
ocurred given that mary called. We can do it with the following query:

?- earthquake(X), mary_calls(t).

Suppose now that we want the joint distribution for john_calls and
mary_calls. We can obtain this with the following query:

?- john_calls(X), mary_calls(Y).


Inference Options
-------------------------------------------------------------------------------
PFL supports both ground and lifted inference. The inference algorithm
can be chosen using the set_solver/1 predicate. The following algorithms
are supported:
- lve:  generalized counting first-order variable elimination (GC-FOVE)
- hve:  (ground) variable elimination
- lbp:  lifted first-order belief propagation
- cbp:  counting belief propagation
- bp:   (ground) belief propagation
- lkc:  lifted first-order knowledge compilation

For example, if we want to use ground variable elimination to solve some
query, we need to call first the following goal:

?- set_solver(hve).

It is possible to tweak several parameters of PFL through the
set_horus_flag/2 predicate. The first argument is a key that
identifies the parameter that we desire to tweak, while the second
is some possible value for this key.

The verbosity key controls the level of log information that will be
printed by the corresponding solver. Its possible values are positive
integers. The bigger the number, more log information will be printed.
For example, to view some basic log information we need to call the 
following goal:

?- set_horus_flag(verbosity, 1).

The use_logarithms key controls whether the calculations performed
during inference should be done in the log domain or not. Its values
can be true or false. By default is false.

There are also keys specific to the inference algorithm. For example,
elim_heuristic key controls the elimination heuristic that will be
used by ground variable elimination. The following heuristics are
supported:
- sequential
- min_neighbors
- min_weight
- min_fill
- weighted_min_fill

An explanation of this heuristics can be found in Probabilistic Graphical
Models by Daphne Koller.

The schedule, accuracy and max_iter keys are specific for inference
algorithms based on message passing, namely lbp, cbp and bp.
The key schedule can be used to specify the order in which the messages
are sent in belief propagation. The possible values are:
- seq_fixed: at each iteration, all messages are sent in the same order
- seq_random: at each iteration, the messages are sent with a random order
- parallel: at each iteration, the messages are all calculated using the
values of the previous iteration.
- max_residual: the next message to be sent is the one with maximum residual,
(Residual Belief Propagation:Informed Scheduling for Asynchronous Message
Passing)

The max_iter key sets the maximum number of iterations. One iteration
consists in sending all possible messages. The accuracy key indicate
when we should stop sending messages. If the largest difference between
a message sent in the current iteration and one message sent in the previous
iteration is less that accuracy value given, we terminate belief propagation.

