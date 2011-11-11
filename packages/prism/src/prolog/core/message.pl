%% -*- Prolog -*-

%%========================================================================
%%
%% This module contains a set of error and warning messages displayed in
%% the Prism system.  Each message entry has the following form:
%%
%%     $pp_message(ID,Type,Message)
%%
%% <ID> is a positive integer that identifies the message.
%%
%% <Type> denotes the message type, which is one of the following:
%%
%%     * fatal
%%     * inter(nal error)
%%     * error
%%     * fail
%%     * warn
%%     * obsol(ete)
%%     * info
%%
%% <Message> is (to be written).
%%
%%========================================================================

%%
%% Errors related to probabilistic models
%%

% Errors related to probabilities
$pp_message($msg(0000),error,"invalid probability -- {1}").
$pp_message($msg(0001),error,"invalid probability list -- {1}").
$pp_message($msg(0002),error,"invalid ratio list -- {1}").
$pp_message($msg(0003),error,"invalid probabilistic atomic formula -- {1}").
$pp_message($msg(0004),error,"invalid user-defined probabilistic atomic formula -- {1}").
$pp_message($msg(0005),error,"invalid extended probabilistic atomic formula -- {1}").
$pp_message($msg(0006),error,"invalid tabled probabilistic atomic formula -- {1}").
$pp_message($msg(0007),error,"invalid probabilistic callable -- {1}").

% Errors related to random switches
$pp_message($msg(0100),error,"no multi-valued switch declarations given").
$pp_message($msg(0101),error,"non-ground switch name -- {1}").
$pp_message($msg(0102),error,"outcome space not given -- {1}").
$pp_message($msg(0103),error,"probability distribution not given -- {1}").
$pp_message($msg(0104),error,"hyperparameters not given -- {1}").
%$pp_message($msg(0105),error,"").
$pp_message($msg(0106),error,"modified outcome space; probabilities expected to be unfixed -- {1}").
$pp_message($msg(0107),error,"modified outcome space; obsolete expectations -- {1}").
$pp_message($msg(0108),error,"modified outcome space; hyperparameters expected to be unfixed -- {1}").
$pp_message($msg(0109),warn, "distribution fixed -- {1}").
$pp_message($msg(0110),warn, "hyperparameters fixed -- {1}").

% Errors related to distribution
$pp_message($msg(0200),error,"invalid distribution -- {1}").
$pp_message($msg(0201),error,"invalid hyperparameters -- {1}").
$pp_message($msg(0202),error,"default distribution unavailable").
$pp_message($msg(0203),error,"default hyperparameters unavailable").
$pp_message($msg(0204),error,"invalid number of outcomes -- {1}").
$pp_message($msg(0205),error,"invalid switch configuration -- {1}").
%$pp_message($msg(0206),error,"").
%$pp_message($msg(0207),error,"").
$pp_message($msg(0208),error,"invalid alpha values -- {1}").
$pp_message($msg(0209),error,"invalid delta values -- {1}").
$pp_message($msg(0210),error,"distribution does not match -- ({1},{2})").
$pp_message($msg(0211),error,"size unmatched -- ({1},{2})").

%%
%% Errors related to built-ins for probabilistic inferences
%%

% Errors in loading
$pp_message($msg(1000),error,"invalid filename -- {1}").
$pp_message($msg(1001),error,"invalid PRISM option -- {1}").
$pp_message($msg(1002),warn, "tabling disabled in the consultation mode").
$pp_message($msg(1003),error,"batch file not specified").
$pp_message($msg(1004),error,"prism_main/0-1 undefined -- {1}").
$pp_message($msg(1005),error,"invalid module for prism or upprism").

% Errors in translation
$pp_message($msg(1100),fail ,"bad or duplicate predicate -- {1}").
$pp_message($msg(1101),error,"co-existing p_table and p_not_table declarations").
$pp_message($msg(1102),error,"invalid predicate indicator -- {1}").
$pp_message($msg(1103),error,"invalid call in write_call").
$pp_message($msg(1104),warn, "parameters left unset/unfixed; ground terms expected -- values_x({1},_,{2})").
$pp_message($msg(1105),error,"invalid outcome space; ground list expected").

% Errors in sampling
$pp_message($msg(1201),error,"invalid goal; probabilistic goal expected -- {1}").
$pp_message($msg(1202),error,"invalid constraint; callable term expected -- {1}").
$pp_message($msg(1203),error,"invalid number of samples; positive integer expected -- {1}").
$pp_message($msg(1204),error,"invalid number of trials; `inf' or positive integer expected -- {1}").

% Errors in EM learning
$pp_message($msg(1300),error,"no observed data; the data_source flag set to `none'").
$pp_message($msg(1301),error,"no observed data; data/1 undefined").
$pp_message($msg(1302),error,"invalid observed data -- {1}").
$pp_message($msg(1303),error,"invalid observed goal; tabled probabilistic atomic formula expected -- {1}").
$pp_message($msg(1304),error,"no explanations -- {1}").
$pp_message($msg(1305),error,"DAEM not applicable to models with failure").
$pp_message($msg(1306),error,"invalied goal count; positive integer expected -- {1}").

% Errors in other probabilistic inferences
$pp_message($msg(1400),error,"invalid number of top-ranked expls; positive integer expected -- {1}").
$pp_message($msg(1401),error,"invalid number of intermediate candidate expls; positive integer expected -- {1}").
$pp_message($msg(1402),error,"invalid subgoal aggregation pattern -- {1}").
$pp_message($msg(1403),error,"invalid subgoal pattern -- {1}").
$pp_message($msg(1404),warn, "subgoals unmatched").
$pp_message($msg(1405),error,"invalid subgoal argument; integer expected -- {1}").
$pp_message($msg(1406),error,"invalid subgoal argument; atom expected -- {1}").
$pp_message($msg(1407),error,"invalid subgoal argument; ground compound expected -- {1}").
$pp_message($msg(1408),error,"invalid subgoal argument; list expected -- {1}").
$pp_message($msg(1409),error,"invalid subgoal argument; d-list expected -- {1}").

%%
%% Errors related to built-ins for auxiliary operations
%%

% Errors in random operations
$pp_message($msg(2000),error,"invalid random seed -- {1}").
$pp_message($msg(2001),error,"invalid random state -- {1}").
$pp_message($msg(2002),error,"invalid max value; positive integer expected -- {1}").
$pp_message($msg(2003),error,"invalid min value; integer expected -- {1}").
$pp_message($msg(2004),error,"invalid max value; integer expected -- {1}").
$pp_message($msg(2005),error,"invalid max value; positive number expected -- {1}").
$pp_message($msg(2006),error,"invalid min value; number expected -- {1}").
$pp_message($msg(2007),error,"invalid max value; number expected -- {1}").
$pp_message($msg(2008),error,"invalid min/max pair -- ({1},{2})").
$pp_message($msg(2009),error,"invalid mu; number expected -- {1}").
$pp_message($msg(2010),error,"invalid sigma; positive number expected -- {1}").
$pp_message($msg(2011),error,"invalid elements; list expected -- {1}").
$pp_message($msg(2012),error,"invalid number of selections; integer expected -- {1}").
$pp_message($msg(2013),error,"number of selections out of range -- {1}").
$pp_message($msg(2014),error,"invalid number of groups; positive integer expected -- {1}").

% Errors in list handling
$pp_message($msg(2100),error,"invalid predicate name -- {1}").
$pp_message($msg(2101),error,"invalid unary operator -- {1}").
$pp_message($msg(2102),error,"invalid binary operator -- {1}").
$pp_message($msg(2103),error,"invalid argument; list not shorter than {2} expected -- {1}").
$pp_message($msg(2104),error,"invalid argument; list expected -- {1}").
$pp_message($msg(2105),error,"invalid argument; non-negative integer expected -- {1}").
$pp_message($msg(2106),error,"invalid argument; positive integer expected -- {1}").
$pp_message($msg(2107),error,"invalid agglist operation -- {1}").
$pp_message($msg(2108),error,"invalid argument; list of numbers expected -- {1}").
$pp_message($msg(2109),error,"invalid argument; list or nil expected -- {1}").
$pp_message($msg(2110),error,"invalid argument; list of non-variables expected -- {1}").

%%
%% Miscellaneous errors
%%

% File I/Os
$pp_message($msg(3000),error,"invalid file specification -- {1}").
$pp_message($msg(3001),error,"file not found -- {1}").
$pp_message($msg(3002),error,"unknown or illegal option -- {1}").
$pp_message($msg(3003),error,"duplicate option -- {1}").
$pp_message($msg(3004),error,"no information on the last observation").
$pp_message($msg(3005),error,"too few rows").
$pp_message($msg(3006),error,"too few columns").
$pp_message($msg(3007),error,"parsing failure in CSV format").
$pp_message($msg(3008),warn, "too few rows compared to the specification").

% Execution flags
$pp_message($msg(3100),error,"invalid prism flag -- {1}").
$pp_message($msg(3101),error,"invalid value for {1} -- {2}").
$pp_message($msg(3102),warn, "prism flag replaced by {2} -- {1}").
$pp_message($msg(3103),error,"prism flag deleted in version {2} -- {1}").
$pp_message($msg(3104),error,"prism flag value deleted in version {2} -- {1}").
$pp_message($msg(3105),warn, "prism flag value replaced by {2} -- {1}").

% Write calls
$pp_message($msg(3200),error,"control constructs (other than conjunction) disallowed -- {1}").

% Deprecated predicates
$pp_message($msg(3300),warn, "predicate replaced by {2} -- {1}").
$pp_message($msg(3301),warn, "predicate deprecated -- {1}").

% Math predicates
$pp_message($msg(3400),error,"invalid argument -- {1}").

%%
%% System-related errors
%%

% Internal errors
$pp_message($msg(9800),inter,"error term not found").
$pp_message($msg(9801),inter,"error message not found").
$pp_message($msg(9802),inter,"invalid internal representation").
$pp_message($msg(9803),inter,"unmatched branches").
$pp_message($msg(9804),inter,"unexpected failure").
$pp_message($msg(9805),inter,"failure in hash-id registration -- {1}").

% Fatal errors
$pp_message($msg(9900),fatal,"assertion failure -- {1}").
