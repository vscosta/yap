/****************************************
  File:     rltree.yap
  Author:   Nuno A. Fonseca
  Comments: Range-List (RL) tree data structure implementation for YAP
  version:  $Id: rltree.yap,v 1.1 2008-03-26 23:05:22 nunofonseca Exp $
****************************************/

:- module(rltree, [
		  rl_new/2,       %% (+Maximum Interval value, -Range-List Id)
		  rl_free/1,      %% (+Range-List Id)
		  rl_size/2,      %% (+Range-List Id,-Size in bytes)      
		  rl_copy/2,      %% (+Range-List Id,-New Range-List Id) - copies one rl_tree
		  rl_set_out/2,   %%(+Range-List Id,+Number) - removes Number from the range-list
		  rl_in/2,        %%(+Range-List Id,?Number) - checks if a number is in the rl-tree
		  rl_set_in/2,    %%(+Range-List Id,+Number)
		  rl_set_all_in/1,%%(+Range-List Id) 
		  rl_print/1,     %%(+Range-List Id) 
		  rl_freeze/1     %%(+Range-List Id) 
          ]).

:- load_foreign_files([yap_rl], [], init_rl).
