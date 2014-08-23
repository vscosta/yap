
:- module(mpe,[
	      mpi_open/0,
	      mpi_start/0,
	      mpi_close/1,
	      mpi_create_event/1,
	      mpi_create_state/4]).

:- load_foreign_files([mpe], [], init_mpe).


