
:- module(mpi,[
	      mpi_open/3,
	      mpi_close/0,
	      mpi_send/3,
	      mpi_receive/3,
	      mpi_bcast/3,
	      mpi_bcast/2,
	      mpi_barrier/0]).

:- load_foreign_files([mpi], [], init_mpi).


