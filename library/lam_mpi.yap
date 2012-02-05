% Author: Nuno A. Fonseca
% Date: 2006-06-01
% $Id: lam_mpi.yap,v 1.1 2006-06-04 18:43:38 nunofonseca Exp $

:- module(lam_mpi, [
                  mpi_init/0,
                  mpi_finalize/0,
                  mpi_comm_size/1,
                  mpi_comm_rank/1,
                  mpi_version/2,
		  mpi_send/3,
		  mpi_isend/4,
		  mpi_recv/3,
		  mpi_irecv/3,
		  mpi_wait/2,
		  mpi_wait_recv/3,
		  mpi_test/2,
		  mpi_test_recv/3,
		  mpi_bcast/2,
		  mpi_ibcast2/2,
		  mpi_ibcast2/3,
		  mpi_bcast2/2,
		  mpi_bcast2/3,
		  mpi_barrier/0,
		  mpi_msg_buffer_size/2,
		  mpi_msg_size/2,
		  mpi_gc/0
          ]).

:- load_foreign_files([yap_mpi], [], init_mpi).

mpi_msg_size(Term, Size) :-
	terms:export_term(Term, Buf, Size),
	terms:import_term(Buf, _).

