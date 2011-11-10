/* -*- c-basic-offset: 4 ; tab-width: 4 -*- */

#ifdef MPI

#include "bprolog.h"
#include "core/error.h"
#include "up/up.h"
#include "mp/mp.h"
#include "mp/mp_core.h"
#include <unistd.h> /* STDOUT_FILENO */
#include <string.h>
#include <mpi.h>

/*------------------------------------------------------------------------*/

/* cpred.c (B-Prolog) */
int bp_string_2_term(const char *, TERM, TERM);

/*------------------------------------------------------------------------*/

static char str_prealloc[65536];

/*------------------------------------------------------------------------*/

static int send_term(TERM arg, int mode, int rank)
{
    char *str;
    int len;

    str = (char *)bpx_term_2_string(arg);
    len = strlen(str);

    switch (mode) {
    case 0:
        MPI_Send (&len,  1 , MPI_INT , rank, TAG_GOAL_LEN, MPI_COMM_WORLD);
        MPI_Send ( str, len, MPI_CHAR, rank, TAG_GOAL_STR, MPI_COMM_WORLD);
        break;
    case 1:
        MPI_Bcast(&len,  1 , MPI_INT , rank, MPI_COMM_WORLD);
        MPI_Bcast( str, len, MPI_CHAR, rank, MPI_COMM_WORLD);
        break;
    }

    mp_debug("SEND(%d,%d): %s", mode, rank, str);

    return BP_TRUE;
}

static int recv_term(TERM arg, int mode, int rank)
{
    char  *str;
    TERM  op1, op2;
    int   len, res;

    switch (mode) {
    case 0:
        MPI_Recv (&len, 1, MPI_INT, rank, TAG_GOAL_LEN, MPI_COMM_WORLD, NULL);
        break;
    case 1:
        MPI_Bcast(&len, 1, MPI_INT, rank, MPI_COMM_WORLD);
        break;
    }

    if (len < sizeof(str_prealloc))
        str = str_prealloc;
    else {
        str = MALLOC(len + 1);
    }

    switch (mode) {
    case 0:
        MPI_Recv (str, len, MPI_CHAR, rank, TAG_GOAL_STR, MPI_COMM_WORLD, NULL);
        break;
    case 1:
        MPI_Bcast(str, len, MPI_CHAR, rank, MPI_COMM_WORLD);
        break;
    }

    *(str + len) = '\0';

    mp_debug("RECV(%d,%d): %s", mode, rank, str);

    op1 = bpx_build_var();
    op2 = bpx_build_var();

    res = bp_string_2_term(str,op1,op2);
    if (str != str_prealloc) {
        free(str);
    }
    if (res == BP_TRUE) {
        return bpx_unify(arg, op1);
    }
    return res;
}

/*------------------------------------------------------------------------*/

int pc_mp_size_1(void)
{
    return bpx_unify(bpx_get_call_arg(1,1), bpx_build_integer(mp_size));
}

int pc_mp_rank_1(void)
{
    return bpx_unify(bpx_get_call_arg(1,1), bpx_build_integer(mp_rank));
}

int pc_mp_master_0(void)
{
    return (mp_rank == 0) ? BP_TRUE : BP_FALSE;
}

int pc_mp_abort_0(void)
{
    mp_quit(0);
}

int pc_mp_wtime_1(void)
{
    return bpx_unify(bpx_get_call_arg(1,1), bpx_build_float(MPI_Wtime()));
}

int pc_mp_sync_2(void)
{
    int args[2], amin[2], amax[2];

    args[0] = bpx_get_integer(bpx_get_call_arg(1,2)); /* tag */
    args[1] = bpx_get_integer(bpx_get_call_arg(2,2)); /* sync-id */

    mp_debug("SYNC(%d,%d): BGN", args[0], args[1]);

    MPI_Allreduce(args, amin, 2, MPI_INT, MPI_MIN, MPI_COMM_WORLD);
    MPI_Allreduce(args, amax, 2, MPI_INT, MPI_MAX, MPI_COMM_WORLD);

    if (amin[0] != amax[0]) {
        emit_internal_error("failure on sync (%d,%d)", args[0], args[1]);
        RET_INTERNAL_ERR;
    }

    if (amin[1] < 0) {
        return BP_FALSE;
    }

    if (amin[1] != amax[1]) {
        emit_internal_error("failure on sync (%d,%d)", args[0], args[1]);
        RET_INTERNAL_ERR;
    }

    mp_debug("SYNC(%d,%d): END", args[0], args[1]);

    return BP_TRUE;
}

int pc_mp_send_goal_1(void)
{
    MPI_Status status;

    MPI_Recv(NULL, 0, MPI_INT, MPI_ANY_SOURCE, TAG_GOAL_REQ, MPI_COMM_WORLD, &status);
    return send_term(bpx_get_call_arg(1,1), 0, status.MPI_SOURCE);
}

int pc_mp_recv_goal_1(void)
{
    MPI_Send(NULL, 0, MPI_INT, 0, TAG_GOAL_REQ, MPI_COMM_WORLD);
    return recv_term(bpx_get_call_arg(1,1), 0, 0);
}

int pc_mpm_bcast_command_1(void)
{
    return send_term(bpx_get_call_arg(1,1), 1, 0);
}

int pc_mps_bcast_command_1(void)
{
    return recv_term(bpx_get_call_arg(1,1), 1, 0);
}

int pc_mps_revert_stdout_0(void)
{
    if (fd_dup_stdout >= 0) {
        dup2(fd_dup_stdout, STDOUT_FILENO);
        close(fd_dup_stdout);
        fd_dup_stdout = -1;
    }

    return BP_TRUE;
}

/*------------------------------------------------------------------------*/

#endif /* MPI */
