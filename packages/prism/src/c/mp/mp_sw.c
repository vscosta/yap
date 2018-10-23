/* -*- c-basic-offset: 4 ; tab-width: 4 -*- */

#ifdef MPI

/*------------------------------------------------------------------------*/

#include "bprolog.h"
#include "core/idtable.h"
#include "core/idtable_preds.h"
#include "up/up.h"
#include "up/em_aux.h"
#include "up/graph.h"
#include "up/flags.h"
#include "mp/mp.h"
#include "mp/mp_core.h"
#include "mp/mp_em_aux.h"
#include <mpi.h>
#include <stdlib.h>
#include <string.h>

/*------------------------------------------------------------------------*/

int *occ_position = NULL;
static int *  sizes = NULL;
static int ** swids = NULL;

#define L(i) (sizes[i * 2 + 0]) /* length of the message from RANK #i */
#define N(i) (sizes[i * 2 + 1]) /* number of switches in RANK #i*/

/*------------------------------------------------------------------------*/

/* cpred.c (B-Prolog) */
int bp_string_2_term(const char *, TERM, TERM);

/* mic.c (B-Prolog) */
NORET quit(const char *);

/*------------------------------------------------------------------------*/

static void parse_switch_req(const char *msg, int src)
{
    const char *p;
    TERM  op1, op2;
    int   i;

    swids[src] = MALLOC(sizeof(int) * N(src));

    p = msg;

    for (i = 0; i < N(src); i++) {
        op1 = bpx_build_var();
        op2 = bpx_build_var();
        bp_string_2_term(p, op1, op2);
        swids[src][i] = prism_sw_id_register(op1);
        while (*(p++) != '\0') ;
    }
}

/*------------------------------------------------------------------------*/

int pc_mp_send_switches_0(void)
{
    char  *msg, *str;
    TERM  msw;
    int   msglen, msgsiz;
    int   vals[2];
    int   i, n;

    msglen = 0;
    msgsiz = 65536;
    msg = MALLOC(msgsiz);

    for (i = 0; i < occ_switch_tab_size; i++) {
        msw = bpx_get_arg(1, prism_sw_ins_term(occ_switches[i]->id));
        str = (char *)bpx_term_2_string(msw);

        n = strlen(str) + 1;

        if (msgsiz <= msglen + n) {
            msgsiz = (msglen + n + 65536) & ~65535;
            msg = REALLOC(msg, msgsiz);
        }

        strcpy(msg + msglen, str);
        msglen += n;
    }

    msg[msglen++] = '\0'; /* this is safe */

    vals[0] = msglen;
    vals[1] = occ_switch_tab_size;

    MPI_Gather(vals, 2, MPI_INT, NULL, 0, MPI_INT, 0, MPI_COMM_WORLD);
    MPI_Send(msg, msglen, MPI_CHAR, 0, TAG_SWITCH_REQ, MPI_COMM_WORLD);

    free(msg);

    return BP_TRUE;
}

int pc_mp_recv_switches_0(void)
{
    int  i, lmax, vals[2];
    char *msg;

    sizes = MALLOC(sizeof(int) * 2 * mp_size);
    swids = MALLOC(sizeof(int *) * mp_size);

    MPI_Gather(vals, 2, MPI_INT, sizes, 2, MPI_INT, 0, MPI_COMM_WORLD);

    lmax = 0;

    for (i = 1; i < mp_size; i++) {
        if (lmax < L(i)) {
            lmax = L(i);
        }
    }

    msg = MALLOC(lmax);

    for (i = 1; i < mp_size; i++) {
        MPI_Recv(msg, L(i), MPI_CHAR, i, TAG_SWITCH_REQ, MPI_COMM_WORLD, NULL);
        parse_switch_req(msg, i);
    }

    free(msg);

    return BP_TRUE;
}

int pc_mp_send_swlayout_0(void)
{
    int i, j, *msg, *pos;

    msg = MALLOC(sizeof(int) * sw_tab_size);
    pos = MALLOC(sizeof(int) * sw_ins_tab_size);

    j = 0;

    for (i = 0; i < occ_switch_tab_size; i++) {
        pos[occ_switches[i]->id] = j;
        j += num_sw_vals[i];
    }

    sw_msg_size = j;

    for (i = 1; i < mp_size; i++) {
        for (j = 0; j < N(i); j++) {
            msg[j] = pos[switches[swids[i][j]]->id];
        }

        MPI_Send(msg, N(i), MPI_INT, i, TAG_SWITCH_RES, MPI_COMM_WORLD);
        free(swids[i]);
    }

    free(pos);
    free(msg);

    free(sizes);
    free(swids);

    return BP_TRUE;
}

int pc_mp_recv_swlayout_0(void)
{
    occ_position = MALLOC(sizeof(int) * occ_switch_tab_size);

    MPI_Recv(occ_position, occ_switch_tab_size, MPI_INT, 0, TAG_SWITCH_RES, MPI_COMM_WORLD, NULL);

    /* debug */
    {
        int i;
        TERM msw;
        for (i = 0; i < occ_switch_tab_size; i++) {
            msw = bpx_get_arg(1, prism_sw_ins_term(occ_switches[i]->id));
            mp_debug("%s -> %d", bpx_term_2_string(msw), occ_position[i]);
        }
    }

    return BP_TRUE;
}

int pc_mpm_alloc_occ_switches_0(void)
{
    occ_switches = MALLOC(sizeof(SW_INS_PTR) * sw_tab_size);

    occ_switch_tab_size = sw_tab_size;
    memmove(occ_switches, switches, sizeof(SW_INS_PTR) * sw_tab_size);
    if (fix_init_order) {
        sort_occ_switches();
    }
    alloc_num_sw_vals();

    return BP_TRUE;
}

void release_occ_position(void)
{
    free(occ_position);
    occ_position = NULL;
}

/*------------------------------------------------------------------------*/

#endif /* MPI */
