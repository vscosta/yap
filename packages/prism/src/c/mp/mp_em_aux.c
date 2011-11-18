/* -*- c-basic-offset: 4 ; tab-width: 4 -*- */

#ifdef MPI

/*------------------------------------------------------------------------*/

#include "bprolog.h"
#include "up/up.h"
#include "up/em.h"
#include "up/graph.h"
#include "mp/mp.h"
#include "mp/mp_core.h"
#include "mp/mp_sw.h"
#include <stdlib.h>

/*------------------------------------------------------------------------*/

int sw_msg_size = 0;
static void * sw_msg_send = NULL;
static void * sw_msg_recv = NULL;

/*------------------------------------------------------------------------*/

/* mic.c (B-Prolog) */
NORET quit(const char *);

/*------------------------------------------------------------------------*/

void alloc_sw_msg_buffers(void)
{
    sw_msg_send = MALLOC(sizeof(double) * sw_msg_size);
    sw_msg_recv = MALLOC(sizeof(double) * sw_msg_size);
}

void release_sw_msg_buffers(void)
{
    free(sw_msg_send);
    sw_msg_send = NULL;
    free(sw_msg_recv);
    sw_msg_recv = NULL;
}

/*------------------------------------------------------------------------*/

void mpm_bcast_fixed(void)
{
    SW_INS_PTR sw_ins_ptr;
    char *meg_ptr;
    int i;

    meg_ptr = sw_msg_send;

    for (i = 0; i < occ_switch_tab_size; i++) {
        for (sw_ins_ptr = occ_switches[i]; sw_ins_ptr != NULL; sw_ins_ptr = sw_ins_ptr->next) {
            *(meg_ptr++) = (!!sw_ins_ptr->fixed) | ((!!sw_ins_ptr->fixed_h) << 1);
        }
    }

    MPI_Bcast(sw_msg_send, sw_msg_size, MPI_CHAR, 0, MPI_COMM_WORLD);
    mp_debug("mpm_bcast_fixed");
}

void mps_bcast_fixed(void)
{
    SW_INS_PTR sw_ins_ptr;
    char *meg_ptr;
    int i;

    MPI_Bcast(sw_msg_recv, sw_msg_size, MPI_CHAR, 0, MPI_COMM_WORLD);
    mp_debug("mps_bcast_fixed");

    for (i = 0; i < occ_switch_tab_size; i++) {
        meg_ptr = sw_msg_recv;
        meg_ptr += occ_position[i];
        for (sw_ins_ptr = occ_switches[i]; sw_ins_ptr != NULL; sw_ins_ptr = sw_ins_ptr->next) {
            sw_ins_ptr->fixed   = !!(*meg_ptr & 1);
            sw_ins_ptr->fixed_h = !!(*meg_ptr & 2);
            meg_ptr++;
        }
    }
}

void mpm_bcast_inside(void)
{
    SW_INS_PTR sw_ins_ptr;
    double *meg_ptr;
    int i;

    meg_ptr = sw_msg_send;

    for (i = 0; i < occ_switch_tab_size; i++) {
        for (sw_ins_ptr = occ_switches[i]; sw_ins_ptr != NULL; sw_ins_ptr = sw_ins_ptr->next) {
            *(meg_ptr++) = sw_ins_ptr->inside;
        }
    }

    MPI_Bcast(sw_msg_send, sw_msg_size, MPI_DOUBLE, 0, MPI_COMM_WORLD);
    mp_debug("mpm_bcast_inside");
}

void mps_bcast_inside(void)
{
    SW_INS_PTR sw_ins_ptr;
    double *meg_ptr;
    int i;

    MPI_Bcast(sw_msg_recv, sw_msg_size, MPI_DOUBLE, 0, MPI_COMM_WORLD);
    mp_debug("mps_bcast_inside");

    for (i = 0; i < occ_switch_tab_size; i++) {
        meg_ptr = sw_msg_recv;
        meg_ptr += occ_position[i];
        for (sw_ins_ptr = occ_switches[i]; sw_ins_ptr != NULL; sw_ins_ptr = sw_ins_ptr->next) {
            sw_ins_ptr->inside = *(meg_ptr++);
        }
    }
}

void mpm_bcast_inside_h(void)
{
    SW_INS_PTR sw_ins_ptr;
    double *meg_ptr;
    int i;

    meg_ptr = sw_msg_send;

    for (i = 0; i < occ_switch_tab_size; i++) {
        for (sw_ins_ptr = occ_switches[i]; sw_ins_ptr != NULL; sw_ins_ptr = sw_ins_ptr->next) {
            *(meg_ptr++) = sw_ins_ptr->inside_h;
        }
    }

    MPI_Bcast(sw_msg_send, sw_msg_size, MPI_DOUBLE, 0, MPI_COMM_WORLD);
    mp_debug("mpm_bcast_inside_h");
}

void mps_bcast_inside_h(void)
{
    SW_INS_PTR sw_ins_ptr;
    double *meg_ptr;
    int i;

    MPI_Bcast(sw_msg_recv, sw_msg_size, MPI_DOUBLE, 0, MPI_COMM_WORLD);
    mp_debug("mps_bcast_inside_h");

    for (i = 0; i < occ_switch_tab_size; i++) {
        meg_ptr = sw_msg_recv;
        meg_ptr += occ_position[i];
        for (sw_ins_ptr = occ_switches[i]; sw_ins_ptr != NULL; sw_ins_ptr = sw_ins_ptr->next) {
            sw_ins_ptr->inside_h = *(meg_ptr++);
        }
    }
}

void mpm_bcast_smooth(void)
{
    SW_INS_PTR sw_ins_ptr;
    double *meg_ptr;
    int i;

    meg_ptr = sw_msg_send;

    for (i = 0; i < occ_switch_tab_size; i++) {
        for (sw_ins_ptr = occ_switches[i]; sw_ins_ptr != NULL; sw_ins_ptr = sw_ins_ptr->next) {
            *(meg_ptr++) = sw_ins_ptr->smooth;
        }
    }

    MPI_Bcast(sw_msg_send, sw_msg_size, MPI_DOUBLE, 0, MPI_COMM_WORLD);
    mp_debug("mpm_bcast_smooth");
}

void mps_bcast_smooth(void)
{
    SW_INS_PTR sw_ins_ptr;
    double *meg_ptr;
    int i;

    MPI_Bcast(sw_msg_recv, sw_msg_size, MPI_DOUBLE, 0, MPI_COMM_WORLD);
    mp_debug("mps_bcast_smooth");

    for (i = 0; i < occ_switch_tab_size; i++) {
        meg_ptr = sw_msg_recv;
        meg_ptr += occ_position[i];
        for (sw_ins_ptr = occ_switches[i]; sw_ins_ptr != NULL; sw_ins_ptr = sw_ins_ptr->next) {
            sw_ins_ptr->smooth = *(meg_ptr++);
        }
    }
}

/*------------------------------------------------------------------------*/

void clear_sw_msg_send(void)
{
    double *meg_ptr;
    double *end_ptr;

    meg_ptr = sw_msg_send;
    end_ptr = meg_ptr + sw_msg_size;
    while (meg_ptr != end_ptr) {
        *(meg_ptr++) = 0.0;
    }
}

void mpm_share_expectation(void)
{
    SW_INS_PTR sw_ins_ptr;
    double *meg_ptr;
    int i;

    MPI_Allreduce(sw_msg_send, sw_msg_recv, sw_msg_size, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);

    meg_ptr = sw_msg_recv;

    for (i = 0; i < occ_switch_tab_size; i++) {
        for (sw_ins_ptr = occ_switches[i]; sw_ins_ptr != NULL; sw_ins_ptr = sw_ins_ptr->next) {
            sw_ins_ptr->total_expect = *(meg_ptr++);
        }
    }
}

void mps_share_expectation(void)
{
    SW_INS_PTR sw_ins_ptr;
    double *meg_ptr;
    int i;

    for (i = 0; i < occ_switch_tab_size; i++) {
        meg_ptr = sw_msg_send;
        meg_ptr += occ_position[i];
        for (sw_ins_ptr = occ_switches[i]; sw_ins_ptr != NULL; sw_ins_ptr = sw_ins_ptr->next) {
            *(meg_ptr++) = sw_ins_ptr->total_expect;
        }
    }

    MPI_Allreduce(sw_msg_send, sw_msg_recv, sw_msg_size, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);

    for (i = 0; i < occ_switch_tab_size; i++) {
        meg_ptr = sw_msg_recv;
        meg_ptr += occ_position[i];
        for (sw_ins_ptr = occ_switches[i]; sw_ins_ptr != NULL; sw_ins_ptr = sw_ins_ptr->next) {
            sw_ins_ptr->total_expect = *(meg_ptr++);
        }
    }
}

double mp_sum_value(double value)
{
    double g_value;
    MPI_Allreduce(&value, &g_value, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);
    return g_value;
}

/*------------------------------------------------------------------------*/

#endif /* MPI */
