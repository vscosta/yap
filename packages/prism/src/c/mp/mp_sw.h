/* -*- c-basic-offset: 4 ; tab-width: 4 -*- */

#ifndef MP_SW_H
#define MP_SW_H

/*-------------------------------------------------------------------------*/

extern int *occ_position;

/*-------------------------------------------------------------------------*/

int pc_mp_send_switches_0(void);
int pc_mp_recv_switches_0(void);
int pc_mp_send_swlayout_0(void);
int pc_mp_recv_swlayout_0(void);
int pc_mpm_alloc_occ_switches_0(void);

void release_occ_position(void);

/*-------------------------------------------------------------------------*/

#endif /* MP_SW_H */
