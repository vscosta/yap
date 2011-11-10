/* -*- c-basic-offset: 4 ; tab-width: 4 -*- */

#ifndef MP_EM_AUX_H
#define MP_EM_AUX_H

/*-------------------------------------------------------------------------*/

extern int sw_msg_size;

/*-------------------------------------------------------------------------*/

void    alloc_sw_msg_buffers(void);
void    release_sw_msg_buffers(void);
void    mpm_bcast_fixed(void);
void    mps_bcast_fixed(void);
void    mpm_bcast_inside(void);
void    mps_bcast_inside(void);
void    mpm_bcast_inside_h(void);
void    mps_bcast_inside_h(void);
void    mpm_bcast_smooth(void);
void    mps_bcast_smooth(void);
void    clear_sw_msg_send(void);
void    mpm_share_expectation(void);
void    mps_share_expectation(void);
double  mp_sum_value(double);

/*-------------------------------------------------------------------------*/

#endif /* MP_EM_AUX_H */
