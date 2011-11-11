/* -*- c-basic-offset: 4 ; tab-width: 4 -*- */

#ifndef MP_PREDS_H
#define MP_PREDS_H

/*-------------------------------------------------------------------------*/

int pc_mp_size_1(void);
int pc_mp_rank_1(void);
int pc_mp_master_0(void);
int pc_mp_abort_0(void);
int pc_mp_wtime_1(void);
int pc_mp_sync_2(void);
int pc_mp_send_goal_1(void);
int pc_mp_recv_goal_1(void);
int pc_mpm_bcast_command_1(void);
int pc_mps_bcast_command_1(void);
int pc_mps_revert_stdout_0(void);

/*-------------------------------------------------------------------------*/

#endif /* MP_PREDS_H */
