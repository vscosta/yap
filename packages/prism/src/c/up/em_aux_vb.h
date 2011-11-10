#ifndef EM_AUX_VB_H
#define EM_AUX_VB_H

int    check_smooth_vb(void);
void   initialize_hyperparams(void);
int    compute_pi_scaling_none(void);
int    compute_pi_scaling_log_exp(void);
int    compute_inside_vb_scaling_none(void);
int    compute_inside_vb_scaling_log_exp(void);
int    compute_daem_inside_vb_scaling_none(void);
int    compute_daem_inside_vb_scaling_log_exp(void);
double compute_free_energy_l0(void);
double compute_daem_free_energy_l0(void);
double compute_free_energy_l1_scaling_none(void);
double compute_free_energy_l1_scaling_log_exp(void);
double compute_daem_free_energy_l1_scaling_none(void);
double compute_daem_free_energy_l1_scaling_log_exp(void);
int    update_hyperparams(void);
int    update_daem_hyperparams(void);
void   save_hyperparams(void);
void   restore_hyperparams(void);
void   transfer_hyperparams(void);
void   get_param_means(void);

#endif /* EM_AUX_VB_H */
