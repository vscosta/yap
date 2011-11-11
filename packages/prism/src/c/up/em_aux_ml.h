#ifndef EM_AUX_ML_H
#define EM_AUX_ML_H

int    check_smooth(int *);
void   initialize_params(void);
int    compute_inside_scaling_none(void);
int    compute_inside_scaling_log_exp(void);
int    compute_daem_inside_scaling_none(void);
int    compute_daem_inside_scaling_log_exp(void);
int    examine_inside_scaling_none(void);
int    examine_inside_scaling_log_exp(void);
int    compute_expectation_scaling_none(void);
int    compute_expectation_scaling_log_exp(void);
double compute_likelihood_scaling_none(void);
double compute_likelihood_scaling_log_exp(void);
double compute_log_prior(void);
double compute_daem_log_prior(void);
int    update_params(void);
int    update_params_smooth(void);
void   save_params(void);
void   restore_params(void);
double compute_bic(double);
double compute_cs(double);

#endif /* EM_AUX_ML_H */

