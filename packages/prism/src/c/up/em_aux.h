#ifndef EM_AUX_H
#define EM_AUX_H

extern int *  num_sw_vals;  /* #-vals of switches that occur in e-graphs */
extern double itemp;                /* inversed temperature (for DAEM)   */
extern double inside_failure;       /* inside prob. of failure           */
extern int    failure_observed;     /* flag: true if failure is observed */

void alloc_occ_switches(void);
void sort_occ_switches(void);
void release_occ_switches(void);
void alloc_num_sw_vals(void);
void release_num_sw_vals(void);
void transfer_hyperparams_prolog(void);

#endif /* EM_AUX_H */
