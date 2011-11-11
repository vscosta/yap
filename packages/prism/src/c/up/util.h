#ifndef UTIL_H
#define UTIL_H

/*====================================================================*/

int  pc_mp_mode_0(void);
int  pc_get_term_depth_2(void);

int  prism_printf(const char *, ...);
int  compare_sw_ins(const void *, const void *);
int  get_term_depth(TERM);

int  pc_lngamma_2(void);

int  pc_mtrace_0(void);
int  pc_muntrace_0(void);

void xsleep(unsigned int);
int  pc_sleep_1(void);

/*====================================================================*/

#endif /* UTIL_H */
