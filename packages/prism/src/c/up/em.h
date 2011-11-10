/* -*- c-basic-offset: 4 ; tab-width: 4 -*- */

#ifndef __EM_H__
#define __EM_H__

/*------------------------------------------------------------------------*/

#define DEFAULT_MAX_ITERATE (10000)

/*------------------------------------------------------------------------*/

struct EM_Engine {
    int     smooth;             /* [in ] flag: use MAP? */
    double  lambda;             /* [out] log post */
    double  likelihood;         /* [out] log likelihood */
    int     iterate;            /* [out] number of iterations */
    double  bic;                /* [out] BIC score */
    double  cs;                 /* [out] CS score */

    /* Functions called during computation. */
    int     (* compute_inside         )(void);
    int     (* examine_inside         )(void);
    int     (* compute_expectation    )(void);
    double  (* compute_likelihood     )(void);
    double  (* compute_log_prior      )(void);
    int     (* update_params          )(void);
};

struct VBEM_Engine {
    double  free_energy;        /* [out] free energy */
    int     iterate;            /* [out] number of iterations */

    /* Functions called during computation. */
    int     (* compute_pi             )(void);
    int     (* compute_inside         )(void);
    int     (* examine_inside         )(void);
    int     (* compute_expectation    )(void);
    double  (* compute_free_energy_l0 )(void);
    double  (* compute_free_energy_l1 )(void);
    double  (* compute_likelihood     )(void);
    int     (* update_hyperparams     )(void);
};

typedef struct EM_Engine   * EM_ENG_PTR;
typedef struct VBEM_Engine * VBEM_ENG_PTR;

/*------------------------------------------------------------------------*/

#define SHOW_PROGRESS(n)                                                \
    do {                                                                \
        if(!verb_em && em_message > 0 && (n) % em_progress == 0) {      \
            if((n) % (em_progress * 10) == 0)                            \
                prism_printf("%d", n);                                  \
            else                                                        \
                prism_printf(".");                                      \
        }                                                               \
    } while (0)

#define SHOW_PROGRESS_HEAD(str, r)                                      \
    do {                                                                \
        if(num_restart > 1) {                                           \
            if(verb_em)                                                 \
                prism_printf("<<<< RESTART #%d >>>>\n", r);             \
            else if(em_message > 0)                                     \
                prism_printf("[%d] ", r);                               \
        }                                                               \
        if(!verb_em && em_message > 0)                                  \
            prism_printf("%s: ", str);                                  \
    } while (0)

#define SHOW_PROGRESS_TAIL(converged, n, x)                             \
    do {                                                                \
        const char *str =                                               \
            converged ? "Converged" : "Stopped";                        \
                                                                        \
        if(verb_em)                                                     \
            prism_printf("* %s (%.9f)\n", str, x);                      \
        else if(em_message > 0)                                         \
            prism_printf("(%d) (%s: %.9f)\n", n, str, x);               \
    } while (0)

#define SHOW_PROGRESS_TEMP(x)                                           \
    do {                                                                \
        if(verb_em)                                                     \
            prism_printf("* Temperature = %.3f\n", x);                  \
        else if(em_message > 0 && show_itemp)                           \
            prism_printf("<%.3f>", x);                                  \
        else                                                            \
            prism_printf("*");                                          \
    } while (0)

#define SHOW_PROGRESS_INTR()                                            \
    do {                                                                \
        if(verb_em)                                                     \
            prism_printf("* Interrupted\n");                            \
        else if(em_message > 0)                                         \
            prism_printf("(Interrupted)\n");                            \
    } while (0)

#define REACHED_MAX_ITERATE(n)                                          \
    ((max_iterate == -1 && (n) >= DEFAULT_MAX_ITERATE) ||               \
     (max_iterate >= +1 && (n) >= max_iterate))

/*------------------------------------------------------------------------*/

#endif /* __EM_H__ */
