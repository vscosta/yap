#ifndef ERROR_H
#define ERROR_H

/*--------------------------------------------------------------------*/

#define RET_ERR(err)                              \
    do {                                          \
        exception = (err);                        \
        return BP_ERROR;                          \
    } while (0)

#define RET_RUNTIME_ERR                           \
    do {                                          \
        exception = err_runtime;                  \
        return BP_ERROR;                          \
    } while (0)

#define RET_INTERNAL_ERR                          \
    do {                                          \
        exception = err_internal;                 \
        return BP_ERROR;                          \
    } while (0)

#define RET_ON_ERR(expr)                          \
    do {                                          \
        if ((expr) == BP_ERROR) return BP_ERROR;  \
    } while (0)

#define RET_ERR_ON_ERR(expr,err)                  \
    do {                                          \
        if ((expr) == BP_ERROR) {                 \
            exception = (err);                    \
            return BP_ERROR;                      \
        }                                         \
    } while (0)

/*--------------------------------------------------------------------*/

extern TERM err_runtime;
extern TERM err_internal;

extern TERM err_cycle_detected;
extern TERM err_invalid_likelihood;
extern TERM err_invalid_free_energy;
extern TERM err_invalid_numeric_value;
extern TERM err_invalid_goal_id;
extern TERM err_invalid_switch_instance_id;
extern TERM err_underflow;
extern TERM err_overflow;
extern TERM err_ctrl_c_pressed;

extern TERM ierr_invalid_likelihood;
extern TERM ierr_invalid_free_energy;
extern TERM ierr_function_not_implemented;
extern TERM ierr_unmatched_branches;

/*--------------------------------------------------------------------*/

TERM build_runtime_error(const char *);
TERM build_internal_error(const char *);
void emit_error(const char *, ...);
void emit_internal_error(const char *, ...);

/*--------------------------------------------------------------------*/

#endif /* ERROR_H */
