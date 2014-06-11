#include <stdarg.h>
#include "bprolog.h"
#include "core/bpx.h"

/*--------------------------------------------------------------------*/

#ifndef __YAP_PROLOG__
TERM bpx_build_atom(const char *);
#endif
TERM build_runtime_error(const char *s);
TERM build_internal_error(const char *s);
void register_prism_errors(void);
void emit_error(const char *fmt, ...);
void emit_internal_error(const char *fmt, ...);

/*--------------------------------------------------------------------*/

TERM err_runtime;
TERM err_internal;

TERM err_cycle_detected;
TERM err_invalid_likelihood;
TERM err_invalid_free_energy;
TERM err_invalid_numeric_value;
TERM err_invalid_goal_id;
TERM err_invalid_switch_instance_id;
TERM err_underflow;
TERM err_overflow;
TERM err_ctrl_c_pressed;

TERM ierr_invalid_likelihood;
TERM ierr_invalid_free_energy;
TERM ierr_function_not_implemented;
TERM ierr_unmatched_branches;

/*--------------------------------------------------------------------*/

TERM build_runtime_error(const char *s)
{
    TERM t;

    if (s == NULL) return bpx_build_atom("prism_runtime_error");

    t = bpx_build_structure("prism_runtime_error",1);
    bpx_unify(bpx_get_arg(1,t),bpx_build_atom(s));

    return t;
}

TERM build_internal_error(const char *s)
{
    TERM t;

    if (s == NULL) return bpx_build_atom("prism_internal_error");

    t = bpx_build_structure("prism_internal_error",1);
    bpx_unify(bpx_get_arg(1,t),bpx_build_atom(s));

    return t;
}

/*--------------------------------------------------------------------*/

void register_prism_errors(void)
{
    err_runtime                    = build_runtime_error(NULL);
    err_internal                   = build_internal_error(NULL);

    err_cycle_detected             = build_runtime_error("cycle_detected");
    err_invalid_likelihood         = build_runtime_error("invalid_likelihood");
    err_invalid_free_energy        = build_runtime_error("invalid_free_energy");
    err_invalid_numeric_value      = build_runtime_error("invalid_numeric_value");
    err_invalid_goal_id            = build_runtime_error("invalid_goal_id");
    err_invalid_switch_instance_id = build_runtime_error("invalid_switch_instance_id");
    err_underflow                  = build_runtime_error("underflow");
    err_overflow                   = build_runtime_error("overflow");
    err_ctrl_c_pressed             = build_runtime_error("ctrl_c_pressed");

    ierr_invalid_likelihood        = build_internal_error("invalid_likelihood");
    ierr_invalid_free_energy       = build_internal_error("invalid_free_energy");
    ierr_function_not_implemented  = build_internal_error("function_not_implemented");
    ierr_unmatched_branches        = build_internal_error("unmatched_branches");
}

/*--------------------------------------------------------------------*/

void emit_error(const char *fmt, ...)
{
    va_list ap;

    fprintf(curr_out, "*** PRISM ERROR: ");
    va_start(ap, fmt);
    vfprintf(curr_out, fmt, ap);
    va_end(ap);
    fprintf(curr_out, "\n");

    fflush(curr_out);
}

void emit_internal_error(const char *fmt, ...)
{
    va_list ap;

    fprintf(curr_out, "*** PRISM INTERNAL ERROR: ");
    va_start(ap, fmt);
    vfprintf(curr_out, fmt, ap);
    va_end(ap);
    fprintf(curr_out, "\n");

    fflush(curr_out);
}

/*--------------------------------------------------------------------*/
