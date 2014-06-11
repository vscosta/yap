#include <stdarg.h>
#include "bprolog.h"
#include "up/up.h"
#include "core/gamma.h"

/*------------------------------------------------------------------------*/

/* mic.c (B-Prolog) */
int compare(TERM, TERM);
int prism_printf(const char *fmt, ...);
int pc_mp_mode_0(void);
int compare_sw_ins(const void *a, const void *b);
int get_term_depth(TERM t);
int pc_get_term_depth_2(void);
int pc_lngamma_2(void);
int pc_mtrace_0(void);
int pc_muntrace_0(void);
void xsleep(unsigned int milliseconds);
int pc_sleep_1(void);

/*------------------------------------------------------------------------*/

int prism_printf(const char *fmt, ...)
{
    va_list ap;
    int rv;

    va_start(ap, fmt);
    rv = vfprintf(curr_out, fmt, ap);
    va_end(ap);

    fflush(curr_out);

    return rv;
}

/*------------------------------------------------------------------------*/

int pc_mp_mode_0(void)
{
#ifdef MPI
    return BP_TRUE;
#else
    return BP_FALSE;
#endif
}

/*------------------------------------------------------------------------*/

int compare_sw_ins(const void *a, const void *b)
{
    SW_INS_PTR sw_ins_a, sw_ins_b;
    TERM msw_a, msw_b;

    sw_ins_a = *(const SW_INS_PTR *)(a);
    sw_ins_b = *(const SW_INS_PTR *)(b);

    msw_a = prism_sw_ins_term(sw_ins_a->id);
    msw_b = prism_sw_ins_term(sw_ins_b->id);

    return compare(bpx_get_arg(1,msw_a), bpx_get_arg(1,msw_b));
}

/*------------------------------------------------------------------------*/

int get_term_depth(TERM t)
{
    SYM_REC_PTR sym;
    int i, n, d, di;

    XDEREF(t);

    SWITCH_OP(t, l_term_depth, { return 0; }, { return (0); }, {
        if (IsNumberedVar(t)) return 0;

        d = 0;
        i = 0;

        while (bpx_is_list(t)) {
            di = get_term_depth(bpx_get_car(t)) + (++i);
            d = d > di ? d : di;
            t = bpx_get_cdr(t);
        }

        di = get_term_depth(t) + i;
        d = d > di ? d : di;

        return d;
    }, {
        sym = GET_STR_SYM_REC(t);

        if (sym == float_psc) return 0;

        n = GET_ARITY_STR(sym);
        d = 0;

        for (i = 1; i <= n; i++) {
            di = get_term_depth(bpx_get_arg(i, t));
            d = d > di ? d : di;
        }

        return d + 1;
    }, { return 0; });

    return 0;  /* arbitrary */
}

int pc_get_term_depth_2(void)
{
    return bpx_unify(bpx_build_integer(get_term_depth(bpx_get_call_arg(1,2))),
                     bpx_get_call_arg(2,2));
}

/*------------------------------------------------------------------------*/

int pc_lngamma_2(void)
{
	double x = bpx_get_float(bpx_get_call_arg(1,2));
	TERM t = bpx_build_float(lngamma(x));

	return bpx_unify(bpx_get_call_arg(2,2),t);
}

/*------------------------------------------------------------------------*/

int pc_mtrace_0(void)
{
#ifdef MALLOC_TRACE
  mtrace();
#endif
  return BP_TRUE;
}

int pc_muntrace_0(void)
{
#ifdef MALLOC_TRACE
  muntrace();
#endif
  return BP_TRUE;
}

/*------------------------------------------------------------------------*/

/* effective only for Linux and Mac OS X */
void xsleep(unsigned int milliseconds)
{
#ifndef _MSC_VER
  usleep(milliseconds * 1000);
#endif
}

int pc_sleep_1(void)
{
  xsleep(bpx_get_integer(bpx_get_call_arg(1,1)));

  return BP_TRUE;
}
