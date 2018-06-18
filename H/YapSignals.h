/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		YapSignals.h						 *
* comments:	YAP signal handling interface				 *
*									 *
* Last rev:     $Date							 *
* $Log $								 *
*									 *
*									 *
*************************************************************************/

#if HAVE_SIGNAL_H
#include <signal.h>
#endif
#if HAVE_ERRNO_H
#include <errno.h>
#endif

typedef enum
{
  YAP_NO_SIGNAL = 0,	/* received an alarm */
#ifdef SIGALRM
  YAP_ALARM_SIGNAL = SIGALRM,	/* received an alarm */
#endif
#ifdef SIGHUP
  YAP_HUP_SIGNAL = SIGHUP,	/* received SIGHUP */
#endif
#ifdef SIGUSR1
  YAP_USR1_SIGNAL = SIGUSR1,	/* received SIGUSR1 */
#endif
#ifdef SIGUSR2
  YAP_USR2_SIGNAL = SIGUSR2,	/* received SIGUSR2 */
#endif
#ifdef SIGINT
  YAP_INT_SIGNAL = SIGINT,	/* received SIGINT (unused for now) */
#endif
#ifdef SIGPIPE
  YAP_PIPE_SIGNAL = SIGPIPE,	/* call atom garbage collector asap */
#endif
#ifdef SIGVTALRM
  YAP_VTALARM_SIGNAL = SIGVTALRM, /* received SIGVTALARM */
#endif
#ifdef SIGFPE
  YAP_FPE_SIGNAL = SIGFPE, 	/* received SIGFPE */
#endif
#define PROLOG_SIG 31
  YAP_WAKEUP_SIGNAL = (PROLOG_SIG+1),	/* goals to wake up */
  YAP_ITI_SIGNAL = (PROLOG_SIG+2),	/* received inter thread signal */
  YAP_TROVF_SIGNAL = (PROLOG_SIG+3),	/* received trail overflow */
  YAP_CDOVF_SIGNAL = (PROLOG_SIG+4),	/* received code overflow */
  YAP_STOVF_SIGNAL = (PROLOG_SIG+5),	/* received stack overflow */
  YAP_TRACE_SIGNAL = (PROLOG_SIG+6),	/* received start trace */
  YAP_DEBUG_SIGNAL = (PROLOG_SIG+7),	/* received start debug */
  YAP_BREAK_SIGNAL = (PROLOG_SIG+8),	/* received break signal */
  YAP_STACK_DUMP_SIGNAL = (PROLOG_SIG+9),	/* received stack dump signal */
  YAP_STATISTICS_SIGNAL = (PROLOG_SIG+10),	/* received statistics */
  YAP_AGC_SIGNAL = (PROLOG_SIG+11),	/* call atom garbage collector asap */
  YAP_WINTIMER_SIGNAL = (PROLOG_SIG+12),	/* windows alarm */
  YAP_FAIL_SIGNAL = (PROLOG_SIG+13),	/* P = FAILCODE */
  YAP_ABORT_SIGNAL = (PROLOG_SIG+14),	/* P = FAILCODE */
  YAP_EXIT_SIGNAL = (PROLOG_SIG+15),	/* P = FAILCODE */
  YAP_CREEP_SIGNAL = (PROLOG_SIG+16)	/* received a creep, make sure it is the last signal */
} yap_signals;

#define	Yap_get_signal(S) Yap_get_signal__(S PASS_REGS)
#define	Yap_has_a_signal() Yap_has_a_signal__(PASS_REGS1)
#define	Yap_has_signals(S1, S2) Yap_has_signals__(S1, S2 PASS_REGS)
#define	Yap_only_has_signals(S1, S2) Yap_only_has_signals__(S1, S2 PASS_REGS)
#define	Yap_has_signal(S) Yap_has_signal__(S PASS_REGS)
#define	Yap_only_has_signal(S) Yap_only_has_signal__(S PASS_REGS)

INLINE_ONLY uint64_t SIGNAL_TO_BIT( yap_signals sig);

INLINE_ONLY uint64_t
SIGNAL_TO_BIT( yap_signals sig)
{
  return ((uint64_t)1 << (sig-1));
}


INLINE_ONLY int Yap_has_a_signal__ ( USES_REGS1 );

INLINE_ONLY int Yap_has_signal__ ( yap_signals sig USES_REGS );

INLINE_ONLY int Yap_only_has_signal__(yap_signals sig USES_REGS);

INLINE_ONLY int
Yap_has_a_signal__ (USES_REGS1)
{
  return LOCAL_Signals != ((uint64_t)0);
}

INLINE_ONLY int
Yap_has_signal__(yap_signals sig USES_REGS)
{
  return (LOCAL_Signals & SIGNAL_TO_BIT(sig)) != ((uint64_t)0);
}

INLINE_ONLY int
Yap_only_has_signal__(yap_signals sig USES_REGS)
{
  return (LOCAL_Signals & SIGNAL_TO_BIT(sig)) == SIGNAL_TO_BIT(sig);
}


void	Yap_signal(yap_signals);
void	Yap_external_signal(int, yap_signals);
int	Yap_get_signal__(yap_signals  USES_REGS);
int	Yap_has_a_signal__(  USES_REGS1 );
int	Yap_has_signals__(yap_signals,yap_signals  USES_REGS);
int	Yap_only_has_signals__(yap_signals,yap_signals  USES_REGS);

int Yap_HandleInterrupts( void );
