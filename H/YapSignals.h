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
  YAP_CREEP_SIGNAL = (NSIG+1),	/* received a creep */
  YAP_WAKEUP_SIGNAL = (NSIG+2),	/* goals to wake up */
  YAP_ITI_SIGNAL = (NSIG+3),	/* received inter thread signal */
  YAP_TROVF_SIGNAL = (NSIG+4),	/* received trail overflow */
  YAP_CDOVF_SIGNAL = (NSIG+5),	/* received code overflow */
  YAP_STOVF_SIGNAL = (NSIG+6),	/* received stack overflow */
  YAP_TRACE_SIGNAL = (NSIG+7),	/* received start trace */
  YAP_DEBUG_SIGNAL = (NSIG+8),	/* received start debug */
  YAP_BREAK_SIGNAL = (NSIG+9),	/* received break signal */
  YAP_STACK_DUMP_SIGNAL = (NSIG+10),	/* received stack dump signal */
  YAP_STATISTICS_SIGNAL = (NSIG+11),	/* received statistics */
  YAP_AGC_SIGNAL = (NSIG+12),	/* call atom garbage collector asap */
  YAP_WINTIMER_SIGNAL = (NSIG+13),	/* windows alarm */
  YAP_FAIL_SIGNAL = (NSIG+14),	/* P = FAILCODE */
  YAP_ABORT_SIGNAL = (NSIG+15),	/* P = FAILCODE */
  YAP_EXIT_SIGNAL = (NSIG+16)	/* P = FAILCODE */
} yap_signals;

#define	Yap_get_signal(S) Yap_get_signal__(S PASS_REGS)
#define	Yap_has_a_signal() Yap_has_a_signal__(PASS_REGS1)
#define	Yap_has_signals(S1, S2) Yap_has_signals__(S1, S2 PASS_REGS)
#define	Yap_only_has_signals(S1, S2) Yap_only_has_signals__(S1, S2 PASS_REGS)
#define	Yap_has_signal(S) Yap_has_signal__(S PASS_REGS)
#define	Yap_only_has_signal(S) Yap_only_has_signal__(S PASS_REGS)

inline static uint64_t
SIGNAL_TO_BIT( yap_signals sig)
{
  return ((uint64_t)1 << (sig-1));
}


INLINE_ONLY inline EXTERN int Yap_has_a_signal__ ( USES_REGS1 );

INLINE_ONLY inline EXTERN int
Yap_has_a_signal__ (USES_REGS1)
{
  return LOCAL_Signals != ((uint64_t)0);
}

INLINE_ONLY inline EXTERN int
Yap_has_signal__(yap_signals sig USES_REGS)
{
  return (LOCAL_Signals & SIGNAL_TO_BIT(sig)) != ((uint64_t)0);
}

INLINE_ONLY inline EXTERN int
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
