/*************************************************************************
*									 *
*	 YAP Prolog    @(#)amidefs.h	1.3 3/15/90
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		tracer.h						 *
* Last rev:								 *
* mods:									 *
* comments:	definitions for low level tracer			 *
*									 *
*************************************************************************/

#ifdef LOW_LEVEL_TRACER

typedef enum {
  enter_pred,
  try_or,
  retry_or,
  retry_pred,
  retry_table_producer,
  retry_table_consumer
} yap_low_level_port;

void	STD_PROTO(low_level_trace,(yap_low_level_port, PredEntry *, CELL *));
void	STD_PROTO(_YAP_InitLowLevelTrace,(void));
void	STD_PROTO(toggle_low_level_trace,(void));

extern int _YAP_do_low_level_trace;

#endif


