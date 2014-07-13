#include "Yap.h"
#include "clause.h"
#include "eval.h"
#if HRAVE_ERRNO_H
#include <errno.h>
#else
extern int errno;
#endif
#include <string.h>
#include <ctype.h>

#define UPPER_ENTRY(S) \
    tmp = (char*)malloc((strlen(S)+1)*sizeof(char)); \
    while (S[i]) { \
      if (S[i] != '-' && S[i] != '_' && S[i] != ' ') { \
	    if ((S[i] >= '0' && S[i] <= '9') || (S[i] == '.')) \
		  tmp[j] = S[i]; \
		else \
	      tmp[j] = toupper(S[i]); \
		j++; \
	  } \
      i++; \
    } \
    tmp[j] = 0; \
    strcpy(S, tmp); \
    free(tmp);

#if YAP_JIT
void Yap_InitJitAnalysisPreds( void );
void Yap_InitJitCodegenPreds( void );
void Yap_InitJitConfigPreds( void );
void Yap_InitJitTransformPreds( void );
#if YAP_STAT_PREDS
void Yap_InitJitStatisticPreds( void );
#endif
#endif /* YAP_JIT */
#if YAP_DBG_PREDS
void Yap_InitJitDebugPreds( void );
#endif

Environment ExpEnv;
#if YAP_JIT
extern NativeContext *NativeArea;
extern IntermediatecodeContext *IntermediatecodeArea;
#endif
