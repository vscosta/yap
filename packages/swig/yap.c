
#include <stdio.h>
#include <stdlib.h>

#include "YAPInterface.h"

static void init_yap( void );
int yap_prove_string(char *s);

static int yap_on;

int
prove_string(char *s) {
  if (!yap_on)
    init_yap();
  if (YAP_RunGoal(YAP_MkAtomTerm(YAP_LookupAtom(s)))) {
    YAP_ShutdownGoal( TRUE );
    return TRUE;
  }
  return FALSE;
}

static void
init_yap( void )
{
  if (YAP_FastInit(NULL) == YAP_BOOT_ERROR) 
    exit(1);
  yap_on = TRUE;
}
