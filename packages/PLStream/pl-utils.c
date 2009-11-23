
#include "pl-incl.h"
#include "pl-ctype.h"

#ifndef HAVE_STRICMP
int
stricmp(const char *s1, const char *s2)
{ while(*s1 && makeLower(*s1) == makeLower(*s2))
    s1++, s2++;
  
  return makeLower(*s1) - makeLower(*s2);
}
#endif

bool
stripostfix(char *s, char *e)
{ int ls = strlen(s);
  int le = strlen(e);

  if ( ls >= le )
    return stricmp(&s[ls-le], e) == 0;

  return FALSE;
} 

