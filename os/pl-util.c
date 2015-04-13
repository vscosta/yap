
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

#if !defined(HAVE_MBSCOLL) || !defined(HAVE_MBCASESCOLL)
static void
wstolower(wchar_t *w, size_t len)
{ wchar_t *e = &w[len];

  for( ; w<e; w++ )
    *w = towlower(*w);
}

static int
int_mbscoll(const char *s1, const char *s2, int icase)
{ size_t l1 = strlen(s1);
  size_t l2 = strlen(s2);
  wchar_t *w1;
  wchar_t *w2;
  int ml1, ml2;
  mbstate_t mbs;
  int rc;

  if ( l1 < 1024 && (w1 = alloca(sizeof(wchar_t)*(l1+1))) )
  { ml1 = FALSE;
  } else
  { w1 = PL_malloc(sizeof(wchar_t)*(l1+1));
    ml1 = TRUE;
  }
  if ( l2 < 1024 && (w2 = alloca(sizeof(wchar_t)*(l2+1))) )
  { ml2 = FALSE;
  } else
  { w2 = PL_malloc(sizeof(wchar_t)*(l2+1));
    ml2 = TRUE;
  }

  memset(&mbs, 0, sizeof(mbs));
  if ( mbsrtowcs(w1, &s1, l1+1, &mbs) == (size_t)-1 )
  { rc = -2;
    goto out;
  }
  if ( mbsrtowcs(w2, &s2, l2+1, &mbs) == (size_t)-1 )
  { rc = 2;
    goto out;
  }
  if ( icase )
  { wstolower(w1, l1);
    wstolower(w2, l2);
  }

  rc = wcscoll(w1, w2);

out:
  if ( ml1 ) PL_free(w1);
  if ( ml2 ) PL_free(w2);

  return rc;
}
#endif
