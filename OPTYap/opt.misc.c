/************************************************************************
**                                                                     **
**                   The YapTab/YapOr/OPTYap systems                   **
**                                                                     **
** YapTab extends the Yap Prolog engine to support sequential tabling  **
** YapOr extends the Yap Prolog engine to support or-parallelism       **
** OPTYap extends the Yap Prolog engine to support or-parallel tabling **
**                                                                     **
**                                                                     **
**      Yap Prolog was developed at University of Porto, Portugal      **
**                                                                     **
************************************************************************/

/***********************
**      Includes      **
***********************/

#include "Yap.h"
#if defined(YAPOR) || defined(TABLING)
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include "Yatom.h"
#include "yapio.h"



/************************************************
**      Global variables are defined here      **
************************************************/

#if defined(YAPOR) && ! defined(THREADS)
struct worker WORKER;
#endif /* YAPOR && ! THREADS */



/*******************************
**      Global functions      **
*******************************/

void itos(int i, char *s) {
  int n,r,j;
  n = 10;
  while (n <= i) n *= 10;
  j = 0;
  while (n > 1) {
    n = n / 10;   
    r = i / n;
    i = i - r * n;
    s[j++] = r + '0';
  }
  s[j] = 0;
  return;
}
#endif /* YAPOR || TABLING */
