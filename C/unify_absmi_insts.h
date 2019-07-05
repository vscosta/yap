/************************************************************************\
 *    Get Instructions                                                  *
\************************************************************************/

#include <amiops.h>

#ifdef INDENT_CODE
{
  {
    {
#endif /* INDENT_CODE */

      Op(get_x_var, xx);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xx.xr);
      XREG(PREG->y_u.xx.xl) = d0;
      PREG = NEXTOP(PREG, xx);
      ENDD(d0);
      GONext();
      ENDOp();

      Op(get_y_var, yx);
      BEGD(d0);
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yx.y;
      d0 = XREG(PREG->y_u.yx.x);
      PREG = NEXTOP(PREG, yx);
      INITIALIZE_PERMVAR(pt0,d0);
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(get_yy_var, yyxx);
      CACHE_Y(YREG);
      BEGD(d0);
      BEGP(pt0);
      pt0 = S_YREG + PREG->y_u.yyxx.y1;
      d0 = XREG(PREG->y_u.yyxx.x1);
      BEGD(d1);
      BEGP(pt1);
      pt1 = S_YREG + PREG->y_u.yyx.y2;
      d1 = XREG(PREG->y_u.yyxx.x2);
      PREG = NEXTOP(PREG, yyxx);
      INITIALIZE_PERMVAR(pt0,d0);
      INITIALIZE_PERMVAR(pt1,d1);
      ENDP(pt1);
      ENDD(d1);
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDCACHE_Y();
      ENDOp();

      /* The code for get_x_val is hard to follow because I use a
       * lot of jumps. The convention is that in the label
       * gval_X_YREG X refers to the state of the first argument, and
       * YREG to the state of the second argument */
      Op(get_x_val, xx);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xx.xl);
      deref_head(d0, gvalx_unk);

      /* d0 will keep the first argument */
    gvalx_nonvar:
      /* first argument is bound */
      BEGD(d1);
      d1 = XREG(PREG->y_u.xx.xr);
      deref_head(d1, gvalx_nonvar_unk);

    gvalx_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, xx);
      UnifyBound(d0, d1);

      BEGP(pt0);
      /* deref second argument */
      deref_body(d1, pt0, gvalx_nonvar_unk, gvalx_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, xx);
      YapBind(pt0, d0);
      GONext();

      ENDP(pt0);
      ENDD(d1);

      BEGP(pt0);
      /* first argument may be unbound */
      deref_body(d0, pt0, gvalx_unk, gvalx_nonvar);
      /* first argument is unbound and in pt0 and in d0 */
      BEGD(d1);
      d1 = XREG(PREG->y_u.xx.xr);
      deref_head(d1, gvalx_var_unk);

    gvalx_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, xx);
      YapBind(pt0, d1);
      GONext();

      BEGP(pt1);
      deref_body(d1, pt1, gvalx_var_unk, gvalx_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, xx);
      UnifyCells(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDD(d1);
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      /* The code for get_y_val mostly uses the code for get_x_val
       */

      Op(get_y_val, yx);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yx.y;
      d0 = *pt0;

      /* From now on, it's just a copy of the code for get_x_val */

      deref_head(d0, gvaly_unk);
    gvaly_nonvar:

      /* first argument is bound */
      d1 = XREG(PREG->y_u.yx.x);
      deref_head(d1, gvaly_nonvar_unk);
    gvaly_nonvar_nonvar:

      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, yx);
      UnifyBound(d0, d1);

      BEGP(pt1);
      /* deref second argument */
      deref_body(d1, pt1, gvaly_nonvar_unk, gvaly_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, yx);
      YapBind(pt1, d0);
      GONext();
      ENDP(pt1);


      /* first argument may be unbound */
      derefa_body(d0, pt0, gvaly_unk, gvaly_nonvar);
      /* first argument is unbound */
      d1 = XREG(PREG->y_u.yx.x);
      deref_head(d1, gvaly_var_unk);

    gvaly_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, yx);
      YapBind(pt0, d1);
      GONext();

      BEGP(pt1);
      deref_body(d1, pt1, gvaly_var_unk, gvaly_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, yx);
      UnifyCells(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(get_atom, xc);
      BEGD(d0);
      BEGD(d1);
      /* fetch arguments */
      d0 = XREG(PREG->y_u.xc.x);
      d1 = PREG->y_u.xc.c;

      BEGP(pt0);
      deref_head(d0, gatom_unk);
      /* argument is nonvar */
    gatom_nonvar:
      if (d0 == d1) {
	PREG = NEXTOP(PREG, xc);
	GONext();
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_unk, gatom_nonvar);
      /* argument is a variable */
      PREG = NEXTOP(PREG, xc);
      YapBind(pt0, d1);
      GONext();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(get_atom_exo, x);
      BEGD(d0);
      BEGD(d1);
      /* fetch arguments */
      d0 = XREG(PREG->y_u.x.x);
      d1 = *SREG;
      SREG++;

      BEGP(pt0);
      deref_head(d0, gatom_exo_unk);
      /* argument is nonvar */
    gatom_exo_nonvar:
      if (d0 == d1) {
	PREG = NEXTOP(PREG, x);
	GONext();
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_exo_unk, gatom_exo_nonvar);
      /* argument is a variable */
      pt0 = (CELL *)d0;
      PREG = NEXTOP(PREG, x);
      YapBind(pt0, d1);
      GONext();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(get_2atoms, cc);
      BEGD(d0);
      BEGD(d1);
      /* fetch arguments */
      d0 = ARG1;

      BEGP(pt0);
      deref_head(d0, gatom_2unk);
      /* argument is nonvar */
    gatom_2nonvar:
      if (d0 == PREG->y_u.cc.c1) {
	goto gatom_2b;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_2unk, gatom_2nonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.cc.c1);
      ENDP(pt0);
    gatom_2b:
      /* fetch arguments */
      d0 = ARG2;
      d1 = PREG->y_u.cc.c2;

      BEGP(pt0);
      deref_head(d0, gatom_2bunk);
      /* argument is nonvar */
    gatom_2bnonvar:
      if (d0 == d1) {
	PREG = NEXTOP(PREG, cc);
	GONext();
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_2bunk, gatom_2bnonvar);
      /* argument is a variable */
      PREG = NEXTOP(PREG, cc);
      YapBind(pt0, d1);
      GONext();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(get_3atoms, ccc);
      BEGD(d0);
      BEGD(d1);
      /* fetch arguments */
      d0 = ARG1;

      BEGP(pt0);
      deref_head(d0, gatom_3unk);
      /* argument is nonvar */
    gatom_3nonvar:
      if (d0 == PREG->y_u.ccc.c1) {
	goto gatom_3b;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_3unk, gatom_3nonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.ccc.c1);
      ENDP(pt0);
    gatom_3b:
      /* fetch arguments */
      d0 = ARG2;

      BEGP(pt0);
      deref_head(d0, gatom_3bunk);
      /* argument is nonvar */
    gatom_3bnonvar:
      if (d0 == PREG->y_u.ccc.c2) {
	goto gatom_3c;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_3bunk, gatom_3bnonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.ccc.c2);
      ENDP(pt0);
    gatom_3c:
      /* fetch arguments */
      d0 = ARG3;
      d1 = PREG->y_u.ccc.c3;

      BEGP(pt0);
      deref_head(d0, gatom_3cunk);
      /* argument is nonvar */
    gatom_3cnonvar:
      if (d0 == d1) {
	PREG = NEXTOP(PREG, ccc);
	GONext();
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_3cunk, gatom_3cnonvar);
      /* argument is a variable */
      PREG = NEXTOP(PREG, ccc);
      YapBind(pt0, d1);
      GONext();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(get_4atoms, cccc);
      BEGD(d0);
      BEGD(d1);
      /* fetch arguments */
      d0 = ARG1;

      BEGP(pt0);
      deref_head(d0, gatom_4unk);
      /* argument is nonvar */
    gatom_4nonvar:
      if (d0 == PREG->y_u.cccc.c1) {
	goto gatom_4b;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_4unk, gatom_4nonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.cccc.c1);
      ENDP(pt0);
    gatom_4b:
      /* fetch arguments */
      d0 = ARG2;

      BEGP(pt0);
      deref_head(d0, gatom_4bunk);
      /* argument is nonvar */
    gatom_4bnonvar:
      if (d0 == PREG->y_u.cccc.c2) {
	goto gatom_4c;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_4bunk, gatom_4bnonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.cccc.c2);
      ENDP(pt0);
    gatom_4c:
      /* fetch arguments */
      d0 = ARG3;

      BEGP(pt0);
      deref_head(d0, gatom_4cunk);
      /* argument is nonvar */
    gatom_4cnonvar:
      if (d0 == PREG->y_u.cccc.c3) {
	goto gatom_4d;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_4cunk, gatom_4cnonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.cccc.c3);
      ENDP(pt0);
    gatom_4d:
      /* fetch arguments */
      d0 = ARG4;
      d1 = PREG->y_u.cccc.c4;

      BEGP(pt0);
      deref_head(d0, gatom_4dunk);
      /* argument is nonvar */
    gatom_4dnonvar:
      if (d0 == d1) {
	PREG = NEXTOP(PREG, cccc);
	GONext();
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_4dunk, gatom_4dnonvar);
      /* argument is a variable */
      PREG = NEXTOP(PREG, cccc);
      YapBind(pt0, d1);
      GONext();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(get_5atoms, ccccc);
      BEGD(d0);
      BEGD(d1);
      /* fetch arguments */
      d0 = ARG1;

      BEGP(pt0);
      deref_head(d0, gatom_5unk);
      /* argument is nonvar */
    gatom_5nonvar:
      if (d0 == PREG->y_u.ccccc.c1) {
	goto gatom_5b;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_5unk, gatom_5nonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.ccccc.c1);
      ENDP(pt0);
    gatom_5b:
      /* fetch arguments */
      d0 = ARG2;

      BEGP(pt0);
      deref_head(d0, gatom_5bunk);
      /* argument is nonvar */
    gatom_5bnonvar:
      if (d0 == PREG->y_u.ccccc.c2) {
	goto gatom_5c;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_5bunk, gatom_5bnonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.ccccc.c2);
      ENDP(pt0);
    gatom_5c:
      /* fetch arguments */
      d0 = ARG3;

      BEGP(pt0);
      deref_head(d0, gatom_5cunk);
      /* argument is nonvar */
    gatom_5cnonvar:
      if (d0 == PREG->y_u.ccccc.c3) {
	goto gatom_5d;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_5cunk, gatom_5cnonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.ccccc.c3);
      ENDP(pt0);
    gatom_5d:
      /* fetch arguments */
      d0 = ARG4;

      BEGP(pt0);
      deref_head(d0, gatom_5dunk);
      /* argument is nonvar */
    gatom_5dnonvar:
      if (d0 == PREG->y_u.ccccc.c4) {
	goto gatom_5e;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_5dunk, gatom_5dnonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.ccccc.c4);
      ENDP(pt0);
    gatom_5e:
      /* fetch arguments */
      d0 = ARG5;
      d1 = PREG->y_u.ccccc.c5;

      BEGP(pt0);
      deref_head(d0, gatom_5eunk);
      /* argument is nonvar */
    gatom_5enonvar:
      if (d0 == d1) {
	PREG = NEXTOP(PREG, ccccc);
	GONext();
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_5eunk, gatom_5enonvar);
      /* argument is a variable */
      PREG = NEXTOP(PREG, ccccc);
      YapBind(pt0, d1);
      GONext();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(get_6atoms, cccccc);
      BEGD(d0);
      BEGD(d1);
      /* fetch arguments */
      d0 = ARG1;

      BEGP(pt0);
      deref_head(d0, gatom_6unk);
      /* argument is nonvar */
    gatom_6nonvar:
      if (d0 == PREG->y_u.cccccc.c1) {
	goto gatom_6b;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_6unk, gatom_6nonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.cccccc.c1);
      ENDP(pt0);
    gatom_6b:
      /* fetch arguments */
      d0 = ARG2;

      BEGP(pt0);
      deref_head(d0, gatom_6bunk);
      /* argument is nonvar */
    gatom_6bnonvar:
      if (d0 == PREG->y_u.cccccc.c2) {
	goto gatom_6c;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_6bunk, gatom_6bnonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.cccccc.c2);
      ENDP(pt0);
    gatom_6c:
      /* fetch arguments */
      d0 = ARG3;

      BEGP(pt0);
      deref_head(d0, gatom_6cunk);
      /* argument is nonvar */
    gatom_6cnonvar:
      if (d0 == PREG->y_u.cccccc.c3) {
	goto gatom_6d;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_6cunk, gatom_6cnonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.cccccc.c3);
      ENDP(pt0);
    gatom_6d:
      /* fetch arguments */
      d0 = ARG4;

      BEGP(pt0);
      deref_head(d0, gatom_6dunk);
      /* argument is nonvar */
    gatom_6dnonvar:
      if (d0 == PREG->y_u.cccccc.c4) {
	goto gatom_6e;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_6dunk, gatom_6dnonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.cccccc.c4);
      ENDP(pt0);
    gatom_6e:
      /* fetch arguments */
      d0 = ARG5;

      BEGP(pt0);
      deref_head(d0, gatom_6eunk);
      /* argument is nonvar */
    gatom_6enonvar:
      if (d0 == PREG->y_u.cccccc.c5) {
	goto gatom_6f;
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_6eunk, gatom_6enonvar);
      /* argument is a variable */
      YapBind(pt0, PREG->y_u.cccccc.c5);
      ENDP(pt0);
    gatom_6f:
      /* fetch arguments */
      d0 = ARG6;
      d1 = PREG->y_u.cccccc.c6;

      BEGP(pt0);
      deref_head(d0, gatom_6funk);
      /* argument is nonvar */
    gatom_6fnonvar:
      if (d0 == d1) {
	PREG = NEXTOP(PREG, cccccc);
	GONext();
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_6funk, gatom_6fnonvar);
      /* argument is a variable */
      PREG = NEXTOP(PREG, cccccc);
      YapBind(pt0, d1);
      GONext();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      /* The next instructions can lead to either the READ stream
       * or the write stream */

      OpRW(get_list, x);
      BEGD(d0);
      d0 = XREG(PREG->y_u.x.x);
      deref_head(d0, glist_unk);

    glist_nonvar:
      /* did we find a list? */
      if (!IsPairTerm(d0)) {
	FAIL();
      }
      START_PREFETCH(x);
      PREG = NEXTOP(PREG, x);
      /* enter read mode */
      SREG = RepPair(d0);
      GONext();
      END_PREFETCH();

      BEGP(pt0);
      deref_body(d0, pt0, glist_unk, glist_nonvar);
      /* glist var */
      /* enter write mode */
      CACHE_S();
      S_SREG = HR;
      START_PREFETCH_W(x);
      BEGD(d0);
      d0 = AbsPair(S_SREG);
      YapBindUnsafe(pt0, d0);
      PREG = NEXTOP(PREG, x);
      S_SREG = HR;
      /* don't put an ENDD just after a label */
      HR = S_SREG + 2;
      ENDD(d0);
      WRITEBACK_S(S_SREG);
      GONextW();


      END_PREFETCH_W();
      ENDCACHE_S();
      ENDP(pt0);

      ENDD(d0);
      ENDOpRW();

      OpRW(get_struct, xfa);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xfa.x);
      deref_head(d0, gstruct_unk);

    gstruct_nonvar:
      if (!IsApplTerm(d0))
	FAIL();
      /* we have met a compound term */
      START_PREFETCH(xfa);
      CACHE_S();
      S_SREG = RepAppl(d0);
      /* check functor */
      d0 = (CELL) (PREG->y_u.xfa.f);
      if (*S_SREG != d0) {
	FAIL();
      }
      WRITEBACK_S(S_SREG+1);
      ENDCACHE_S();
      PREG = NEXTOP(PREG, xfa);
      /* enter read mode */
      GONext();
      END_PREFETCH();

      BEGP(pt0);
      deref_body(d0, pt0, gstruct_unk, gstruct_nonvar);
      /* Enter Write mode */
      /* set d1 to be the new structure we are going to create */
      START_PREFETCH_W(xfa);
      BEGD(d1);
      d1 = AbsAppl(HR);
      YapBindUnsafe(pt0, d1);
      /* now, set pt0 to point to the heap where we are going to
       * build our term */
      pt0 = HR;
      ENDD(d1);
      /* first, put the functor */
      d0 = (CELL) (PREG->y_u.xfa.f);
      *pt0++ = d0;
      HR = pt0 + PREG->y_u.xfa.a;
      PREG = NEXTOP(PREG, xfa);
      /* set SREG */
      SREG = pt0;
      /* update HR */
      GONextW();
      END_PREFETCH_W();
      ENDP(pt0);

      ENDD(d0);
      ENDOpRW();

      Op(get_float, xd);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xd.x);
      deref_head(d0, gfloat_unk);

    gfloat_nonvar:
      if (!IsApplTerm(d0))
	FAIL();
      /* we have met a preexisting float */
      START_PREFETCH(xd);
      BEGP(pt0);
      pt0 = RepAppl(d0);
      /* check functor */
      if (*pt0 != (CELL)FunctorDouble) {
	FAIL();
      }
      BEGP(pt1);
      pt1 = PREG->y_u.xd.d;
      PREG = NEXTOP(PREG, xd);
      if (
	  pt1[1] != pt0[1]
#if SIZEOF_DOUBLE == 2*SIZEOF_INT_P
	  || pt1[2] != pt0[2]
#endif
	  ) FAIL();
      ENDP(pt1);
      ENDP(pt0);
      /* enter read mode */
      GONext();
      END_PREFETCH();

      BEGP(pt0);
      deref_body(d0, pt0, gfloat_unk, gfloat_nonvar);
      /* Enter Write mode */
      /* set d1 to be the new structure we are going to create */
      START_PREFETCH(xc);
      BEGD(d1);
      d1 = AbsAppl(PREG->y_u.xd.d);
      PREG = NEXTOP(PREG, xd);
      YapBind(pt0, d1);
      GONext();
      ENDD(d1);
      END_PREFETCH();
      ENDP(pt0);

      ENDD(d0);
      ENDOp();

      Op(get_string, xu);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xu.x);
      deref_head(d0, gstring_unk);

    gstring_nonvar:
      if (!IsApplTerm(d0))
	FAIL();
      /* we have met a preexisting string */
      START_PREFETCH(xu);
      BEGP(pt0);
      pt0 = RepAppl(d0);
      /* check functor */
      if (*pt0 != (CELL)FunctorString) {
	FAIL();
      }
      BEGP(pt1);
      pt1 = RepAppl(PREG->y_u.xu.ut);
      PREG = NEXTOP(PREG, xu);
      if (
	  pt1[1] != pt0[1] ||
	  strcmp((const char *)(pt1+2), (const char *)(pt0+2))
	  ) FAIL();
      ENDP(pt1);
      ENDP(pt0);
      /* enter read mode */
      GONext();
      END_PREFETCH();

      BEGP(pt0);
      deref_body(d0, pt0, gstring_unk, gstring_nonvar);
      /* Enter Write mode */
      /* set d1 to be the new structure we are going to create */
      START_PREFETCH(xc);
      BEGD(d1);
      d1 = PREG->y_u.xu.ut;
      PREG = NEXTOP(PREG, xu);
      YapBind(pt0, d1);
      GONext();
      ENDD(d1);
      END_PREFETCH();
      ENDP(pt0);

      ENDD(d0);
      ENDOp();

      Op(get_longint, xi);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xi.x);
      deref_head(d0, glongint_unk);

    glongint_nonvar:
      if (!IsApplTerm(d0))
	FAIL();
      /* we have met a preexisting longint */
      START_PREFETCH(xi);
      BEGP(pt0);
      pt0 = RepAppl(d0);
      /* check functor */
      if (*pt0 != (CELL)FunctorLongInt) {
	FAIL();
      }
      if (PREG->y_u.xi.i[1] != (CELL)pt0[1]) FAIL();
      ENDP(pt0);
      PREG = NEXTOP(PREG, xi);
      /* enter read mode */
      GONext();
      END_PREFETCH();

      BEGP(pt0);
      deref_body(d0, pt0, glongint_unk, glongint_nonvar);
      /* Enter Write mode */
      /* set d1 to be the new structure we are going to create */
      START_PREFETCH(xi);
      BEGD(d1);
      d1 = AbsAppl(PREG->y_u.xi.i);
      PREG = NEXTOP(PREG, xi);
      YapBind(pt0, d1);
      GONext();
      ENDD(d1);
      END_PREFETCH();
      ENDP(pt0);

      ENDD(d0);
      ENDOp();

      Op(get_bigint, xN);
#ifdef USE_GMP
      BEGD(d0);
      d0 = XREG(PREG->y_u.xN.x);
      deref_head(d0, gbigint_unk);

    gbigint_nonvar:
      if (!IsApplTerm(d0))
	FAIL();
      /* we have met a preexisting bigint */
      START_PREFETCH(xN);
      BEGP(pt0);
      pt0 = RepAppl(d0);
      /* check functor */
      if (*pt0 != (CELL)FunctorBigInt)
	{
	  FAIL();
	}
      if (Yap_gmp_tcmp_big_big(d0,PREG->y_u.xN.b))
	FAIL();
      PREG = NEXTOP(PREG, xN);
      ENDP(pt0);
      /* enter read mode */
      GONext();
      END_PREFETCH();

      BEGP(pt0);
      deref_body(d0, pt0, gbigint_unk, gbigint_nonvar);
      /* Enter Write mode */
      /* set d1 to be the new structure we are going to create */
      START_PREFETCH(xN);
      BEGD(d1);
      d1 = PREG->y_u.xN.b;
      PREG = NEXTOP(PREG, xN);
      YapBind(pt0, d1);
      GONext();
      ENDD(d1);
      END_PREFETCH();
      ENDP(pt0);

      ENDD(d0);
#else
      FAIL();
#endif
      ENDOp();


      Op(get_dbterm, xD);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xD.x);
      deref_head(d0, gdbterm_unk);

    gdbterm_nonvar:
      BEGD(d1);
      /* we have met a preexisting dbterm */
      d1 = PREG->y_u.xD.D;
      PREG = NEXTOP(PREG, xD);
      UnifyBound(d0,d1);
      ENDD(d1);

      BEGP(pt0);
      deref_body(d0, pt0, gdbterm_unk, gdbterm_nonvar);
      /* Enter Write mode */
      /* set d1 to be the new structure we are going to create */
      START_PREFETCH(xD);
      BEGD(d1);
      d1 = PREG->y_u.xD.D;
      PREG = NEXTOP(PREG, xD);
      YapBind(pt0, d1);
      GONext();
      ENDD(d1);
      END_PREFETCH();
      ENDP(pt0);

      ENDD(d0);
      ENDOp();

      /************************************************************************\
       *    Optimised Get List Instructions                                   *
\************************************************************************/

      OpRW(glist_valx, xx);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xx.xl);
      deref_head(d0, glist_valx_write);
    glist_valx_read:
      BEGP(pt0);
      /* did we find a list? */
      if (!IsPairTerm(d0))
	FAIL();
      /* enter read mode */
      START_PREFETCH(xx);
      pt0 = RepPair(d0);
      SREG = pt0 + 1;
      /* start unification with first argument */
      d0 = *pt0;
      deref_head(d0, glist_valx_unk);

      /* first argument is in d0 */
    glist_valx_nonvar:
      /* first argument is bound */
      BEGD(d1);
      d1 = XREG(PREG->y_u.xx.xr);
      deref_head(d1, glist_valx_nonvar_unk);

    glist_valx_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, xx);
      UnifyBound(d0, d1);

      BEGP(pt1);
      /* deref second argument */
      deref_body(d1, pt1, glist_valx_nonvar_unk, glist_valx_nonvar_nonvar);
      /* head bound, argument unbound */
      PREG = NEXTOP(PREG, xx);
      YapBind(pt1, d0);
      GONext();
      ENDP(pt1);


      ENDD(d1);

      /* head may be unbound */
      derefa_body(d0, pt0, glist_valx_unk, glist_valx_nonvar);
      /* head is unbound, pt0 has the value */
      d0 = XREG(PREG->y_u.xx.xr);
      deref_head(d0, glist_valx_var_unk);

    glist_valx_var_nonvar:
      /* head is unbound, second arg bound */
      PREG = NEXTOP(PREG, xx);
      Bind_Global(pt0, d0);
      GONext();

      BEGP(pt1);
      deref_body(d0, pt1, glist_valx_var_unk, glist_valx_var_nonvar);
      /* head and second argument are unbound */
      PREG = NEXTOP(PREG, xx);
      UnifyGlobalCellToCell(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDP(pt0);
      END_PREFETCH();

      BEGP(pt0);
      deref_body(d0, pt0, glist_valx_write, glist_valx_read);
      CACHE_S();
      /* enter write mode */
      S_SREG = HR;
      BEGD(d1);
      d1 = XREG(PREG->y_u.xx.xr);
      d0 = AbsPair(S_SREG);
      S_SREG[0] = d1;
      ENDD(d1);
      ALWAYS_START_PREFETCH_W(xx);
      HR = S_SREG + 2;
      WRITEBACK_S(S_SREG+1);
      YapBindUnsafe(pt0, d0);
      PREG = NEXTOP(PREG, xx);
      ALWAYS_GONextW();
      ALWAYS_END_PREFETCH_W();
      ENDCACHE_S();
      ENDP(pt0);

      ENDD(d0);
      ENDOpRW();

      OpRW(glist_valy, yx);
      BEGD(d0);
      d0 = XREG(PREG->y_u.yx.x);
      deref_head(d0, glist_valy_write);
    glist_valy_read:
      BEGP(pt0);
      /* did we find a list? */
      if (!IsPairTerm(d0))
	FAIL();
      START_PREFETCH(yx);
      /* enter read mode */
      pt0 = RepPair(d0);
      SREG = pt0 + 1;
      /* start unification with first argument */
      d0 = *pt0;
      deref_head(d0, glist_valy_unk);

    glist_valy_nonvar:
      /* first argument is bound */
      BEGD(d1);
      BEGP(pt1);
      pt1 = YREG + PREG->y_u.yx.y;
      d1 = *pt1;
      PREG = NEXTOP(PREG, yx);
      deref_head(d1, glist_valy_nonvar_unk);

    glist_valy_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      SREG = pt0 + 1;
      UnifyBound(d0, d1);

      /* deref second argument */
      derefa_body(d1, pt1, glist_valy_nonvar_unk, glist_valy_nonvar_nonvar);
      /* first argument bound, second unbound */
      YapBind(pt1, d0);
      GONext();


      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, glist_valy_unk, glist_valy_nonvar);
      /* first argument is unbound */
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.yx.y;
      d1 = *pt1;
      deref_head(d1, glist_valy_var_unk);
    glist_valy_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, yx);
      Bind_Global(pt0, d1);
      GONext();

      derefa_body(d1, pt1, glist_valy_var_unk, glist_valy_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, yx);
      UnifyGlobalCellToCell(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDD(d1);

      END_PREFETCH();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d0, pt0, glist_valy_write, glist_valy_read);
      /* enter write mode */
      START_PREFETCH_W(yx);
      BEGP(pt1);
      pt1 = HR;
      d0 = AbsPair(pt1);
      YapBindUnsafe(pt0, d0);
      BEGD(d0);
      /* include XREG on it */
      d0 = YREG[PREG->y_u.yx.y];
      pt1[0] = d0;
      ENDD(d0);
      HR = pt1 + 2;
      SREG = pt1 + 1;
      ENDP(pt1);
      PREG = NEXTOP(PREG, yx);
      GONextW();
      END_PREFETCH_W();
      ENDP(pt0);

      ENDD(d0);
      ENDOpRW();

      Op(gl_void_varx, xx);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xx.xl);
      deref_head(d0, glist_void_varx_write);
    glist_void_varx_read:
      /* did we find a list? */
      if (!IsPairTerm(d0))
	FAIL();
      ALWAYS_START_PREFETCH(xx);
      /* enter read mode */
      BEGP(pt0);
      pt0 = RepPair(d0);
      d0 = pt0[1];
      XREG(PREG->y_u.xx.xr) = d0;
      PREG = NEXTOP(PREG, xx);
      ALWAYS_GONext();
      ENDP(pt0);
      ALWAYS_END_PREFETCH();

      BEGP(pt0);
      deref_body(d0, pt0, glist_void_varx_write, glist_void_varx_read);
      /* enter write mode */
      BEGP(pt1);
      pt1 = HR;
      /* include XREG on it */
      XREG(PREG->y_u.xx.xr) =
	Unsigned(pt1 + 1);
      RESET_VARIABLE(pt1);
      RESET_VARIABLE(pt1+1);
      HR = pt1 + 2;
      BEGD(d0);
      d0 = AbsPair(pt1);
      YapBind(pt0, d0);
      PREG = NEXTOP(PREG, xx);
      ENDD(d0);
      ENDP(pt1);
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(gl_void_vary, yx);
      BEGD(d0);
      d0 = XREG(PREG->y_u.yx.x);
      deref_head(d0, glist_void_vary_write);
    glist_void_vary_read:
      /* did we find a list? */
      if (!IsPairTerm(d0))
	FAIL();
      /* enter read mode */
      BEGP(pt0);
      pt0 = RepPair(d0);
      d0 = pt0[1];
      ENDP(pt0);
      INITIALIZE_PERMVAR(YREG+PREG->y_u.yx.y,d0);
      PREG = NEXTOP(PREG, yx);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, glist_void_vary_write, glist_void_vary_read);
      /* enter write mode */
      BEGP(pt1);
      pt1 = HR;
      /* include XREG on it */
      INITIALIZE_PERMVAR(YREG+PREG->y_u.yx.y,Unsigned(pt1 + 1));
      PREG = NEXTOP(PREG, yx);
      RESET_VARIABLE(pt1);
      RESET_VARIABLE(pt1+1);
      d0 = AbsPair(pt1);
      HR = pt1 + 2;
      YapBind(pt0, d0);
      GONext();
      ENDP(pt1);
      ENDP(pt0);

      ENDD(d0);
      ENDOp();

      Op(gl_void_valx, xx);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xx.xl);
      deref_head(d0, glist_void_valx_write);
    glist_void_valx_read:
      BEGP(pt0);
      /* did we find a list? */
      if (!IsPairTerm(d0))
	FAIL();
      /* enter read mode */
      pt0 = RepPair(d0)+1;
      /* start unification with first argument */
      d0 = *pt0;
      deref_head(d0, glist_void_valx_unk);

    glist_void_valx_nonvar:
      /* first argument is bound */
      BEGD(d1);
      d1 = XREG(PREG->y_u.xx.xr);
      deref_head(d1, glist_void_valx_nonvar_unk);

    glist_void_valx_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, xx);
      UnifyBound(d0, d1);

      /* deref second argument */
      BEGP(pt1);
      deref_body(d1, pt1, glist_void_valx_nonvar_unk, glist_void_valx_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, xx);
      YapBind(pt1, d0);
      GONext();
      ENDP(pt1);
      ENDD(d1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, glist_void_valx_unk, glist_void_valx_nonvar);
      /* first argument is unbound */
      BEGD(d1);
      d1 = XREG(PREG->y_u.xx.xr);
      deref_head(d1, glist_void_valx_var_unk);

    glist_void_valx_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, xx);
      Bind_Global(pt0, d1);
      GONext();

      BEGP(pt1);
      deref_body(d1, pt1, glist_void_valx_var_unk, glist_void_valx_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, xx);
      UnifyGlobalCellToCell(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDD(d1);
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d0, pt0, glist_void_valx_write, glist_void_valx_read);
      /* enter write mode */
      BEGP(pt1);
      pt1 = HR;
      d0 = AbsPair(pt1);
      YapBindUnsafe(pt0, d0);
      pt1 = HR;
      BEGD(d0);
      /* include XREG on it */
      d0 = XREG(PREG->y_u.xx.xr);
      RESET_VARIABLE(pt1);
      pt1[1] = d0;
      HR = pt1 + 2;
      ENDD(d0);
      ENDP(pt1);
      PREG = NEXTOP(PREG, xx);
      GONext();
      ENDP(pt0);

      ENDD(d0);
      ENDOp();

      Op(gl_void_valy, yx);
      BEGD(d0);
      d0 = XREG(PREG->y_u.yx.x);
      deref_head(d0, glist_void_valy_write);
    glist_void_valy_read:
      BEGP(pt0);
      /* did we find a list? */
      if (!IsPairTerm(d0))
	FAIL();
      /* enter read mode */
      pt0 = RepPair(d0)+1;
      /* start unification with first argument */
      d0 = *pt0;
      deref_head(d0, glist_void_valy_unk);

    glist_void_valy_nonvar:
      /* first argument is bound */
      BEGD(d1);
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.yx.y;
      d1 = *pt1;
      deref_head(d1, glist_void_valy_nonvar_unk);

    glist_void_valy_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, yx);
      UnifyBound(d0, d1);

      /* deref second argument */
      derefa_body(d1, pt1, glist_void_valy_nonvar_unk, glist_void_valy_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, yx);
      YapBind(pt1, d0);
      GONext();

      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, glist_void_valy_unk, glist_void_valy_nonvar);
      /* first argument is unbound */
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.yx.y;
      d1 = *pt1;
      deref_head(d1, glist_void_valy_var_unk);

    glist_void_valy_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, yx);
      Bind_Global(pt0, d1);
      GONext();

      deref_body(d1, pt1, glist_void_valy_var_unk, glist_void_valy_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, yx);
      UnifyGlobalCellToCell(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDD(d1);
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d0, pt0, glist_void_valy_write, glist_void_valy_read);
      /* enter write mode */
      CACHE_S();
      S_SREG = HR;
      d0 = AbsPair(S_SREG);
      YapBindUnsafe(pt0, d0);
      S_SREG = HR;
      /* include XREG on it */
      BEGD(d1);
      d1 = YREG[PREG->y_u.yx.y];
      RESET_VARIABLE(S_SREG);
      S_SREG[1] = d1;
      ENDD(d1);
      PREG = NEXTOP(PREG, yx);
      HR = S_SREG + 2;
      ENDCACHE_S();
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();



      /************************************************************************\
       *      Unify instructions                                              *
\************************************************************************/

      Op(unify_x_var, ox);
      CACHE_S();
      READ_IN_S();
      BEGD(d0);
      d0 = *S_SREG;
#ifdef YAPOR_SBA
      if (d0 == 0)
	d0 = (CELL)S_SREG;
#endif
      WRITEBACK_S(S_SREG+1);
      ALWAYS_START_PREFETCH(ox);
      XREG(PREG->y_u.ox.x) = d0;
      PREG = NEXTOP(PREG, ox);
      ALWAYS_GONext();
      ALWAYS_END_PREFETCH();
      ENDD(d0);
      ENDCACHE_S();
      ENDOp();

      OpW(unify_x_var_write, ox);
      CACHE_S();
      READ_IN_S();
      BEGP(pt0);
      pt0 = &XREG(PREG->y_u.ox.x);
      PREG = NEXTOP(PREG, ox);
      RESET_VARIABLE(S_SREG);
      *pt0 = (CELL) S_SREG;
      WRITEBACK_S(S_SREG+1);
      ENDP(pt0);
      ENDCACHE_S();
      GONextW();
      ENDOpW();

      BOp(unify_l_x_var, ox);
      ALWAYS_START_PREFETCH(ox);
      BEGP(pt0);
      BEGD(d0);
      d0 = SREG[0];
      pt0 = &XREG(PREG->y_u.ox.x);
      PREG = NEXTOP(PREG, ox);
#ifdef YAPOR_SBA
      if (d0 == 0)
	d0 = (CELL)SREG;
#endif
      *pt0 = d0;
      ALWAYS_GONext();
      ENDD(d0);
      ENDP(pt0);
      ALWAYS_END_PREFETCH();
      ENDBOp();

      BOp(unify_l_x_var_write, ox);
      ALWAYS_START_PREFETCH(ox);
      CACHE_S();
      READ_IN_S();
      BEGP(pt0);
      pt0 = &XREG(PREG->y_u.ox.x);
      PREG = NEXTOP(PREG, ox);
      RESET_VARIABLE(S_SREG);
      *pt0 = (CELL)S_SREG;
      ENDP(pt0);
      ENDCACHE_S();
      ALWAYS_GONext();
      ENDBOp();
      ALWAYS_END_PREFETCH();

      BOp(unify_x_var2, oxx);
      CACHE_S();
      ALWAYS_START_PREFETCH(oxx);
      READ_IN_S();
      BEGP(pt0);
      pt0 = &XREG(PREG->y_u.oxx.xr);
      BEGD(d0);
      d0 = S_SREG[0];
      BEGD(d1);
      d1 = S_SREG[1];
#ifdef YAPOR_SBA
      if (d0 == 0)
	d0 = (CELL)S_SREG;
      if (d1 == 0)
	d1 = (CELL)(S_SREG+1);
#endif
      WRITEBACK_S(S_SREG+2);
      XREG(PREG->y_u.oxx.xl) = d0;
      PREG = NEXTOP(PREG, oxx);
      *pt0 = d1;
      ENDD(d0);
      ENDD(d1);
      ENDP(pt0);
      ALWAYS_GONext();
      ENDBOp();
      ALWAYS_END_PREFETCH();
      ENDCACHE_S();

      OpW(unify_x_var2_write, oxx);
      CACHE_S();
      READ_IN_S();
      BEGP(pt0);
      pt0 = &XREG(PREG->y_u.oxx.xr);
      RESET_VARIABLE(S_SREG);
      XREG(PREG->y_u.oxx.xl) = (CELL) S_SREG;
      S_SREG++;
      PREG = NEXTOP(PREG, oxx);
      RESET_VARIABLE(S_SREG);
      *pt0 = (CELL) S_SREG;
      ENDP(pt0);
      WRITEBACK_S(S_SREG+1);
      ENDCACHE_S();
      GONextW();
      ENDOpW();

      BOp(unify_l_x_var2, oxx);
      ALWAYS_START_PREFETCH(oxx);
      CACHE_S();
      READ_IN_S();
      BEGP(pt0);
      pt0 = &XREG(PREG->y_u.oxx.xr);
      BEGD(d0);
      d0 = S_SREG[0];
      BEGD(d1);
      d1 = S_SREG[1];
#ifdef YAPOR_SBA
      if (d0 == 0)
	XREG(PREG->y_u.oxx.xl) = (CELL)S_SREG;
      else
#endif
	XREG(PREG->y_u.oxx.xl) = d0;
      PREG = NEXTOP(PREG, oxx);
#ifdef YAPOR_SBA
      if (d1 == 0)
	*pt0 = (CELL)(S_SREG+1);
      else
#endif
	*pt0 = d1;
      ENDD(d0);
      ENDD(d1);
      ENDP(pt0);
      ENDCACHE_S();
      ALWAYS_GONext();
      ENDBOp();
      ALWAYS_END_PREFETCH();

      Op(unify_l_x_var2_write, oxx);
      CACHE_S();
      READ_IN_S();
      BEGP(pt0);
      pt0 = &XREG(PREG->y_u.oxx.xr);
      XREG(PREG->y_u.oxx.xl) = (CELL) S_SREG;
      RESET_VARIABLE(S_SREG);
      S_SREG++;
      *pt0 = (CELL) S_SREG;
      PREG = NEXTOP(PREG, oxx);
      RESET_VARIABLE(S_SREG);
      ENDP(pt0);
      ENDCACHE_S();
      GONext();
      ENDOp();

      Op(unify_y_var, oy);
      BEGD(d0);
      d0 = *SREG++;
#ifdef YAPOR_SBA
      if (d0 == 0) {
	INITIALIZE_PERMVAR(YREG+PREG->y_u.oy.y,(CELL)(SREG-1));
      } else
#else
	INITIALIZE_PERMVAR(YREG+PREG->y_u.oy.y,d0);
#endif /* YAPOR_SBA */
      PREG = NEXTOP(PREG, oy);
      GONext();
      ENDD(d0);
      ENDOp();

      OpW(unify_y_var_write, oy);
      CACHE_S();
      READ_IN_S();
      INITIALIZE_PERMVAR(YREG+PREG->y_u.oy.y,(CELL) S_SREG);
      PREG = NEXTOP(PREG, oy);
      RESET_VARIABLE(S_SREG);
      WRITEBACK_S(S_SREG+1);
      ENDCACHE_S();
      GONextW();
      ENDOpW();

      Op(unify_l_y_var, oy);
      BEGD(d0);
      d0 = SREG[0];
#ifdef YAPOR_SBA
      if (d0 == 0) {
	INITIALIZE_PERMVAR(YREG+PREG->y_u.oy.y,(CELL)SREG);
      } else
#else
	INITIALIZE_PERMVAR(YREG+PREG->y_u.oy.y,d0);
#endif /* YAPOR_SBA */
      PREG = NEXTOP(PREG, oy);
      GONext();
      ENDD(d0);
      ENDOp();

      Op(unify_l_y_var_write, oy);
      CACHE_S();
      READ_IN_S();
      INITIALIZE_PERMVAR(YREG+PREG->y_u.oy.y,(CELL) S_SREG);
      PREG = NEXTOP(PREG, oy);
      RESET_VARIABLE(S_SREG);
      ENDCACHE_S();
      GONext();
      ENDOp();

      /* We assume the value in X is pointing to an object in the
       * global stack */
      Op(unify_x_val, ox);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, uvalx_unk);

    uvalx_nonvar:
      /* first argument is bound */
      d1 = XREG(PREG->y_u.ox.x);
      deref_head(d1, uvalx_nonvar_unk);

    uvalx_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, ox);
      SREG++;
      UnifyBound(d0, d1);

      /* deref second argument */
      /* pt0 is in the structure and pt1 the register */
      BEGP(pt1);
      deref_body(d1, pt1, uvalx_nonvar_unk, uvalx_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, ox);
      SREG++;
      YapBind(pt1, d0);
      GONext();
      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, uvalx_unk, uvalx_nonvar);
      /* first argument is unbound */
      d1 = XREG(PREG->y_u.ox.x);
      deref_head(d1, uvalx_var_unk);

    uvalx_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, ox);
      SREG++;
      Bind_Global(pt0, d1);
      GONext();

      BEGP(pt1);
      deref_body(d1, pt1, uvalx_var_unk, uvalx_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, ox);
      SREG++;
      UnifyGlobalCellToCell(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      OpW(unify_x_val_write, ox);
      /* we are in write mode */
      *SREG++ = XREG(PREG->y_u.ox.x);
      PREG = NEXTOP(PREG, ox);
      GONextW();
      ENDOpW();

      /* We assume the value in X is pointing to an object in the
       * global stack */
      Op(unify_l_x_val, ox);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, ulvalx_unk);

    ulvalx_nonvar:
      /* first argument is bound */
      d1 = XREG(PREG->y_u.ox.x);
      deref_head(d1, ulvalx_nonvar_unk);

    ulvalx_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, ox);
      UnifyBound(d0, d1);

      BEGP(pt1);
      /* deref second argument */
      deref_body(d1, pt1, ulvalx_nonvar_unk, ulvalx_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, ox);
      YapBind(pt1, d0);
      GONext();
      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, ulvalx_unk, ulvalx_nonvar);
      /* first argument is unbound */
      d1 = XREG(PREG->y_u.ox.x);
      deref_head(d1, ulvalx_var_unk);

    ulvalx_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, ox);
      Bind_Global(pt0, d1);
      GONext();

      BEGP(pt1);
      deref_body(d1, pt1, ulvalx_var_unk, ulvalx_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, ox);
      UnifyGlobalCellToCell(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(unify_l_x_val_write, ox);
      /* we are in write mode */
      SREG[0] = XREG(PREG->y_u.ox.x);
      PREG = NEXTOP(PREG, ox);
      GONext();
      ENDOp();

      /* We assume the value in X is pointing to an object in the
       * global stack */
      Op(unify_y_val, oy);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, uvaly_unk);

    uvaly_nonvar:
      /* first argument is bound */
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.oy.y;
      d1 = *pt1;
      deref_head(d1, uvaly_nonvar_unk);

    uvaly_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, oy);
      SREG++;
      UnifyBound(d0, d1);

      /* deref second argument */
      derefa_body(d1, pt1, uvaly_nonvar_unk, uvaly_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, oy);
      SREG++;
      YapBind(pt1, d0);
      GONext();
      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, uvaly_unk, uvaly_nonvar);
      /* first argument is unbound */
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.oy.y;
      d1 = *pt1;
      deref_head(d1, uvaly_var_unk);

    uvaly_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, oy);
      SREG++;
      Bind_Global(pt0, d1);
      GONext();

      derefa_body(d1, pt1, uvaly_var_unk, uvaly_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, oy);
      SREG++;
      UnifyGlobalCellToCell(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      OpW(unify_y_val_write, oy);
      /* we are in write mode */
      BEGD(d0);
      d0 = YREG[PREG->y_u.oy.y];
#ifdef YAPOR_SBA
      if (d0 == 0) /* free variable */
	*SREG++ = (CELL)(YREG+PREG->y_u.oy.y);
      else
#endif
	*SREG++ = d0;
      ENDD(d0);
      PREG = NEXTOP(PREG, oy);
      GONextW();
      ENDOpW();

      /* We assume the value in X is pointing to an object in the
       * global stack */
      Op(unify_l_y_val, oy);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, ulvaly_unk);

    ulvaly_nonvar:
      /* first argument is bound */
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.oy.y;
      d1 = *pt1;
      deref_head(d1, ulvaly_nonvar_unk);

    ulvaly_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, oy);
      UnifyBound(d0, d1);

      /* deref second argument */
      derefa_body(d1, pt1, ulvaly_nonvar_unk, ulvaly_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, oy);
      YapBind(pt1, d0);
      GONext();
      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, ulvaly_unk, ulvaly_nonvar);
      /* first argument is unbound */
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.oy.y;
      d1 = *pt1;
      deref_head(d1, ulvaly_var_unk);

    ulvaly_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, oy);
      Bind_Global(pt0, d1);
      GONext();

      /* Here we are in trouble: we have a clash between pt1 and
       * SREG. We address this by storing SREG in d0 for the duration. */
      derefa_body(d1, pt1, ulvaly_var_unk, ulvaly_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, oy);
      UnifyGlobalCellToCell(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(unify_l_y_val_write, oy);
      /* we are in write mode */
      BEGD(d0);
      d0 = YREG[PREG->y_u.oy.y];
#ifdef YAPOR_SBA
      if (d0 == 0) /* new variable */
	SREG[0] = (CELL)(YREG+PREG->y_u.oy.y);
      else
#endif
	SREG[0] = d0;
      ENDD(d0);
      PREG = NEXTOP(PREG, oy);
      GONext();
      ENDOp();

      /* In the next instructions, we do not know anything about
       * what is in X */
      Op(unify_x_loc, ox);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, uvalx_loc_unk);

    uvalx_loc_nonvar:
      /* first argument is bound */
      d1 = XREG(PREG->y_u.ox.x);
      deref_head(d1, uvalx_loc_nonvar_unk);

    uvalx_loc_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, ox);
      SREG++;
      UnifyBound(d0, d1);

      BEGP(pt1);
      /* deref second argument */
      deref_body(d1, pt1, uvalx_loc_nonvar_unk, uvalx_loc_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, ox);
      SREG++;
      YapBind(pt1, d0);
      GONext();
      ENDP(pt1);


      /* first argument may be unbound */
      derefa_body(d0, pt0, uvalx_loc_unk, uvalx_loc_nonvar);
      /* first argument is unbound */
      d1 = XREG(PREG->y_u.ox.x);
      deref_head(d1, uvalx_loc_var_unk);

    uvalx_loc_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, ox);
      SREG++;
      Bind_Global(pt0, d1);
      GONext();

      /* Here we are in trouble: we have a clash between pt1 and
       * SREG. We address this by storing SREG in d0 for the duration. */
      BEGP(pt1);
      deref_body(d1, pt1, uvalx_loc_var_unk, uvalx_loc_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, ox);
      SREG++;
      UnifyCells(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      OpW(unify_x_loc_write, ox);
      /* we are in write mode */
      BEGD(d0);
      d0 = XREG(PREG->y_u.ox.x);
      deref_head(d0, unify_x_loc_unk);
    unify_x_loc_nonvar:
      *SREG++ = d0;
      PREG = NEXTOP(PREG, ox);
      GONextW();

      BEGP(pt0);
      deref_body(d0, pt0, unify_x_loc_unk, unify_x_loc_nonvar);
      /* move ahead in the instructions */
      PREG = NEXTOP(PREG, ox);
      /* d0 is a variable, check whether we need to globalise it */
      if (pt0 < HR) {
	/* variable is global */
	*SREG++ = Unsigned(pt0);
	GONextW();
      }
      else {
	/* bind our variable to the structure */
	CACHE_S();
	READ_IN_S();
	Bind_Local(pt0, Unsigned(S_SREG));
	RESET_VARIABLE(S_SREG);
	WRITEBACK_S(S_SREG+1);
	ENDCACHE_S();
	GONextW();
      }
      ENDP(pt0);
      ENDD(d0);
      ENDOpW();

      /* In the next instructions, we do not know anything about
       * what is in X */
      Op(unify_l_x_loc, ox);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, ulvalx_loc_unk);

    ulvalx_loc_nonvar:
      /* first argument is bound */
      d1 = XREG(PREG->y_u.ox.x);
      deref_head(d1, ulvalx_loc_nonvar_unk);

    ulvalx_loc_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, ox);
      UnifyBound(d0, d1);

      /* deref second argument */
      deref_body(d1, pt0, ulvalx_loc_nonvar_unk, ulvalx_loc_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, ox);
      YapBind(pt0, d0);
      GONext();

      /* first argument may be unbound */
      derefa_body(d0, pt0, ulvalx_loc_unk, ulvalx_loc_nonvar);
      /* first argument is unbound */
      d1 = XREG(PREG->y_u.ox.x);
      deref_head(d1, ulvalx_loc_var_unk);

    ulvalx_loc_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, ox);
      Bind_Global(pt0, d1);
      GONext();

      BEGP(pt1);
      deref_body(d1, pt1, ulvalx_loc_var_unk, ulvalx_loc_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, ox);
      UnifyGlobalCellToCell(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(unify_l_x_loc_write, ox);
      /* we are in write mode */
      BEGD(d0);
      d0 = XREG(PREG->y_u.ox.x);
      deref_head(d0, ulnify_x_loc_unk);
    ulnify_x_loc_nonvar:
      SREG[0] = d0;
      PREG = NEXTOP(PREG, ox);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, ulnify_x_loc_unk, ulnify_x_loc_nonvar);
      /* d0 is a variable, check whether we need to globalise it */
      PREG = NEXTOP(PREG, ox);
      if (pt0 < HR) {
	/* variable is global */
	SREG[0] = Unsigned(pt0);
	GONext();
      }
      else {
	/* create a new Heap variable and bind our variable to it */
	Bind_Local(pt0, Unsigned(SREG));
	RESET_VARIABLE(SREG);
	GONext();
      }
      ENDP(pt0);
      ENDD(d0);
      ENDOpW();

      Op(unify_y_loc, oy);
      /* we are in read mode */
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, uvaly_loc_unk);

    uvaly_loc_nonvar:
      /* structure is bound */
      BEGP(pt1);
      pt1 =  YREG+PREG->y_u.oy.y;
      d1 = *pt1;
      deref_head(d1, uvaly_loc_nonvar_unk);

    uvaly_loc_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, oy);
      SREG++;
      UnifyBound(d0, d1);

      /* deref second argument */
      derefa_body(d1, pt1, uvaly_loc_nonvar_unk, uvaly_loc_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, oy);
      SREG++;
      YapBind(pt1, d0);
      GONext();
      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, uvaly_loc_unk, uvaly_loc_nonvar);
      /* first argument is unbound */
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.oy.y;
      d1 = *pt1;
      deref_head(d1, uvaly_loc_var_unk);

    uvaly_loc_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, oy);
      SREG++;
      Bind_Global(pt0, d1);
      GONext();

      /* Here we are in trouble: we have a clash between pt1 and
       * SREG. We address this by storing SREG in d0 for the duration. */
      derefa_body(d1, pt1, uvaly_loc_var_unk, uvaly_loc_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, oy);
      SREG++;
      UnifyCells(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      OpW(unify_y_loc_write, oy);
      /* we are in write mode */
      BEGD(d0);
      BEGP(pt0);
      pt0 = YREG+PREG->y_u.oy.y;
      d0 = *pt0;
      deref_head(d0, unify_y_loc_unk);
    unify_y_loc_nonvar:
      *SREG++ = d0;
      PREG = NEXTOP(PREG, oy);
      GONextW();

      derefa_body(d0, pt0, unify_y_loc_unk, unify_y_loc_nonvar);
      /* d0 is a variable, check whether we need to globalise it */
      PREG = NEXTOP(PREG, oy);
      if (pt0 < HR) {
	/* variable is global */
	*SREG++ = Unsigned(pt0);
	GONextW();
      }
      else {
	/* create a new Heap variable and bind our variable to it */
	CACHE_S();
	READ_IN_S();
	Bind_Local(pt0, Unsigned(S_SREG));
	RESET_VARIABLE(S_SREG);
	WRITEBACK_S(S_SREG+1);
	ENDCACHE_S();
	GONextW();
      }
      ENDP(pt0);
      ENDD(d0);
      ENDOpW();

      Op(unify_l_y_loc, oy);
      /* else we are in read mode */
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, ulvaly_loc_unk);

    ulvaly_loc_nonvar:
      /* structure is bound */
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.oy.y;
      d1 = *pt1;
      deref_head(d1, ulvaly_loc_nonvar_unk);

    ulvaly_loc_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, oy);
      UnifyBound(d0, d1);

      /* deref second argument */
      derefa_body(d1, pt1, ulvaly_loc_nonvar_unk, ulvaly_loc_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, oy);
      YapBind(pt1, d0);
      GONext();
      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, ulvaly_loc_unk, ulvaly_loc_nonvar);
      /* first argument is unbound */
      BEGP(pt1);
      pt1 = YREG+PREG->y_u.oy.y;
      d1 = *pt1;
      deref_head(d1, ulvaly_loc_var_unk);

    ulvaly_loc_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, oy);
      Bind_Global(pt0, d1);
      GONext();

      /* Here we are in trouble: we have a clash between pt1 and
       * SREG. We address this by storing SREG in d0 for the duration. */
      derefa_body(d1, pt1, ulvaly_loc_var_unk, ulvaly_loc_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, oy);
      UnifyGlobalCellToCell(pt0, pt1);
      GONext();
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(unify_l_y_loc_write, oy);
      /* we are in write mode */
      BEGD(d0);
      BEGP(pt0);
      pt0 = YREG+PREG->y_u.oy.y;
      d0 = *pt0;
      deref_head(d0, ulunify_y_loc_unk);
    ulunify_y_loc_nonvar:
      SREG[0] = d0;
      PREG = NEXTOP(PREG, oy);
      GONext();

      derefa_body(d0, pt0, ulunify_y_loc_unk, ulunify_y_loc_nonvar);
      /* d0 is a variable, check whether we need to globalise it */
      PREG = NEXTOP(PREG, oy);
      if (pt0 < HR) {
	/* variable is global */
	SREG[0] = Unsigned(pt0);
	GONext();
      }
      else {
	/* create a new Heap variable and bind our variable to it */
	CACHE_S();
	READ_IN_S();
	Bind_Local(pt0, Unsigned(S_SREG));
	RESET_VARIABLE(S_SREG);
	ENDCACHE_S();
	GONext();
      }
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(unify_void, o);
      START_PREFETCH(o);
      PREG = NEXTOP(PREG, o);
      SREG++;
      GONext();
      END_PREFETCH();
      ENDOp();

      OpW(unify_void_write, o);
      CACHE_S();
      READ_IN_S();
      PREG = NEXTOP(PREG, o);
      RESET_VARIABLE(S_SREG);
      WRITEBACK_S(S_SREG+1);
      ENDCACHE_S();
      GONextW();
      ENDOpW();

      Op(unify_l_void, o);
      PREG = NEXTOP(PREG, o);
      GONext();
      ENDOp();

      Op(unify_l_void_write, o);
      PREG = NEXTOP(PREG, o);
      RESET_VARIABLE(SREG);
      GONext();
      ENDOp();

      Op(unify_n_voids, os);
      SREG += PREG->y_u.os.s;
      PREG = NEXTOP(PREG, os);
      GONext();
      ENDOp();

      OpW(unify_n_voids_write, os);
      BEGD(d0);
      CACHE_S();
      d0 = PREG->y_u.os.s;
      READ_IN_S();
      PREG = NEXTOP(PREG, os);
      for (; d0 > 0; d0--) {
	RESET_VARIABLE(S_SREG);
	S_SREG++;
      }
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      ENDD(d0);
      GONextW();
      ENDOpW();

      Op(unify_l_n_voids, os);
      PREG = NEXTOP(PREG, os);
      GONext();
      ENDOp();

      Op(unify_l_n_voids_write, os);
      BEGD(d0);
      d0 = PREG->y_u.os.s;
      PREG = NEXTOP(PREG, os);
      CACHE_S();
      READ_IN_S();
      for (; d0 > 0; d0--) {
	RESET_VARIABLE(S_SREG);
	S_SREG++;
      }
      ENDCACHE_S();
      ENDD(d0);
      GONext();
      ENDOp();

      Op(unify_atom, oc);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG++;
      d0 = *pt0;
      deref_head(d0, uatom_unk);
    uatom_nonvar:
      if (d0 != PREG->y_u.oc.c) {
	FAIL();
      }
      PREG = NEXTOP(PREG, oc);
      GONext();

      derefa_body(d0, pt0, uatom_unk, uatom_nonvar);
      d0 = PREG->y_u.oc.c;
      PREG = NEXTOP(PREG, oc);
      Bind_Global(pt0, d0);
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      OpW(unify_atom_write, oc);
      * SREG++ = PREG->y_u.oc.c;
      PREG = NEXTOP(PREG, oc);
      GONextW();
      ENDOpW();

      Op(unify_l_atom, oc);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *SREG;
      deref_head(d0, ulatom_unk);
    ulatom_nonvar:
      if (d0 != PREG->y_u.oc.c) {
	FAIL();
      }
      PREG = NEXTOP(PREG, oc);
      GONext();

      derefa_body(d0, pt0, ulatom_unk, ulatom_nonvar);
      d0 = PREG->y_u.oc.c;
      PREG = NEXTOP(PREG, oc);
      Bind_Global(pt0, d0);
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(unify_l_atom_write, oc);
      SREG[0] = PREG->y_u.oc.c;
      PREG = NEXTOP(PREG, oc);
      GONext();
      ENDOp();

      Op(unify_n_atoms, osc);
      {
	register Int i = PREG->y_u.osc.s;             /* not enough registers */

	BEGD(d1);
	d1 = PREG->y_u.osc.c;
	for (; i > 0; i--) {
	  BEGD(d0);
	  BEGP(pt0);
	  pt0 = SREG++;
	  d0 = *pt0;
	  deref_head(d0, uatom_n_var);
	uatom_n_nonvar:
	  if (d0 != d1) {
	    FAIL();
	  }
	  continue;

	  derefa_body(d0, pt0, uatom_n_var, uatom_n_nonvar);
	  Bind_Global(pt0, d1);
	  continue;
	  ENDP(pt0);
	  ENDD(d0);
	}
	ENDD(d1);
      }
      PREG = NEXTOP(PREG, osc);
      GONext();
      ENDOp();

      OpW(unify_n_atoms_write, osc);
      BEGD(d0);
      BEGD(d1);
      d0 = PREG->y_u.osc.s;
      d1 = PREG->y_u.osc.c;
      /* write N atoms */
      CACHE_S();
      READ_IN_S();
      PREG = NEXTOP(PREG, osc);
      for (; d0 > 0; d0--)
	*S_SREG++ = d1;
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      ENDD(d1);
      ENDD(d0);
      GONextW();
      ENDOpW();

      Op(unify_float, od);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG++;
      d0 = *pt0;
      deref_head(d0, ufloat_unk);
    ufloat_nonvar:
      if (!IsApplTerm(d0)) {
	FAIL();
      }
      /* look inside term */
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d0);
      d0 = *pt0;
      if (d0 != (CELL)FunctorDouble) {
	FAIL();
      }
      ENDD(d0);
      BEGP(pt1);
      pt1 = PREG->y_u.od.d;
      PREG = NEXTOP(PREG, od);
      if (
	  pt1[1] != pt0[1]
#if SIZEOF_DOUBLE == 2*SIZEOF_INT_P
	  || pt1[2] != pt0[2]
#endif
	  ) FAIL();
      ENDP(pt1);
      ENDP(pt0);
      GONext();

      derefa_body(d0, pt0, ufloat_unk, ufloat_nonvar);
      BEGD(d1);
      d1 = AbsAppl(PREG->y_u.od.d);
      PREG = NEXTOP(PREG, od);
      Bind_Global(pt0, d1);
      GONext();
      ENDD(d1);
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      OpW(unify_float_write, od);
      * SREG++ = AbsAppl(PREG->y_u.od.d);
      PREG = NEXTOP(PREG, od);
      GONextW();
      ENDOpW();

      Op(unify_l_float, od);
      BEGD(d0);
      CACHE_S();
      READ_IN_S();
      d0 = *S_SREG;
      deref_head(d0, ulfloat_unk);
    ulfloat_nonvar:
      if (!IsApplTerm(d0)) {
	FAIL();
      }
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d0);
      d0 = *pt0;
      if (d0 != (CELL)FunctorDouble) {
	FAIL();
      }
      ENDD(d0);
      BEGP(pt1);
      pt1 = PREG->y_u.od.d;
      PREG = NEXTOP(PREG, od);
      if (
	  pt1[1] != pt0[1]
#if SIZEOF_DOUBLE == 2*SIZEOF_INT_P
	  || pt1[2] != pt0[2]
#endif
	  ) FAIL();
      ENDP(pt1);
      ENDP(pt0);
      GONext();

      derefa_body(d0, S_SREG, ulfloat_unk, ulfloat_nonvar);
      BEGD(d1);
      d1 = AbsAppl(PREG->y_u.od.d);
      PREG = NEXTOP(PREG, od);
      Bind_Global(S_SREG, d1);
      GONext();
      ENDD(d1);
      ENDCACHE_S();
      ENDD(d0);
      ENDOp();

      Op(unify_l_float_write, od);
      SREG[0] = AbsAppl(PREG->y_u.od.d);
      PREG = NEXTOP(PREG, od);
      GONext();
      ENDOp();

      Op(unify_string, ou);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG++;
      d0 = *pt0;
      deref_head(d0, ustring_unk);
    ustring_nonvar:
      if (!IsApplTerm(d0)) {
	FAIL();
      }
      /* look inside term */
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d0);
      d0 = *pt0;
      if (d0 != (CELL)FunctorString) {
	FAIL();
      }
      ENDD(d0);
      BEGP(pt1);
      pt1 = RepAppl(PREG->y_u.ou.ut);
      PREG = NEXTOP(PREG, ou);
      if (
	  pt1[1] != pt0[1]
	  || strcmp( (const char *)(pt1 + 2), (const char *)(pt0+2) )
	  ) FAIL();
      ENDP(pt1);
      ENDP(pt0);
      GONext();

      derefa_body(d0, pt0, ustring_unk, ustring_nonvar);
      BEGD(d1);
      d1 = PREG->y_u.ou.ut;
      PREG = NEXTOP(PREG, ou);
      Bind_Global(pt0, d1);
      GONext();
      ENDD(d1);
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(unify_l_string, ou);
      BEGD(d0);
      CACHE_S();
      READ_IN_S();
      d0 = *S_SREG;
      deref_head(d0, ulstring_unk);
    ulstring_nonvar:
      if (!IsApplTerm(d0)) {
	FAIL();
      }
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d0);
      d0 = *pt0;
      if (d0 != (CELL)FunctorString) {
	FAIL();
      }
      ENDD(d0);
      BEGP(pt1);
      pt1 = RepAppl(PREG->y_u.ou.ut);
      PREG = NEXTOP(PREG, ou);
      if (
	  pt1[1] != pt0[1]
	  || strcmp( (const char *)(pt1 + 2), (const char *)(pt0+2) )
	  ) FAIL();
      ENDP(pt1);
      ENDP(pt0);
      GONext();

      derefa_body(d0, S_SREG, ulstring_unk, ulstring_nonvar);
      BEGD(d1);
      d1 = PREG->y_u.ou.ut;
      PREG = NEXTOP(PREG, ou);
      Bind_Global(S_SREG, d1);
      GONext();
      ENDD(d1);
      ENDCACHE_S();
      ENDD(d0);
      ENDOp();

      Op(unify_longint, oi);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG++;
      d0 = *pt0;
      deref_head(d0, ulongint_unk);
    ulongint_nonvar:
      /* look inside term */
      if (!IsApplTerm(d0)) {
	FAIL();
      }
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d0);
      d0 = *pt0;
      if (d0 != (CELL)FunctorLongInt) {
	FAIL();
      }
      ENDD(d0);
      BEGP(pt1);
      pt1 = PREG->y_u.oi.i;
      PREG = NEXTOP(PREG, oi);
      if (pt1[1] != pt0[1]) FAIL();
      ENDP(pt1);
      ENDP(pt0);
      GONext();

      derefa_body(d0, pt0, ulongint_unk, ulongint_nonvar);
      BEGD(d1);
      d1 = AbsAppl(PREG->y_u.oi.i);
      PREG = NEXTOP(PREG, oi);
      Bind_Global(pt0, d1);
      GONext();
      ENDD(d1);
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      OpW(unify_longint_write, oi);
      * SREG++ = AbsAppl(PREG->y_u.oi.i);
      PREG = NEXTOP(PREG, oi);
      GONextW();
      ENDOpW();

      Op(unify_l_longint, oi);
      BEGD(d0);
      CACHE_S();
      READ_IN_S();
      d0 = *S_SREG;
      deref_head(d0, ullongint_unk);
    ullongint_nonvar:
      if (!IsApplTerm(d0)) {
	FAIL();
      }
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d0);
      d0 = *pt0;
      if (d0 != (CELL)FunctorLongInt) {
	FAIL();
      }
      ENDD(d0);
      BEGP(pt1);
      pt1 = PREG->y_u.oi.i;
      PREG = NEXTOP(PREG, oi);
      if (pt1[1] != pt0[1]) FAIL();
      ENDP(pt1);
      ENDP(pt0);
      GONext();

      derefa_body(d0, S_SREG, ullongint_unk, ullongint_nonvar);
      BEGD(d1);
      d1 = AbsAppl(PREG->y_u.oi.i);
      PREG = NEXTOP(PREG, oi);
      Bind_Global(S_SREG, d1);
      GONext();
      ENDD(d1);
      ENDCACHE_S();
      ENDD(d0);
      ENDOp();

      Op(unify_l_longint_write, oi);
      SREG[0] = AbsAppl(PREG->y_u.oi.i);
      PREG = NEXTOP(PREG, oi);
      GONext();
      ENDOp();

      Op(unify_bigint, oN);
#ifdef USE_GMP
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG++;
      d0 = *pt0;
      deref_head(d0, ubigint_unk);
    ubigint_nonvar:
      /* look inside term */
      if (!IsApplTerm(d0)) {
	FAIL();
      }
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d1);
      d1 = *pt0;
      if (d1 != (CELL)FunctorBigInt)
	{
	  FAIL();
	}
      ENDD(d1);
      if (Yap_gmp_tcmp_big_big(d0,PREG->y_u.oN.b))
	FAIL();
      PREG = NEXTOP(PREG, oN);
      ENDP(pt0);
      GONext();

      derefa_body(d0, pt0, ubigint_unk, ubigint_nonvar);
      BEGD(d1);
      d1 = PREG->y_u.oN.b;
      PREG = NEXTOP(PREG, oN);
      Bind_Global(pt0, d1);
      GONext();
      ENDD(d1);
      ENDP(pt0);
      ENDD(d0);
#else
      FAIL();
#endif
      ENDOp();

      Op(unify_l_bigint, oN);
#ifdef USE_GMP
      BEGD(d0);
      CACHE_S();
      READ_IN_S();
      d0 = *S_SREG;
      deref_head(d0, ulbigint_unk);
    ulbigint_nonvar:
      if (!IsApplTerm(d0)) {
	FAIL();
      }
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d0);
      d0 = *pt0;
      if (d0 != (CELL)FunctorBigInt)
	{
	  FAIL();
	}
      ENDD(d0);
      if (Yap_gmp_tcmp_big_big(d0,PREG->y_u.oN.b))
	FAIL();
      PREG = NEXTOP(PREG, oN);
      ENDP(pt0);
      GONext();

      derefa_body(d0, S_SREG, ulbigint_unk, ulbigint_nonvar);
      BEGD(d1);
      d1 = PREG->y_u.oN.b;
      PREG = NEXTOP(PREG, oN);
      Bind_Global(S_SREG, d1);
      GONext();
      ENDD(d1);
      ENDCACHE_S();
      ENDD(d0);
#else
      FAIL();
#endif
      ENDOp();

      Op(unify_dbterm, oD);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG++;
      d0 = *pt0;
      deref_head(d0, udbterm_unk);
    udbterm_nonvar:
      BEGD(d1);
      /* we have met a preexisting dbterm */
      d1 = PREG->y_u.oD.D;
      PREG = NEXTOP(PREG, oD);
      UnifyBound(d0,d1);
      ENDD(d1);

      derefa_body(d0, pt0, udbterm_unk, udbterm_nonvar);
      BEGD(d1);
      d1 = PREG->y_u.oD.D;
      PREG = NEXTOP(PREG, oD);
      Bind_Global(pt0, d1);
      GONext();
      ENDD(d1);
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(unify_l_dbterm, oD);
      BEGD(d0);
      CACHE_S();
      READ_IN_S();
      d0 = *S_SREG;
      deref_head(d0, uldbterm_unk);
    uldbterm_nonvar:
      BEGD(d1);
      /* we have met a preexisting dbterm */
      d1 = PREG->y_u.oD.D;
      PREG = NEXTOP(PREG, oD);
      UnifyBound(d0,d1);
      ENDD(d1);

      derefa_body(d0, S_SREG, uldbterm_unk, uldbterm_nonvar);
      BEGD(d1);
      d1 = PREG->y_u.oD.D;
      PREG = NEXTOP(PREG, oD);
      Bind_Global(S_SREG, d1);
      GONext();
      ENDD(d1);
      ENDCACHE_S();
      ENDD(d0);
      ENDOp();

      OpRW(unify_list, o);
      *--SP = Unsigned(SREG + 1);
      *--SP = READ_MODE;
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, ulist_unk);
    ulist_nonvar:
      if (!IsPairTerm(d0)) {
	FAIL();
      }
      /* we continue in read mode */
      START_PREFETCH(o);
      SREG = RepPair(d0);
      PREG = NEXTOP(PREG, o);
      GONext();
      END_PREFETCH();

      derefa_body(d0, pt0, ulist_unk, ulist_nonvar);
      /* we enter write mode */
      START_PREFETCH_W(o);
      CACHE_S();
      READ_IN_S();
      S_SREG = HR;
      PREG = NEXTOP(PREG, o);
      HR = S_SREG + 2;
      d0 = AbsPair(S_SREG);
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      Bind_Global(pt0, d0);
      GONextW();
      END_PREFETCH_W();

      ENDP(pt0);

      ENDD(d0);
      ENDOpRW();

      OpW(unify_list_write, o);
      PREG = NEXTOP(PREG, o);
      BEGD(d0);
      d0 = AbsPair(HR);
      CACHE_S();
      READ_IN_S();
      SP -= 2;
      SP[0] = WRITE_MODE;
      SP[1] = Unsigned(S_SREG + 1);
      S_SREG[0] = d0;
      S_SREG = HR;
      HR = S_SREG + 2;
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      GONextW();
      ENDD(d0);
      ENDOpW();

      OpRW(unify_l_list, o);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, ullist_unk);
    ullist_nonvar:
      START_PREFETCH(o);
      if (!IsPairTerm(d0)) {
	FAIL();
      }
      /* we continue in read mode */
      PREG = NEXTOP(PREG, o);
      SREG = RepPair(d0);
      GONext();
      END_PREFETCH();

      derefa_body(d0, pt0, ullist_unk, ullist_nonvar);
      /* we enter write mode */
      START_PREFETCH_W(o);
      PREG = NEXTOP(PREG, o);
      CACHE_S();
      READ_IN_S();
      S_SREG = HR;
      HR = S_SREG + 2;
      d0 = AbsPair(S_SREG);
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      Bind_Global(pt0, d0);
      GONextW();
      END_PREFETCH_W();

      ENDP(pt0);
      ENDD(d0);
      ENDOpRW();

      OpW(unify_l_list_write, o);
      /* we continue in write mode */
      BEGD(d0);
      d0 = AbsPair(HR);
      PREG = NEXTOP(PREG, o);
      CACHE_S();
      READ_IN_S();
      S_SREG[0] = d0;
      S_SREG = HR;
      HR = S_SREG + 2;
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      GONextW();
      ENDD(d0);
      ENDOpW();

      OpRW(unify_struct, ofa);
      *--SP = Unsigned(SREG + 1);
      *--SP = READ_MODE;
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      START_PREFETCH(ofa);
      deref_head(d0, ustruct_unk);
    ustruct_nonvar:
      /* we are in read mode */
      if (!IsApplTerm(d0)) {
	FAIL();
      }
      CACHE_S();
      READ_IN_S();
      /* we continue in read mode */
      S_SREG = RepAppl(d0);
      /* just check functor */
      d0 = (CELL) (PREG->y_u.ofa.f);
      if (*S_SREG != d0) {
	FAIL();
      }
      PREG = NEXTOP(PREG, ofa);
      WRITEBACK_S(S_SREG+1);
      ENDCACHE_S();
      GONext();
      END_PREFETCH();

      derefa_body(d0, pt0, ustruct_unk, ustruct_nonvar);
      /* Enter Write mode */
      START_PREFETCH_W(ofa);
      /* set d1 to be the new structure we are going to create */
      BEGD(d1);
      d1 = AbsAppl(HR);
      /* we know the variable must be in the heap */
      Bind_Global(pt0, d1);
      /* now, set pt0 to point to the heap where we are going to
       * build our term */
      pt0 = HR;
      ENDD(d1);
      /* first, put the functor */
      d0 = (CELL) (PREG->y_u.ofa.f);
      *pt0++ = d0;
      HR = pt0 + PREG->y_u.ofa.a;
      PREG = NEXTOP(PREG, ofa);
      /* set SREG */
      SREG = pt0;
      /* update H */
      GONextW();
      END_PREFETCH_W();

      ENDP(pt0);

      ENDD(d0);
      ENDOpRW();

      OpW(unify_struct_write, ofa);
      CACHE_S();
      READ_IN_S();
      *--SP = Unsigned(S_SREG + 1);
      *--SP = WRITE_MODE;
      /* we continue in write mode */
      BEGD(d0);
      d0 = AbsAppl(HR);
      S_SREG[0] = d0;
      S_SREG = HR;
      d0 = (CELL) (PREG->y_u.ofa.f);
      *S_SREG++ = d0;
      HR = S_SREG + PREG->y_u.ofa.a;
      PREG = NEXTOP(PREG, ofa);
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      ENDD(d0);
      GONextW();
      ENDOpW();

      OpRW(unify_l_struc, ofa);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, ulstruct_unk);
    ulstruct_nonvar:
      /* we are in read mode */
      START_PREFETCH(ofa);
      if (!IsApplTerm(d0)) {
	FAIL();
      }
      /* we continue in read mode */
      SREG = RepAppl(d0);
      /* just check functor */
      d0 = (CELL) (PREG->y_u.ofa.f);
      if (*SREG++ != d0) {
	FAIL();
      }
      PREG = NEXTOP(PREG, ofa);
      GONext();
      END_PREFETCH();

      derefa_body(d0, pt0, ulstruct_unk, ulstruct_nonvar);
      /* Enter Write mode */
      /* set d1 to be the new structure we are going to create */
      START_PREFETCH_W(ofa);
      BEGD(d1);
      d1 = AbsAppl(HR);
      /* we know the variable must be in the heap */
      Bind_Global(pt0, d1);
      /* now, set pt0 to point to the heap where we are going to
       * build our term */
      pt0 = HR;
      ENDD(d1);
      /* first, put the functor */
      d0 = (CELL) (PREG->y_u.ofa.f);
      *pt0++ = d0;
      HR = pt0 + PREG->y_u.ofa.a;
      PREG = NEXTOP(PREG, ofa);
      /* set SREG */
      SREG = pt0;
      /* update H */
      GONextW();
      END_PREFETCH_W();
      ENDP(pt0);

      ENDD(d0);
      ENDOpRW();

      OpW(unify_l_struc_write, ofa);
      BEGD(d0);
      d0 = AbsAppl(HR);
      CACHE_S();
      READ_IN_S();
      S_SREG[0] = d0;
      S_SREG = HR;
      d0 = (CELL) (PREG->y_u.ofa.f);
      *S_SREG++ = d0;
      HR = S_SREG + PREG->y_u.ofa.a;
      PREG = NEXTOP(PREG, ofa);
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      ENDD(d0);
      GONextW();
      ENDOpW();


      /************************************************************************\
       * Put Instructions                                                      *
\************************************************************************/

      Op(put_x_var, xx);
      BEGP(pt0);
      pt0 = HR;
      XREG(PREG->y_u.xx.xl) = Unsigned(pt0);
      HR = pt0 + 1;
      XREG(PREG->y_u.xx.xr) = Unsigned(pt0);
      PREG = NEXTOP(PREG, xx);
      RESET_VARIABLE(pt0);
      ENDP(pt0);
      GONext();
      ENDOp();

      Op(put_y_var, yx);
      BEGP(pt0);
      pt0 = YREG + PREG->y_u.yx.y;
      XREG(PREG->y_u.yx.x) = (CELL) pt0;
      PREG = NEXTOP(PREG, yx);
#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
      /* We must initialize a shared variable to point to the SBA */
      if (Unsigned((Int)(pt0)-(Int)(H_FZ)) >
	  Unsigned((Int)(B_FZ)-(Int)(H_FZ))) {
	*pt0 =  (CELL)STACK_TO_SBA(pt0);
      } else
#endif /* YAPOR_SBA && FROZEN_STACKS */
	INITIALIZE_PERMVAR(pt0, (CELL)pt0);
      ENDP(pt0);
      GONext();
      ENDOp();

      Op(put_x_val, xx);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xx.xl);
      XREG(PREG->y_u.xx.xr) = d0;
      ENDD(d0);
      PREG = NEXTOP(PREG, xx);
      GONext();
      ENDOp();

      Op(put_xx_val, xxxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->y_u.xxxx.xl1);
      d1 = XREG(PREG->y_u.xxxx.xl2);
      XREG(PREG->y_u.xxxx.xr1) = d0;
      XREG(PREG->y_u.xxxx.xr2) = d1;
      ENDD(d1);
      ENDD(d0);
      PREG = NEXTOP(PREG, xxxx);
      GONext();
      ENDOp();

      Op(put_y_val, yx);
      BEGD(d0);
      d0 = YREG[PREG->y_u.yx.y];
#ifdef YAPOR_SBA
      if (d0 == 0) /* new variable */
	XREG(PREG->y_u.yx.x) = (CELL)(YREG+PREG->y_u.yx.y);
      else
#endif
	XREG(PREG->y_u.yx.x) = d0;
      ENDD(d0);
      PREG = NEXTOP(PREG, yx);
      GONext();
      ENDOp();

      Op(put_y_vals, yyxx);
      ALWAYS_START_PREFETCH(yyxx);
      BEGD(d0);
      d0 = YREG[PREG->y_u.yyxx.y1];
#ifdef YAPOR_SBA
      if (d0 == 0) /* new variable */
	XREG(PREG->y_u.yyxx.x1) = (CELL)(YREG+PREG->y_u.yyxx.y1);
      else
#endif
	XREG(PREG->y_u.yyxx.x1) = d0;
      ENDD(d0);
      /* allow for some prefetching */
      PREG = NEXTOP(PREG, yyxx);
      BEGD(d1);
      d1 = YREG[PREVOP(PREG,yyxx)->y_u.yyxx.y2];
#ifdef YAPOR_SBA
      if (d1 == 0) /* new variable */
	XREG(PREVOP(PREG->y_u.yyxx,yyxx).x2) = (CELL)(YREG+PREG->y_u.yyxx.y2);
      else
#endif
	XREG(PREVOP(PREG,yyxx)->y_u.yyxx.x2) = d1;
      ENDD(d1);
      ALWAYS_GONext();
      ALWAYS_END_PREFETCH();
      ENDOp();

      Op(put_unsafe, yx);
      BEGD(d0);
      BEGP(pt0);
      pt0 = YREG+PREG->y_u.yx.y;
      d0 = *pt0;
      deref_head(d0, punsafe_unk);
    punsafe_nonvar:
      XREG(PREG->y_u.yx.x) = d0;
      PREG = NEXTOP(PREG, yx);
      GONext();


      derefa_body(d0, pt0, punsafe_unk, punsafe_nonvar);
      /* d0 is a variable, check whether we need to globalise it */
      if (pt0 <= HR || pt0 >= YREG) {
	/* variable is safe */
	XREG(PREG->y_u.yx.x) = Unsigned(pt0);
	PREG = NEXTOP(PREG, yx);
	GONext();
      }
      else {
	/* create a new Heap variable and bind our variable to it */
	Bind_Local(pt0, Unsigned(HR));
	XREG(PREG->y_u.yx.x) = (CELL) HR;
	RESET_VARIABLE(HR);
	HR++;
	PREG = NEXTOP(PREG, yx);
	GONext();
      }
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(put_atom, xc);
      BEGD(d0);
      d0 = PREG->y_u.xc.c;
      XREG(PREG->y_u.xc.x) = d0;
      PREG = NEXTOP(PREG, xc);
      GONext();
      ENDD(d0);
      ENDOp();

      Op(put_dbterm, xD);
      BEGD(d0);
      d0 = PREG->y_u.xD.D;
      XREG(PREG->y_u.xD.x) = d0;
      PREG = NEXTOP(PREG, xD);
      GONext();
      ENDD(d0);
      ENDOp();

      Op(put_bigint, xN);
      BEGD(d0);
      d0 = PREG->y_u.xN.b;
      XREG(PREG->y_u.xN.x) = d0;
      PREG = NEXTOP(PREG, xN);
      GONext();
      ENDD(d0);
      ENDOp();

      Op(put_float, xd);
      BEGD(d0);
      d0 = AbsAppl(PREG->y_u.xd.d);
      XREG(PREG->y_u.xd.x) = d0;
      PREG = NEXTOP(PREG, xd);
      GONext();
      ENDD(d0);
      ENDOp();

      Op(put_longint, xi);
      BEGD(d0);
      d0 = AbsAppl(PREG->y_u.xi.i);
      XREG(PREG->y_u.xi.x) = d0;
      PREG = NEXTOP(PREG, xi);
      GONext();
      ENDD(d0);
      ENDOp();

      Op(put_list, x);
      CACHE_S();
      READ_IN_S();
      S_SREG = HR;
      HR += 2;
      BEGD(d0);
      d0 = AbsPair(S_SREG);
      XREG(PREG->y_u.x.x) = d0;
      PREG = NEXTOP(PREG, x);
      ENDD(d0);
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      GONext();
      ENDOp();

      Op(put_struct, xfa);
      BEGD(d0);
      d0 = AbsAppl(HR);
      XREG(PREG->y_u.xfa.x) = d0;
      d0 = (CELL) (PREG->y_u.xfa.f);
      *HR++ = d0;
      SREG = HR;
      HR += PREG->y_u.xfa.a;
      ENDD(d0);
      PREG = NEXTOP(PREG, xfa);
      GONext();
      ENDOp();

      /************************************************************************\
       *      Write Instructions                                              *
\************************************************************************/

      Op(write_x_var, x);
      XREG(PREG->y_u.x.x) = Unsigned(SREG);
      PREG = NEXTOP(PREG, x);
      RESET_VARIABLE(SREG);
      SREG++;
      GONext();
      ENDOp();

      Op(write_void, e);
      PREG = NEXTOP(PREG, e);
      RESET_VARIABLE(SREG);
      SREG++;
      GONext();
      ENDOp();

      Op(write_n_voids, s);
      BEGD(d0);
      d0 = PREG->y_u.s.s;
      PREG = NEXTOP(PREG, s);
      for (; d0 > 0; d0--) {
	RESET_VARIABLE(SREG);
	SREG++;
      }
      ENDD(d0);
      GONext();
      ENDOp();

      Op(write_y_var, y);
      INITIALIZE_PERMVAR(YREG+PREG->y_u.y.y,Unsigned(SREG));
      PREG = NEXTOP(PREG, y);
      RESET_VARIABLE(SREG);
      SREG++;
      GONext();
      ENDOp();

      Op(write_x_val, x);
      BEGD(d0);
      d0 = XREG(PREG->y_u.x.x);
      *SREG++ = d0;
      ENDD(d0);
      PREG = NEXTOP(PREG, x);
      GONext();
      ENDOp();

      Op(write_x_loc, x);
      BEGD(d0);
      d0 = XREG(PREG->y_u.x.x);
      PREG = NEXTOP(PREG, x);
      deref_head(d0, w_x_unk);
    w_x_bound:
      *SREG++ = d0;
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, w_x_unk, w_x_bound);
#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
      if (pt0 > HR && pt0<(CELL *)B_FZ) {
#else
	if (pt0 > HR) {
#endif /* YAPOR_SBA && FROZEN_STACKS */
	  /* local variable: let us bind it to the list */
#ifdef FROZEN_STACKS  /* TRAIL */
	  Bind_Local(pt0, Unsigned(SREG));
#else
	  TRAIL_LOCAL(pt0, Unsigned(SREG));
	  *pt0 = Unsigned(SREG);
#endif /* FROZEN_STACKS */
	  RESET_VARIABLE(SREG);
	  SREG++;
	  GONext();
	}
	else {
	  *SREG++ = Unsigned(pt0);
	  GONext();
	}
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(write_y_val, y);
	BEGD(d0);
	d0 = YREG[PREG->y_u.y.y];
#ifdef YAPOR_SBA
	if (d0 == 0) /* new variable */
	  *SREG++ = (CELL)(YREG+PREG->y_u.y.y);
	else
#endif
	  *SREG++ = d0;
	ENDD(d0);
	PREG = NEXTOP(PREG, y);
	GONext();
	ENDOp();

	Op(write_y_loc, y);
	BEGD(d0);
	BEGP(pt0);
	pt0 = YREG+PREG->y_u.y.y;
	d0 = *pt0;
	deref_head(d0, w_y_unk);
      w_y_bound:
	PREG = NEXTOP(PREG, y);
	*SREG++ = d0;
	GONext();

	derefa_body(d0, pt0, w_y_unk, w_y_bound);
	if (pt0 > HR
#if defined(YAPOR_SBA) && defined(FROZEN_STACKS)
	    && pt0<(CELL *)B_FZ
#endif /* YAPOR_SBA && FROZEN_STACKS */
	    ) {
	  PREG = NEXTOP(PREG, y);
	  /* local variable: let us bind it to the list */
#ifdef FROZEN_STACKS
	  Bind_Local(pt0, Unsigned(SREG));
#else
	  *pt0 = Unsigned(SREG);
	  TRAIL_LOCAL(pt0, Unsigned(SREG));
#endif /* FROZEN_STACKS */
	  RESET_VARIABLE(SREG);
	  SREG++;
	  GONext();
	} else {
	  PREG = NEXTOP(PREG, y);
	  *SREG++ = Unsigned(pt0);
	  GONext();
	}
	ENDP(pt0);
	ENDD(d0);
	ENDOp();

	Op(write_atom, c);
	BEGD(d0);
	d0 = PREG->y_u.c.c;
	*SREG++ = d0;
	ENDD(d0);
	PREG = NEXTOP(PREG, c);
	GONext();
	ENDOp();

	Op(write_bigint, N);
	BEGD(d0);
	d0 = PREG->y_u.N.b;
	*SREG++ = d0;
	ENDD(d0);
	PREG = NEXTOP(PREG, N);
	GONext();
	ENDOp();

	Op(write_dbterm, D);
	BEGD(d0);
	d0 = PREG->y_u.D.D;
	*SREG++ = d0;
	ENDD(d0);
	PREG = NEXTOP(PREG, D);
	GONext();
	ENDOp();

	Op(write_float, d);
	BEGD(d0);
	d0 = AbsAppl(PREG->y_u.d.d);
	*SREG++ = d0;
	ENDD(d0);
	PREG = NEXTOP(PREG, d);
	GONext();
	ENDOp();

	Op(write_longint, i);
	BEGD(d0);
	d0 = AbsAppl(PREG->y_u.i.i);
	*SREG++ = d0;
	ENDD(d0);
	PREG = NEXTOP(PREG, i);
	GONext();
	ENDOp();

	Op(write_n_atoms, sc);
	BEGD(d0);
	BEGD(d1);
	d0 = PREG->y_u.sc.s;
	d1 = PREG->y_u.sc.c;
	for (; d0 > 0; d0--)
	  *SREG++ = d1;
	ENDD(d1);
	ENDD(d0);
	PREG = NEXTOP(PREG, sc);
	GONext();
	ENDOp();

	Op(write_list, e);
	BEGD(d0);
	d0 = AbsPair(HR);
	*SREG++ = d0;
	/* I will not actually store the mode in the stack */
	SP[-1] = Unsigned(SREG);
	SP[-2] = 1;           /* Put instructions follow the main stream */
	SP -= 2;
	SREG = HR;
	HR += 2;
	ENDD(d0);
	PREG = NEXTOP(PREG, e);
	GONext();
	ENDOp();

	Op(write_l_list, e);
	ALWAYS_START_PREFETCH(e);
	PREG = NEXTOP(PREG, e);
	BEGD(d0);
	CACHE_S();
	READ_IN_S();
	d0 = AbsPair(HR);
	*S_SREG = d0;
	WRITEBACK_S(HR);
	HR += 2;
	ENDCACHE_S();
	ENDD(d0);
	ALWAYS_GONext();
	ALWAYS_END_PREFETCH();
	ENDOp();

	Op(write_struct, fa);
	BEGD(d0);
	d0 = AbsAppl(HR);
	*SREG++ = d0;
	SP[-1] = Unsigned(SREG);
	SP[-2] = 1;           /* Put instructions follow the main stream */
	SP -= 2;
	d0 = (CELL) (PREG->y_u.fa.f);
	*HR++ = d0;
	ENDD(d0);
	BEGD(d0);
	d0 = PREG->y_u.fa.a;
	PREG = NEXTOP(PREG, fa);
	SREG = HR;
	HR += d0;
	ENDD(d0);
	GONext();
	ENDOp();

	Op(write_l_struc, fa);
	BEGD(d0);
	d0 = AbsAppl(HR);
	*SREG = d0;
	d0 = (CELL) (PREG->y_u.fa.f);
	*HR++ = d0;
	SREG = HR;
	ENDD(d0);
	BEGD(d0);
	d0 = PREG->y_u.fa.a;
	PREG = NEXTOP(PREG, fa);
	HR += d0;
	ENDD(d0);
	GONext();
	ENDOp();

	/************************************************************************\
	 *   Save last unified struct or list                                 *
\************************************************************************/

	/* vitor: I think I should kill these two instructions, by expanding the
	 * othe instructions.
	 */

	Op(save_pair_x, ox);
	XREG(PREG->y_u.ox.x) = AbsPair(SREG);
	PREG = NEXTOP(PREG, ox);
	GONext();
	ENDOp();

	OpW(save_pair_x_write, ox);
	XREG(PREG->y_u.ox.x) = AbsPair(SREG);
	PREG = NEXTOP(PREG, ox);
	GONextW();
	ENDOpW();

	Op(save_pair_y, oy);
	INITIALIZE_PERMVAR(YREG+PREG->y_u.oy.y,AbsPair(SREG));
	PREG = NEXTOP(PREG, oy);
	GONext();
	ENDOp();

	OpW(save_pair_y_write, oy);
	INITIALIZE_PERMVAR(YREG+PREG->y_u.oy.y,AbsPair(SREG));
	PREG = NEXTOP(PREG, oy);
	GONextW();
	ENDOpW();

	Op(save_appl_x, ox);
	XREG(PREG->y_u.ox.x) = AbsAppl(SREG - 1);
	PREG = NEXTOP(PREG, ox);
	GONext();
	ENDOp();

	OpW(save_appl_x_write, ox);
	XREG(PREG->y_u.ox.x) = AbsAppl(SREG - 1);
	PREG = NEXTOP(PREG, ox);
	GONextW();
	ENDOpW();

	Op(save_appl_y, oy);
	INITIALIZE_PERMVAR(YREG+PREG->y_u.oy.y,AbsAppl(SREG-1));
	PREG = NEXTOP(PREG, oy);
	GONext();
	ENDOp();

	OpW(save_appl_y_write, oy);
	INITIALIZE_PERMVAR(YREG+PREG->y_u.oy.y,AbsAppl(SREG-1));
	PREG = NEXTOP(PREG, oy);
	GONextW();
	ENDOpW();

      
