/************************************************************************       \
 *   Instructions for implemeting 'or;'                                        *
\************************************************************************/

#ifdef INDENT_CODE
{
  {
    {
#endif /* INDENT_CODE */

      BOp(jump, l);
      PREG = PREG->y_u.l.l;
      JMPNext();
      ENDBOp();

      /* This instruction is called when the previous goal
         was interrupted when waking up goals
      */
      BOp(move_back, l);
      PREG = (yamop *)(((char *)PREG)-(Int)(NEXTOP((yamop *)NULL,Osbpp)));
      JMPNext();
      ENDBOp();

      /* This instruction is called when the previous goal
         was interrupted when waking up goals
      */
      BOp(skip, l);
      PREG = NEXTOP(PREG,l);
      JMPNext();
      ENDBOp();

      Op(either, Osblp);
#ifdef LOW_LEVEL_TRACER
      if (Yap_do_low_level_trace) {
        low_level_trace(try_or, PREG->y_u.Osblp.p0, NULL);
      }
#endif
#ifdef COROUTINING
      CACHE_Y_AS_ENV(YREG);
      check_stack(NoStackEither, HR);
      ENDCACHE_Y_AS_ENV();
    either_notest:
#endif
      BEGD(d0);
      /* Try to preserve the environment */
      d0 = PREG->y_u.Osblp.s;
      BEGCHO(pt1);
      pt1 = (choiceptr) ((char *) YREG + (yslot) d0);
#ifdef FROZEN_STACKS
      {
        choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef YAPOR_SBA
        if (pt1 > top_b || pt1 < (choiceptr)HR) pt1 = top_b;
#else
        if (pt1 > top_b) pt1 = top_b;
#endif /* YAPOR_SBA */
      }
#else
      if (pt1 > B) {
        pt1 = B;
      }
#endif /* FROZEN_STACKS */
      pt1 = (choiceptr)(((CELL *) pt1)-1);
      *(CELL **) pt1 = YREG;
      store_yaam_regs_for_either(PREG->y_u.Osblp.l, PREG);
      SREG = (CELL *) (B = pt1);
#ifdef YAPOR
      SCH_set_load(pt1);
#endif  /* YAPOR */
      SET_BB(pt1);
      ENDCHO(pt1);
      /* skip the current instruction plus the next one */
      PREG = NEXTOP(NEXTOP(PREG, Osblp),l);
      GONext();
      ENDD(d0);

#ifdef COROUTINING
    NoStackEither:
      PROCESS_INTERRUPT(interrupt_either, either_notest, PREG->y_u.Osblp.s);
#endif

      ENDOp();

      Op(or_else, Osblp);
      HR = HBREG = PROTECT_FROZEN_H(B);
      ENV = B->cp_env;
      B->cp_cp = PREG;
#ifdef DEPTH_LIMIT
      DEPTH = B->cp_depth;
#endif  /* DEPTH_LIMIT */
      SET_BB(PROTECT_FROZEN_B(B));
#ifdef YAPOR
      if (SCH_top_shared_cp(B)) {
        SCH_new_alternative(PREG, PREG->y_u.Osblp.l);
      } else
#endif  /* YAPOR */
        B->cp_ap = PREG->y_u.Osblp.l;
      PREG = NEXTOP(PREG, Osblp);
      YREG = (CELL *) B->cp_a1;
      GONext();
      ENDOp();

#ifdef YAPOR
      Op(or_last, Osblp);
#else
      Op(or_last, p);
#endif  /* YAPOR */
      BEGCHO(pt0);
      pt0 = B;
#ifdef YAPOR
      if (SCH_top_shared_cp(B)) {
        HR = HBREG = PROTECT_FROZEN_H(pt0);
        YREG = (CELL *) pt0->cp_a1;
        ENV = pt0->cp_env;
#ifdef DEPTH_LIMIT
        DEPTH = pt0->cp_depth;
#endif  /* DEPTH_LIMIT */
        SCH_new_alternative(PREG, NULL);
      }
      else
#endif  /* YAPOR */
        {
          B = pt0->cp_b;
          HR = PROTECT_FROZEN_H(pt0);
          YREG = (CELL *) pt0->cp_a1;
          ENV = pt0->cp_env;
#ifdef DEPTH_LIMIT
          DEPTH = pt0->cp_depth;
#endif  /* DEPTH_LIMIT */
          HBREG = PROTECT_FROZEN_H(B);
        }
#ifdef YAPOR
      PREG = NEXTOP(PREG, Osblp);
#else
      PREG = NEXTOP(PREG, p);
#endif  /* YAPOR */
      SET_BB(PROTECT_FROZEN_B(B));
      GONext();
      ENDCHO(pt0);
      ENDOp();

      /************************************************************************\
       *    Pop operations                                                   *
\************************************************************************/

      OpRW(pop_n, s);
      /* write mode might have been called from read mode */
      BEGD(d0);
      d0 = PREG->y_u.os.s;
      SP = (CELL *) (((char *) SP) + d0);
      ENDD(d0);
      BEGD(d0);
      d0 = SP[0];
      if (d0) {
        START_PREFETCH(s);
        SREG = (CELL *) (SP[1]);
        SP += 2;
        PREG = NEXTOP(PREG, s);
        GONext();
        END_PREFETCH();
      }
      else {
        START_PREFETCH_W(s);
        SREG = (CELL *) (SP[1]);
        SP += 2;
        PREG = NEXTOP(PREG, s);
        GONextW();
	END_PREFETCH_W();
      }
      ENDD(d0);
      ENDOpRW();

      OpRW(pop, e);
      BEGD(d0);
      d0 = SP[0];
      SREG = (CELL *) (SP[1]);
      SP += 2;
      if (d0) {
	START_PREFETCH(e);
	PREG = NEXTOP(PREG, e);
	GONext();
	END_PREFETCH();
      }
      else {
	START_PREFETCH_W(e);
	PREG = NEXTOP(PREG, e);
	GONextW();
	END_PREFETCH_W();
      }
      ENDD(d0);
      ENDOpRW();

