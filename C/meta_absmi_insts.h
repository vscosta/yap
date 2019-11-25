
#ifdef INDENT_CODE
{
  {
    {
#endif /* INDENT_CODE */

      /* join all the meta-call code into a single procedure with three entry points */
      {
	CACHE_Y_AS_ENV(YREG);
	BEGD(d0);       /* term to be meta-called */
	Term mod;       /* module to be used */
	PredEntry *pen;  /* predicate */
	choiceptr b_ptr;        /* cut point */
	Functor f;

	/* we are doing the rhs of a , */
	BOp(p_execute_tail, Osbmp);

	FETCH_Y_FROM_ENV(YREG);
	/* place to cut to */
	b_ptr = (choiceptr)ENV_YREG[E_CB];
	/* original goal */
	d0 = ENV_YREG[-EnvSizeInCells-1];
	/* predicate we had used */
	pen = RepPredProp(AddressOfTerm(ENV_YREG[-EnvSizeInCells-2]));
	/* current module at the time */
	mod = ENV_YREG[-EnvSizeInCells-3];
	/* set YREG */
	/* Try to preserve the environment */
	ENV_YREG = (CELL *) (((char *) YREG) + PREG->y_u.Osbmp.s);
#ifdef FROZEN_STACKS
	{
	  choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef YAPOR_SBA
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) ENV_YREG = (CELL *) top_b;
#else
	  if (ENV_YREG > (CELL *) top_b) ENV_YREG = (CELL *) top_b;
#endif /* YAPOR_SBA */
	}
#else
	if (ENV_YREG > (CELL *) B) {
	  ENV_YREG = (CELL *) B;
	}
#endif /* FROZEN_STACKS */
	/* now, jump to actual execution */
	if (pen->ArityOfPE) {
	  f = pen->FunctorOfPred;
	  /* reuse environment if we are continuining a comma, ie, (g1,g2,g3) */
	  /* can only do it deterministically */
	  /* broken
	     if (f == FunctorComma && (CELL *)B >= ENV) {
	     ENV_YREG = ENV;
	     ENV = (CELL *)ENV[E_E];
	     }
	  */
	  goto execute_pred_f;
	} else
	  goto execute_pred_a;
	ENDBOp();

	/* fetch the module from ARG2 */
	BOp(p_execute2, Osbpp);

	mod = ARG2;
	deref_head(mod, execute2_unk0);
      execute2_nvar0:
	if (!IsAtomTerm(mod)) {
	  saveregs();
	  Yap_Error(TYPE_ERROR_ATOM, mod, "call/2");
	  setregs();
	}
	goto start_execute;

	BEGP(pt1);
	deref_body(mod, pt1, execute2_unk0, execute2_nvar0);
	saveregs();
	Yap_Error(INSTANTIATION_ERROR, mod, "call/2");
	setregs();
	ENDP(pt1);
	/* Oops, second argument was unbound too */
	FAIL();
	ENDBOp();

	BOp(p_execute, Osbmp);
	/* fetch the module from PREG */
	mod = PREG->y_u.Osbmp.mod;
      start_execute:
	/* place to cut to */
	b_ptr = B;
	/* we have mod, and ARG1 has the goal, let us roll */
	/* Try to preserve the environment */
	ENV_YREG = (CELL *) (((char *) YREG) + PREG->y_u.Osbmp.s);
#ifdef FROZEN_STACKS
	{
	  choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef YAPOR_SBA
	  if (ENV_YREG > (CELL *) top_b || ENV_YREG < HR) ENV_YREG = (CELL *) top_b;
#else
	  if (ENV_YREG > (CELL *) top_b) ENV_YREG = (CELL *) top_b;
#endif /* YAPOR_SBA */
	}
#else
	if (ENV_YREG > (CELL *) B) {
	  ENV_YREG = (CELL *) B;
	}
#endif /* FROZEN_STACKS */
	d0 = ARG1;
	// restart_pexecute:
	deref_head(d0, execute_unk);
      execute_nvar:
	if (IsApplTerm(d0)) {
	  f = FunctorOfTerm(d0);
	  if (IsExtensionFunctor(f)) {
	    goto execute_metacall;
	  }
	  pen = RepPredProp(PredPropByFunc(f, mod));
	execute_pred_f:
	  if (pen->PredFlags & (MetaPredFlag|UndefPredFlag)) {
	    /* just strip all of M:G */
	    if (f == FunctorModule) {
	      Term tmod = ArgOfTerm(1,d0);
	      /* loop on modules */
	      if (!IsVarTerm(tmod) && IsAtomTerm(tmod)) {
		d0 = ArgOfTerm(2,d0);
		mod = tmod;
		goto execute_nvar;
	      }
	      goto execute_metacall;
	    }
	    if (f == FunctorComma) {
	      Term nmod = mod;

	      /* optimise conj */
	      SREG = RepAppl(d0);
	      BEGD(d1);
	      d1 = SREG[2];
	      /* create an environment to execute the call */
	      deref_head(d1, execute_comma_unk);
	    execute_comma_nvar:
	      if (IsAtomTerm(d1)) {
		/* atomic goal is simpler */
		ENV_YREG[-EnvSizeInCells-2]  = MkIntegerTerm((Int)PredPropByAtom(AtomOfTerm(d1),nmod));
	      } else if (IsApplTerm(d1)) {
		Functor f1 = FunctorOfTerm(d1);
		if (IsExtensionFunctor(f1)) {
		  goto execute_metacall;
		} else {
		  /* check for modules when looking up */
		  if (f1 == FunctorModule) {
		    Term tmod = ArgOfTerm(1,d1);
		    /* loop on modules */
		    if (!IsVarTerm(tmod) && IsAtomTerm(tmod)) {
		      d1 = ArgOfTerm(2,d1);
		      nmod = tmod;
		      goto execute_comma_nvar;
		    }
		    goto execute_metacall;
		  }
		  ENV_YREG[-EnvSizeInCells-2]  = MkIntegerTerm((Int)PredPropByFunc(f1,nmod));
		}
	      } else {
		goto execute_metacall;
	      }
	      ENV_YREG[-EnvSizeInCells-3]  = mod;
	      /* now, we can create the new environment for the meta-call */
	      /* notice that we are at a call, so we should ignore CP */
	      ENV_YREG[E_CP] = (CELL)NEXTOP(PREG,Osbmp);
	      ENV_YREG[E_CB] = (CELL)b_ptr;
	      ENV_YREG[E_E]  = (CELL)ENV;
#ifdef DEPTH_LIMIT
	      ENV_YREG[E_DEPTH] = DEPTH;
#endif  /* DEPTH_LIMIT */
	      ENV_YREG[-EnvSizeInCells-1]  = d1;
	      ENV = ENV_YREG;
	      ENV_YREG -= EnvSizeInCells+3;
	      CPREG = NEXTOP(PREG, Osbmp);
	      PREG = COMMA_CODE;
	      /* for profiler */
	      save_pc();
	      d0 = SREG[1];
	      goto restart_execute;

	      BEGP(pt1);
	      deref_body(d1, pt1, execute_comma_unk, execute_comma_nvar);
	      goto execute_metacall;
	      ENDP(pt1);
	      ENDD(d1);
	    } else if (mod != CurrentModule) {
	      goto execute_metacall;
	    }
	  }

	  /* copy arguments of meta-call to XREGS */
	  BEGP(pt1);
	  pt1 = RepAppl(d0);
	  BEGD(d2);
	  for (d2 = ArityOfFunctor(f); d2; d2--) {
#ifdef YAPOR_SBA
	    BEGD(d1);
	    d1 = pt1[d2];
	    if (d1 == 0) {
	      XREGS[d2] = (CELL)(pt1+d2);
	    } else {
	      XREGS[d2] = d1;
	    }
#else
	    XREGS[d2] = pt1[d2];
#endif
	  }
	  ENDD(d2);
	  ENDP(pt1);
	  CACHE_A1();
	} else if (IsAtomTerm(d0)) {
	  pen = RepPredProp(PredPropByAtom(AtomOfTerm(d0), mod));
	execute_pred_a:
	  /* handle extra pruning */
	  if (pen->FunctorOfPred == (Functor)AtomCut) {
	    if (b_ptr != B) {
	      saveregs();
	      prune(b_ptr PASS_REGS);
	      setregs();
	    }
	  }
	} else {
	  goto execute_metacall;
	}

	/* execute, but test first for interrupts */
      execute_end:
	// code copied from call
	#ifndef NO_CHECKING
	///	check_stack(NoStackPExecute, HR);
	#endif
      execute_stack_checked:
	CPREG = NEXTOP(PREG, Osbmp);
	ALWAYS_LOOKAHEAD(pen->OpcodeOfPred);
	PREG = pen->CodeOfPred;
	/* for profiler */
	save_pc();
#ifdef DEPTH_LIMIT
	if (DEPTH <= MkIntTerm(1)) {/* I assume Module==0 is primitives */
	  if (pen->ModuleOfPred) {
	    if (DEPTH == MkIntTerm(0)) {
	      FAIL();
	    } else {
	      DEPTH = RESET_DEPTH();
	    }
	  }
	} else if (pen->ModuleOfPred)
	  DEPTH -= MkIntConstant(2);
#endif  /* DEPTH_LIMIT */
#ifdef LOW_LEVEL_TRACER
	if (Yap_do_low_level_trace)
	  low_level_trace(enter_pred,pen,XREGS+1);
#endif  /* LOW_LEVEL_TRACER */
	WRITEBACK_Y_AS_ENV();
	/* setup GB */
	ENV_YREG[E_CB] = (CELL) B;
#ifdef YAPOR
	SCH_check_requests();
#endif  /* YAPOR */
	CACHE_A1();
	ALWAYS_GONext();
	ALWAYS_END_PREFETCH();

	/* meta-call: Prolog to the rescue */
	BEGP(pt1);
	deref_body(d0, pt1, execute_unk, execute_nvar);
      execute_metacall:
	ARG1 = ARG3 = d0;
	pen = PredMetaCall;
	ARG2 = Yap_cp_as_integer(b_ptr);
	if (mod)
	  ARG4 = mod;
	else
	  ARG4 = TermProlog;
	goto execute_end;
	ENDP(pt1);

	/* at this point, we have the arguments all set in the argument registers, pen says who is the current predicate. don't remove. */
	//NoStackPExecute:
	WRITEBACK_Y_AS_ENV();
#ifdef SHADOW_S
	Yap_REGS.S_ = SREG;
#endif
	saveregs_and_ycache();
	d0 = interrupt_pexecute( pen PASS_REGS );
	setregs_and_ycache();
#ifdef SHADOW_S
	SREG = Yap_REGS.S_;
#endif
	if (!d0) FAIL();
	if (d0 == 2) goto execute_stack_checked;
	goto execute_end;
	ENDBOp();

	ENDD(d0);
	ENDCACHE_Y_AS_ENV();
      }
