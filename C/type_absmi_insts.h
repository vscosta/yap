#if 0
{
  {
#endif
    /************************************************************************ \
     *	Basic Primitive Predicates					 *
\************************************************************************/

    Op(p_atom_x, xl);
    BEGD(d0);
    d0 = XREG(PREG->y_u.xl.x);
    deref_head(d0, atom_x_unk);
  atom_x_nvar:
    if (IsAtomTerm(d0) && !IsBlob(AtomOfTerm(d0))) {
      PREG = NEXTOP(PREG, xl);
      GONext();
    }
    else {
      PREG = PREG->y_u.xl.F;
      GONext();
    }

    BEGP(pt0);
    deref_body(d0, pt0, atom_x_unk, atom_x_nvar);
    PREG = PREG->y_u.xl.F;
    GONext();
    ENDP(pt0);
    ENDD(d0);
    ENDOp();

    Op(p_atom_y, yl);
    BEGD(d0);
    BEGP(pt0);
    pt0 = YREG + PREG->y_u.yl.y;
    d0 = *pt0;
    deref_head(d0, atom_y_unk);
  atom_y_nvar:
    if (IsAtomTerm(d0) && !IsBlob(AtomOfTerm(d0))) {
      PREG = NEXTOP(PREG, yl);
      GONext();
    }
    else {
      PREG = PREG->y_u.yl.F;
      GONext();
    }

    derefa_body(d0, pt0, atom_y_unk, atom_y_nvar);
    PREG = PREG->y_u.yl.F;
    GONext();
    ENDP(pt0);
    ENDD(d0);
    ENDOp();

    Op(p_atomic_x, xl);
    BEGD(d0);
    d0 = XREG(PREG->y_u.xl.x);
    deref_head(d0, atomic_x_unk);
  atomic_x_nvar:
    /* non variable */
    if (IsAtomicTerm(d0)) {
      PREG = NEXTOP(PREG, xl);
      GONext();
    }
    else {
      PREG = PREG->y_u.xl.F;
      GONext();
    }

    BEGP(pt0);
    deref_body(d0, pt0, atomic_x_unk, atomic_x_nvar);
    PREG = PREG->y_u.xl.F;
    GONext();
    ENDP(pt0);
    ENDD(d0);
    ENDOp();

    Op(p_atomic_y, yl);
    BEGD(d0);
    BEGP(pt0);
    pt0 = YREG + PREG->y_u.yl.y;
    d0 = *pt0;
    deref_head(d0, atomic_y_unk);
  atomic_y_nvar:
    /* non variable */
    if (IsAtomicTerm(d0)) {
      PREG = NEXTOP(PREG, yl);
      GONext();
    }
    else {
      PREG = PREG->y_u.yl.F;
      GONext();
    }

    derefa_body(d0, pt0, atomic_y_unk, atomic_y_nvar);
    PREG = PREG->y_u.yl.F;
    GONext();
    ENDP(pt0);
    ENDD(d0);
    ENDOp();

    Op(p_integer_x, xl);
    BEGD(d0);
    d0 = XREG(PREG->y_u.xl.x);
    deref_head(d0, integer_x_unk);
  integer_x_nvar:
    /* non variable */
    if (IsIntTerm(d0)) {
      PREG = NEXTOP(PREG, xl);
      GONext();
    }
    if (IsApplTerm(d0)) {
      Functor f0 = FunctorOfTerm(d0);
      if (IsExtensionFunctor(f0)) {
	switch ((CELL)f0) {
	case (CELL)FunctorBigInt:
	  { CELL *pt = RepAppl(d0);
	    if (  pt[1] != BIG_INT ) {
	      PREG = PREG->y_u.xl.F;
	      GONext();
	    }
	  }
    PREG = NEXTOP(PREG, xl);
    GONext();
	  break;
	case (CELL)FunctorLongInt:
	  PREG = NEXTOP(PREG, xl);
	  GONext();
	  break;
	default:
	  PREG = PREG->y_u.xl.F;
	  GONext();
	}
      }
    }
    PREG = PREG->y_u.xl.F;
    GONext();

    BEGP(pt0);
    deref_body(d0, pt0, integer_x_unk, integer_x_nvar);
    PREG = PREG->y_u.xl.F;
    GONext();
    ENDP(pt0);
    ENDD(d0);
    ENDOp();

    Op(p_integer_y, yl);
    BEGD(d0);
    BEGP(pt0);
    pt0 = YREG + PREG->y_u.yl.y;
    d0 = *pt0;
    deref_head(d0, integer_y_unk);
  integer_y_nvar:
    /* non variable */
    if (IsIntTerm(d0)) {
      PREG = NEXTOP(PREG, yl);
      GONext();
    }
    if (IsApplTerm(d0)) {
      Functor f0 = FunctorOfTerm(d0);
      if (IsExtensionFunctor(f0)) {
	switch ((CELL)f0) {
	case (CELL)FunctorBigInt:
	  { CELL *pt = RepAppl(d0);
	    if (  pt[1] != BIG_INT ) {
	      PREG = PREG->y_u.yl.F;
	      GONext();
	    }
	  }
    PREG = NEXTOP(PREG, yl);
    GONext();
    break;
	case (CELL)FunctorLongInt:
	  PREG = NEXTOP(PREG, yl);
	  GONext();
    break;
	default:
	  PREG = PREG->y_u.yl.F;
	  GONext();
	}
      }
    }
    PREG = PREG->y_u.yl.F;
    GONext();

    derefa_body(d0, pt0, integer_y_unk, integer_y_nvar);
    PREG = PREG->y_u.yl.F;
    GONext();
    ENDP(pt0);
    ENDD(d0);
    ENDOp();

    Op(p_nonvar_x, xl);
    BEGD(d0);
    d0 = XREG(PREG->y_u.xl.x);
    deref_head(d0, nonvar_x_unk);
  nonvar_x_nvar:
    PREG = NEXTOP(PREG, xl);
    GONext();

    BEGP(pt0);
    deref_body(d0, pt0, nonvar_x_unk, nonvar_x_nvar);
    PREG = PREG->y_u.xl.F;
    GONext();
    ENDP(pt0);
    ENDD(d0);
    ENDOp();

    Op(p_nonvar_y, yl);
    BEGD(d0);
    BEGP(pt0);
    pt0 = YREG + PREG->y_u.yl.y;
    d0 = *pt0;
    deref_head(d0, nonvar_y_unk);
  nonvar_y_nvar:
    PREG = NEXTOP(PREG, yl);
    GONext();

    derefa_body(d0, pt0, nonvar_y_unk, nonvar_y_nvar);
    PREG = PREG->y_u.yl.F;
    GONext();
    ENDP(pt0);
    ENDD(d0);
    ENDOp();

    Op(p_number_x, xl);
    BEGD(d0);
    d0 = XREG(PREG->y_u.xl.x);
    deref_head(d0, number_x_unk);
  number_x_nvar:
    /* non variable */
    if (IsIntTerm(d0)) {
      PREG = NEXTOP(PREG, xl);
      GONext();
    }
    if (IsApplTerm(d0)) {
      Functor f0 = FunctorOfTerm(d0);
      if (IsExtensionFunctor(f0)) {
	switch ((CELL)f0) {
	case (CELL)FunctorBigInt:
	  { CELL *pt = RepAppl(d0);
	    if (  pt[1] != BIG_RATIONAL || pt[1] != BIG_INT ) {
	      PREG = PREG->y_u.xl.F;
	      GONext();
	    }
	  }
	case (CELL)FunctorLongInt:
	case (CELL)FunctorDouble:
	  PREG = NEXTOP(PREG, xl);
	  GONext();
	  break;
	default:
	  PREG = PREG->y_u.xl.F;
	  GONext();
	}
      }
    }
    PREG = PREG->y_u.xl.F;
    GONext();

    BEGP(pt0);
    deref_body(d0, pt0, number_x_unk, number_x_nvar);
    PREG = PREG->y_u.xl.F;
    GONext();
    ENDP(pt0);
    ENDD(d0);
    ENDOp();

    Op(p_number_y, yl);
    BEGD(d0);
    BEGP(pt0);
    pt0 = YREG + PREG->y_u.yl.y;
    d0 = *pt0;
    deref_head(d0, number_y_unk);
  number_y_nvar:
    /* non variable */
    /* non variable */
    if (IsIntTerm(d0)) {
      PREG = NEXTOP(PREG, xl);
      GONext();
    }
    if (IsApplTerm(d0)) {
      Functor f0 = FunctorOfTerm(d0);
      if (IsExtensionFunctor(f0)) {
	switch ((CELL)f0) {
	case (CELL)FunctorBigInt:
	  { CELL *pt = RepAppl(d0);
	    if (  pt[1] != BIG_RATIONAL || pt[1] != BIG_INT ) {
	      PREG = PREG->y_u.yl.F;
	      GONext();
	    }
	  }
	  break;
	case (CELL)FunctorLongInt:
	case (CELL)FunctorDouble:
	  PREG = NEXTOP(PREG, yl);
	  GONext();
	  break;
	default:
	  PREG = PREG->y_u.yl.F;
	  GONext();
	}
      }
    }
    PREG = PREG->y_u.yl.F;
    GONext();

    derefa_body(d0, pt0, number_y_unk, number_y_nvar);
    PREG = PREG->y_u.yl.F;
    GONext();
    ENDP(pt0);
    ENDD(d0);
    ENDOp();

    Op(p_var_x, xl);
    BEGD(d0);
    d0 = XREG(PREG->y_u.xl.x);
    deref_head(d0, var_x_unk);
  var_x_nvar:
    /* non variable */
    PREG = PREG->y_u.xl.F;
    GONext();

    BEGP(pt0);
    deref_body(d0, pt0, var_x_unk, var_x_nvar);
    PREG = NEXTOP(PREG, xl);
    GONext();
    ENDP(pt0);
    ENDD(d0);
    ENDOp();

    Op(p_var_y, yl);
    BEGD(d0);
    BEGP(pt0);
    pt0 = YREG + PREG->y_u.yl.y;
    d0 = *pt0;
    deref_head(d0, var_y_unk);
  var_y_nvar:
    /* non variable */
    PREG = PREG->y_u.yl.F;
    GONext();

    derefa_body(d0, pt0, var_y_unk, var_y_nvar);
    PREG = NEXTOP(PREG, yl);
    GONext();
    ENDP(pt0);
    ENDD(d0);
    ENDOp();

    Op(p_db_ref_x, xl);
    BEGD(d0);
    d0 = XREG(PREG->y_u.xl.x);
    deref_head(d0, dbref_x_unk);
  dbref_x_nvar:
    /* non variable */
    if (IsDBRefTerm(d0)) {
      /* only allow references to the database, not general references
       * to go through. */
      PREG = NEXTOP(PREG, xl);
      GONext();
    }
    else {
      PREG = PREG->y_u.xl.F;
      GONext();
    }

    BEGP(pt0);
    deref_body(d0, pt0, dbref_x_unk, dbref_x_nvar);
    PREG = PREG->y_u.xl.F;
    GONext();
    ENDP(pt0);
    ENDD(d0);
    ENDOp();

    Op(p_db_ref_y, yl);
    BEGD(d0);
    BEGP(pt0);
    pt0 = YREG + PREG->y_u.yl.y;
    d0 = *pt0;
    deref_head(d0, dbref_y_unk);
  dbref_y_nvar:
    /* non variable */
    if (IsDBRefTerm(d0)) {
      /* only allow references to the database, not general references
       * to go through. */
      PREG = NEXTOP(PREG, yl);
      GONext();
    }
    else {
      PREG = PREG->y_u.yl.F;
      GONext();
    }

    derefa_body(d0, pt0, dbref_y_unk, dbref_y_nvar);
    PREG = PREG->y_u.yl.F;
    GONext();
    ENDP(pt0);
    ENDD(d0);
    ENDOp();

    Op(p_primitive_x, xl);
    BEGD(d0);
    d0 = XREG(PREG->y_u.xl.x);
    deref_head(d0, primi_x_unk);
  primi_x_nvar:
    /* non variable */
    if (IsPrimitiveTerm(d0)) {
      PREG = NEXTOP(PREG, xl);
      GONext();
    }
    else {
      PREG = PREG->y_u.xl.F;
      GONext();
    }

    BEGP(pt0);
    deref_body(d0, pt0, primi_x_unk, primi_x_nvar);
    PREG = PREG->y_u.xl.F;
    GONext();
    ENDP(pt0);
    ENDD(d0);
    ENDOp();

    Op(p_primitive_y, yl);
    BEGD(d0);
    BEGP(pt0);
    pt0 = YREG + PREG->y_u.yl.y;
    d0 = *pt0;
    deref_head(d0, primi_y_unk);
  primi_y_nvar:
    /* non variable */
    if (IsPrimitiveTerm(d0)) {
      PREG = NEXTOP(PREG, yl);
      GONext();
    }
    else {
      PREG = PREG->y_u.yl.F;
      GONext();
    }

    derefa_body(d0, pt0, primi_y_unk, primi_y_nvar);
    PREG = PREG->y_u.yl.F;
    GONext();
    ENDP(pt0);
    ENDD(d0);
    ENDOp();

    Op(p_compound_x, xl);
    BEGD(d0);
    d0 = XREG(PREG->y_u.xl.x);
    deref_head(d0, compound_x_unk);
  compound_x_nvar:
    /* non variable */
    if (IsPairTerm(d0)) {
      PREG = NEXTOP(PREG, xl);
      GONext();
    }
    else if (IsApplTerm(d0)) {
      if (IsExtensionFunctor(FunctorOfTerm(d0))) {
	PREG = PREG->y_u.xl.F;
	GONext();
      }
      PREG = NEXTOP(PREG, xl);
      GONext();
    }
    else {
      PREG = PREG->y_u.xl.F;
      GONext();
    }

    BEGP(pt0);
    deref_body(d0, pt0, compound_x_unk, compound_x_nvar);
    PREG = PREG->y_u.xl.F;
    GONext();
    ENDP(pt0);
    ENDD(d0);
    ENDOp();

    Op(p_compound_y, yl);
    BEGD(d0);
    BEGP(pt0);
    pt0 = YREG + PREG->y_u.yl.y;
    d0 = *pt0;
    deref_head(d0, compound_y_unk);
  compound_y_nvar:
    /* non variable */
    if (IsPairTerm(d0)) {
      PREG = NEXTOP(PREG, yl);
      GONext();
    }
    else if (IsApplTerm(d0)) {
      if (IsExtensionFunctor(FunctorOfTerm(d0))) {
	PREG = PREG->y_u.yl.F;
	GONext();
      }
      PREG = NEXTOP(PREG, yl);
      GONext();
    }
    else {
      PREG = PREG->y_u.yl.F;
      GONext();
    }

    derefa_body(d0, pt0, compound_y_unk, compound_y_nvar);
    PREG = PREG->y_u.yl.F;
    GONext();
    ENDP(pt0);
    ENDD(d0);
    ENDOp();

    Op(p_float_x, xl);
    BEGD(d0);
    d0 = XREG(PREG->y_u.xl.x);
    deref_head(d0, float_x_unk);
  float_x_nvar:
    /* non variable */
    if (IsFloatTerm(d0)) {
      PREG = NEXTOP(PREG, xl);
      GONext();
    }
    PREG = PREG->y_u.xl.F;
    GONext();

    BEGP(pt0);
    deref_body(d0, pt0, float_x_unk, float_x_nvar);
    PREG = PREG->y_u.xl.F;
    GONext();
    ENDP(pt0);
    ENDD(d0);
    ENDOp();

    Op(p_float_y, yl);
    BEGD(d0);
    BEGP(pt0);
    pt0 = YREG + PREG->y_u.yl.y;
    d0 = *pt0;
    deref_head(d0, float_y_unk);
  float_y_nvar:
    /* non variable */
    if (IsFloatTerm(d0)) {
      PREG = NEXTOP(PREG, yl);
      GONext();
    }
    PREG = PREG->y_u.yl.F;
    GONext();

    derefa_body(d0, pt0, float_y_unk, float_y_nvar);
    PREG = PREG->y_u.yl.F;
    GONext();
    ENDP(pt0);
    ENDD(d0);
    ENDOp();
