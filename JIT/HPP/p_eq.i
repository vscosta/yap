#define P_EQ_INSTINIT \
      BLOCKADDRESS = (CELL)(*_PREG); \

#ifdef LOW_LEVEL_TRACER
#define P_EQ_LOW_LEVEL_TRACER \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorSame,0)),XREGS+1);
#endif

#define P_EQ_POST_LOW_LEVEL_TRACER \
	  register CELL d0, d1, d2; \
	  register CELL *pt0, *pt1; \
      d0 = ARG1;

#define P_EQ_P_EQ_NVAR1 \
      d1 = ARG2;

#ifdef USE_GMP
#define P_EQ_P_EQ_NVAR1_NVAR2 \
    BLOCK = (CELL)P_EQ_P_EQ_NVAR1_NVAR2; \
    FAILED = 0; \
    if (d0 == d1) { \
	  (*_PREG) = NEXTOP((*_PREG), l); \
	  GONext(); \
    } \
    else { \
      if (IsPairTerm(d0)) { \
	    if (!IsPairTerm(d1)) { \
	      (*_PREG) = (*_PREG)->u.l.l; \
	      GONext(); \
	    } \
	    else { \
	      always_save_pc(); \
	      d2 = iequ_complex(RepPair(d0)-1, RepPair(d0)+1,RepPair(d1)-1); \
	      if (d2 == FALSE) { \
	        (*_PREG) = (*_PREG)->u.l.l; \
	        GONext(); \
	      } \
	      else { \
	        always_set_pc(); \
	        (*_PREG) = NEXTOP((*_PREG), l); \
	        GONext(); \
	      } \
	    } \
      } \
      else if (IsApplTerm(d0)) { \
	    Functor f0 = FunctorOfTerm(d0); \
	    Functor f1; \
	    if (!IsApplTerm(d1)) { \
	      (*_PREG) = (*_PREG)->u.l.l; \
	      GONext(); \
	    } \
	    else { \
	      f1 = FunctorOfTerm(d1); \
	      if (IsExtensionFunctor(f0)) { \
	        switch ((CELL)f0) { \
	          case (CELL)FunctorDBRef: \
	            if (d0 == d1) { \
	              (*_PREG) = NEXTOP((*_PREG), l); \
	              GONext(); \
	            } \
	            else { \
	              (*_PREG) = (*_PREG)->u.l.l; \
	              GONext(); \
	            } \
	            break; \
	          case (CELL)FunctorLongInt: \
	            if (f1 != FunctorLongInt) { \
	              (*_PREG) = (*_PREG)->u.l.l; \
	              GONext(); \
	            } \
	            else if (LongIntOfTerm(d0) == LongIntOfTerm(d1)) { \
	              (*_PREG) = NEXTOP((*_PREG), l); \
	              GONext(); \
	            } \
	            else { \
	              (*_PREG) = (*_PREG)->u.l.l; \
	              GONext(); \
	            } \
	            break; \
	          case (CELL)FunctorBigInt: \
	            if (f1 != FunctorBigInt) { \
	              (*_PREG) = (*_PREG)->u.l.l; \
	              GONext(); \
	            } \
	            else if (Yap_gmp_tcmp_big_big(d0,d1) == 0) { \
	              (*_PREG) = NEXTOP((*_PREG), l); \
	              GONext(); \
	            } \
	            else { \
	              (*_PREG) = (*_PREG)->u.l.l; \
	              GONext(); \
	            } \
	            break; \
	          case (CELL)FunctorDouble: \
	            if (f1 != FunctorDouble) { \
	              (*_PREG) = (*_PREG)->u.l.l; \
	              GONext(); \
	            } \
	            else if (FloatOfTerm(d0) == FloatOfTerm(d1)) { \
	              (*_PREG) = NEXTOP((*_PREG), l); \
	              GONext(); \
	            } \
	            break; \
	          default: \
	            (*_PREG) = (*_PREG)->u.l.l; \
	            GONext(); \
	        } \
	      } \
	      else { \
	        if (f0 != f1) { \
	          (*_PREG) = (*_PREG)->u.l.l; \
	          GONext(); \
	        } \
	        else { \
	          always_save_pc(); \
	          d2 = iequ_complex(RepAppl(d0), RepAppl(d0)+ArityOfFunctor(f0), RepAppl(d1)); \
	          if (d2 == FALSE) { \
	            (*_PREG) = (*_PREG)->u.l.l; \
	            GONext(); \
	          } \
	          else { \
	            always_set_pc(); \
	            (*_PREG) = NEXTOP((*_PREG), l); \
	            GONext(); \
	          } \
	        } \
	      } \
	    } \
      } \
      else { \
        (*_PREG) = (*_PREG)->u.l.l; \
        GONext(); \
      } \
    }
#else /* USE_GMP */
#define P_EQ_P_EQ_NVAR1_NVAR2 \
    BLOCK = (CELL)P_EQ_P_EQ_NVAR1_NVAR2; \
    FAILED = 0; \
    if (d0 == d1) { \
	  (*_PREG) = NEXTOP((*_PREG), l); \
	  GONext(); \
    } \
    else { \
      if (IsPairTerm(d0)) { \
	    if (!IsPairTerm(d1)) { \
	      (*_PREG) = (*_PREG)->u.l.l; \
	      GONext(); \
	    } \
	    else { \
	      always_save_pc(); \
	      d2 = iequ_complex(RepPair(d0)-1, RepPair(d0)+1,RepPair(d1)-1); \
	      if (d2 == FALSE) { \
	        (*_PREG) = (*_PREG)->u.l.l; \
	        GONext(); \
	      } \
	      else { \
	        always_set_pc(); \
	        (*_PREG) = NEXTOP((*_PREG), l); \
	        GONext(); \
	      } \
	    } \
      } \
      else if (IsApplTerm(d0)) { \
	    Functor f0 = FunctorOfTerm(d0); \
	    Functor f1; \
	    if (!IsApplTerm(d1)) { \
	      (*_PREG) = (*_PREG)->u.l.l; \
	      GONext(); \
	    } \
	    else { \
	      f1 = FunctorOfTerm(d1); \
	      if (IsExtensionFunctor(f0)) { \
	        switch ((CELL)f0) { \
	          case (CELL)FunctorDBRef: \
	            if (d0 == d1) { \
	              (*_PREG) = NEXTOP((*_PREG), l); \
	              GONext(); \
	            } \
	            else { \
	              (*_PREG) = (*_PREG)->u.l.l; \
	              GONext(); \
	            } \
	            break; \
	          case (CELL)FunctorLongInt: \
	            if (f1 != FunctorLongInt) { \
	              (*_PREG) = (*_PREG)->u.l.l; \
	              GONext(); \
	            } \
	            else if (LongIntOfTerm(d0) == LongIntOfTerm(d1)) { \
	              (*_PREG) = NEXTOP((*_PREG), l); \
	              GONext(); \
	            } \
	            else { \
	              (*_PREG) = (*_PREG)->u.l.l; \
	              GONext(); \
	            } \
	            break; \
	          case (CELL)FunctorDouble: \
	            if (f1 != FunctorDouble) { \
	              (*_PREG) = (*_PREG)->u.l.l; \
	              GONext(); \
	            } \
	            else if (FloatOfTerm(d0) == FloatOfTerm(d1)) { \
	              (*_PREG) = NEXTOP((*_PREG), l); \
	              GONext(); \
	            } \
	            break; \
	          default: \
	            (*_PREG) = (*_PREG)->u.l.l; \
	            GONext(); \
	        } \
	      } \
	      else { \
	        if (f0 != f1) { \
	          (*_PREG) = (*_PREG)->u.l.l; \
	          GONext(); \
	        } \
	        else { \
	          always_save_pc(); \
	          d2 = iequ_complex(RepAppl(d0), RepAppl(d0)+ArityOfFunctor(f0), RepAppl(d1)); \
	          if (d2 == FALSE) { \
	            (*_PREG) = (*_PREG)->u.l.l; \
	            GONext(); \
	          } \
	          else { \
	            always_set_pc(); \
	            (*_PREG) = NEXTOP((*_PREG), l); \
	            GONext(); \
	          } \
	        } \
	      } \
	    } \
      } \
      else { \
        (*_PREG) = (*_PREG)->u.l.l; \
        GONext(); \
      } \
    }
#endif /* USE_GMP */

#define P_EQ_P_EQ_NVAR1_UNK2 \
      (*_PREG) = (*_PREG)->u.l.l; \
      GONext();

#define P_EQ_P_EQ_UNK1 \
      d1 = ARG2;

#define P_EQ_P_EQ_VAR1_NVAR2 \
      (*_PREG) = (*_PREG)->u.l.l; \
      GONext();

#define P_EQ_P_EQ_VAR1_UNK2_END \
    BLOCK = (CELL)P_EQ_P_EQ_VAR1_UNK2_END; \
      if (pt1 != pt0) { \
	(*_PREG) = (*_PREG)->u.l.l; \
	GONext(); \
      } \
      else { \
    (*_PREG) = NEXTOP((*_PREG), l); \
    GONext(); \
      }
