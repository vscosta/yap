#ifdef LOW_LEVEL_TRACER
#define P_FUNCTOR_INSTINIT \
      if (Yap_do_low_level_trace) \
	low_level_trace(enter_pred,RepPredProp(Yap_GetPredPropByFunc(FunctorFunctor,0)),XREGS+1); \
	BLOCKADDRESS = (CELL)(*_PREG); \
    register CELL d0, d1; \
    register CELL *pt0, *pt1; \
	while (1) { \
      d0 = ARG1; \
	  if (IsVarTerm(d0)) { \
	    (pt0) = (CELL *)(d0); \
        (d0) = *(CELL *)(d0); \
		while (Unsigned(pt0) != (d0)) { \
		  if(!IsVarTerm(d0)) { \
            if (IsApplTerm(d0)) { \
	          d1 = *RepAppl(d0); \
	          if (IsExtensionFunctor((Functor) d1)) { \
	            if (d1 <= (CELL)FunctorDouble && d1 >= (CELL)FunctorLongInt ) { \
	              d1 = MkIntTerm(0); \
	            } else { \
	              YAAM_FAIL; \
				  break; \
				} \
	          } else { \
	            d0 = MkAtomTerm(NameOfFunctor((Functor) d1)); \
	            d1 = MkIntTerm(ArityOfFunctor((Functor) d1)); \
	          } \
            } \
            else if (IsPairTerm(d0)) { \
	          d0 = TermDot; \
	          d1 = MkIntTerm(2); \
            } \
            else { \
	          d1 = MkIntTerm(0); \
            } \
	        register CELL arity = d1; \
	        d1 = ARG2; \
			if (IsVarTerm(d1) { \
			  (pt0) = (CELL *)(d1); \
              (d1) = *(CELL *)(d1); \
		      while (Unsigned(pt0) != (d1)) { \
		        if(!IsVarTerm(d1)) { \
	              if (d0 != d1) { \
	                YAAM_FAIL; \
					break; \
	              } \
	              d0 = arity; \
			      d1 = ARG3; \
	              if (IsVarTerm(d1)) { \
			        (pt0) = (CELL *)(d1); \
                    (d1) = *(CELL *)(d1); \
		            while (Unsigned(pt0) != (d1)) { \
		              if(!IsVarTerm(d1)) { \
					    if (d0 != d1) { \
	                      YAAM_FAIL; \
						  break; \
	                    } \
	                    (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	                    GONext(); \
						break; \
					  } \
					  (pt0) = (CELL *)(d1); \
                      (d1) = *(CELL *)(d1); \
					} \
					(*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	                Bind(pt0, d0); \
	                GONext(); \
					break; \
				  } \
				  else { \
	                if (d0 != d1) { \
	                  YAAM_FAIL; \
					  break; \
	                } \
	                (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	                GONext(); \
					break; \
				  } \
				} \
				(pt0) = (CELL *)(d1); \
                (d1) = *(CELL *)(d1); \
			  } \
			  Bind(pt0, d0); \
	          d0 = arity; \
			} \
			else { \
	          if (d0 != d1) { \
	            YAAM_FAIL; \
				break; \
	          } \
	          d0 = arity; \
			} \
			d1 = ARG3; \
			if (IsVarTerm(d1)) { \
			  (pt0) = (CELL *)(d1); \
              (d1) = *(CELL *)(d1); \
		      while (Unsigned(pt0) != (d1)) { \
		        if(!IsVarTerm(d1)) { \
				  if (d0 != d1) { \
	                YAAM_FAIL; \
					break; \
	              } \
	              (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	              GONext(); \
				  break; \
				} \
				(pt0) = (CELL *)(d1); \
                (d1) = *(CELL *)(d1); \
			  } \
			  (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	          Bind(pt0, d0); \
	          GONext(); \
			  break; \
			} \
            else { \
	          if (d0 != d1) { \
	            YAAM_FAIL; \
				break; \
	          } \
	          (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	          GONext(); \
			  break; \
			} \
		  } \
		  (pt0) = (CELL *)(d0); \
          (d0) = *(CELL *)(d0); \
		} \
		d0 = ARG2; \
		if (IsVarTerm(d0)) { \
		  (pt1) = (CELL *)(d0); \
          (d0) = *(CELL *)(d0); \
		  while (Unsigned(pt1) != (d0)) { \
		    if(!IsVarTerm(d0)) { \
			  d1 = ARG3; \
		      if (IsVarTerm(d1)) { \
		        (pt1) = (CELL *)(d1); \
                (d1) = *(CELL *)(d1); \
		        while (Unsigned(pt1) != (d1)) { \
		          if(!IsVarTerm(d1)) { \
				    if (IsIntTerm(d1)) { \
	                  d1 = IntOfTerm(d1); \
                    } else { \
	                  saveregs(); \
	                  Yap_Error(TYPE_ERROR_INTEGER,ARG3,"functor/3"); \
	                  setregs(); \
	                  YAAM_FAIL; \
					  break; \
                    } \
                    if (!IsAtomicTerm(d0)) { \
	                  saveregs(); \
	                  Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	                  setregs(); \
	                  YAAM_FAIL; \
					  break; \
                    } \
                    if (d0 == TermDot && d1 == 2) { \
	                  RESET_VARIABLE(HR); \
	                  RESET_VARIABLE(HR+1); \
	                  d0 = AbsPair(HR); \
	                  HR += 2; \
                    } \
                    else if ((Int)d1 > 0) { \
	                  if (!IsAtomTerm(d0)) { \
	                    saveregs(); \
	                    Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	                    setregs(); \
	                    YAAM_FAIL; \
						break; \
	                  } \
	                  if (!IsAtomTerm(d0)) { \
	                    YAAM_FAIL; \
						break; \
	                  } \
	                  else { \
	                    d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1); \
		              } \
	                  pt1 = HR; \
	                  *pt1++ = d0; \
	                  d0 = AbsAppl(HR); \
	                  if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) { \
	                    saveregs(); \
	                    if (!Yap_gcl((1+d1)*sizeof(CELL), 3, YREG, NEXTOP(NEXTOP((*_PREG),e),Osbmp))) { \
	                      Yap_NilError(RESOURCE_ERROR_STACK,LOCAL_ErrorMessage); \
	                      setregs(); \
	                      JMPNext(); \
						  break; \
	                    } else { \
	                      setregs(); \
	                    } \
	                    continue; \
	                  } \
	                  while ((Int)d1--) { \
	                    RESET_VARIABLE(pt1); \
	                    pt1++; \
	                  } \
	                  HR = pt1; \
                    } else if ((Int)d1  < 0) { \
	                  saveregs(); \
	                  Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3"); \
	                  setregs(); \
	                  YAAM_FAIL; \
					  break; \
                    } \
                    (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbpp),l); \
                    Bind(pt0, d0); \
                    GONext(); \
					break; \
			      } \
			      (pt1) = (CELL *)(d1); \
                  (d1) = *(CELL *)(d1); \
		        } \
		        saveregs(); \
                Yap_Error(INSTANTIATION_ERROR, d1, "functor/3"); \
                setregs(); \
                YAAM_FAIL; \
				break; \
		      } \
		      else { \
               if (IsIntTerm(d1)) { \
	              d1 = IntOfTerm(d1); \
                } else { \
	              saveregs(); \
	              Yap_Error(TYPE_ERROR_INTEGER,ARG3,"functor/3"); \
	              setregs(); \
	              YAAM_FAIL; \
				  break; \
                } \
                if (!IsAtomicTerm(d0)) { \
	              saveregs(); \
	              Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	              setregs(); \
	              YAAM_FAIL; \
				  break; \
                } \
                if (d0 == TermDot && d1 == 2) { \
	              RESET_VARIABLE(HR); \
	              RESET_VARIABLE(HR+1); \
	              d0 = AbsPair(HR); \
	              HR += 2; \
                } \
                else if ((Int)d1 > 0) { \
	              if (!IsAtomTerm(d0)) { \
	                saveregs(); \
	                Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	                setregs(); \
	                YAAM_FAIL; \
					break; \
	              } \
	              if (!IsAtomTerm(d0)) { \
	                YAAM_FAIL; \
					break; \
	              } \
	              else { \
	                d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1); \
		          } \
	              pt1 = HR; \
	              *pt1++ = d0; \
	              d0 = AbsAppl(HR); \
	              if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) { \
	                saveregs(); \
	                if (!Yap_gcl((1+d1)*sizeof(CELL), 3, YREG, NEXTOP(NEXTOP((*_PREG),e),Osbmp))) { \
	                  Yap_NilError(RESOURCE_ERROR_STACK,LOCAL_ErrorMessage); \
	                  setregs(); \
	                  JMPNext(); \
					  break; \
	                } else { \
	                  setregs(); \
	                } \
	                continue; \
	              } \
	              while ((Int)d1--) { \
	                RESET_VARIABLE(pt1); \
	                pt1++; \
	              } \
	              HR = pt1; \
                } else if ((Int)d1  < 0) { \
	              saveregs(); \
	              Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3"); \
	              setregs(); \
	              YAAM_FAIL; \
				  break; \
                } \
                (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbpp),l); \
                Bind(pt0, d0); \
                GONext(); \
				break; \
	          } \
			} \
			(pt1) = (CELL *)(d0); \
            (d0) = *(CELL *)(d0); \
		  } \
		  saveregs(); \
          Yap_Error(INSTANTIATION_ERROR, d0, "functor/3"); \
          setregs(); \
          YAAM_FAIL; \
		  break; \
		} \
		else { \
          d1 = ARG3; \
		  if (IsVarTerm(d1)) { \
		    (pt1) = (CELL *)(d1); \
            (d1) = *(CELL *)(d1); \
		    while (Unsigned(pt1) != (d1)) { \
		      if(!IsVarTerm(d1)) { \
				if (IsIntTerm(d1)) { \
	              d1 = IntOfTerm(d1); \
                } else { \
	              saveregs(); \
	              Yap_Error(TYPE_ERROR_INTEGER,ARG3,"functor/3"); \
	              setregs(); \
	              YAAM_FAIL; \
				  break; \
                } \
                if (!IsAtomicTerm(d0)) { \
	              saveregs(); \
	              Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	              setregs(); \
	              YAAM_FAIL; \
				  break; \
                } \
                if (d0 == TermDot && d1 == 2) { \
	              RESET_VARIABLE(HR); \
	              RESET_VARIABLE(HR+1); \
	              d0 = AbsPair(HR); \
	              HR += 2; \
                } \
                else if ((Int)d1 > 0) { \
	              if (!IsAtomTerm(d0)) { \
	                saveregs(); \
	                Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	                setregs(); \
	                YAAM_FAIL; \
					break; \
	              } \
	              if (!IsAtomTerm(d0)) { \
	                YAAM_FAIL; \
					break; \
	              } \
	              else { \
	                d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1); \
		          } \
	              pt1 = HR; \
	              *pt1++ = d0; \
	              d0 = AbsAppl(HR); \
	              if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) { \
	                saveregs(); \
	                if (!Yap_gcl((1+d1)*sizeof(CELL), 3, YREG, NEXTOP(NEXTOP((*_PREG),e),Osbmp))) { \
	                  Yap_NilError(RESOURCE_ERROR_STACK,LOCAL_ErrorMessage); \
	                  setregs(); \
	                  JMPNext(); \
					  break; \
	                } else { \
	                  setregs(); \
	                } \
	                continue; \
	              } \
	              while ((Int)d1--) { \
	                RESET_VARIABLE(pt1); \
	                pt1++; \
	              } \
	              HR = pt1; \
                } else if ((Int)d1  < 0) { \
	              saveregs(); \
	              Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3"); \
	              setregs(); \
	              YAAM_FAIL; \
				  break; \
                } \
                (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbpp),l); \
                Bind(pt0, d0); \
                GONext(); \
				break; \
			  } \
			  (pt1) = (CELL *)(d1); \
              (d1) = *(CELL *)(d1); \
		    } \
		    saveregs(); \
            Yap_Error(INSTANTIATION_ERROR, d1, "functor/3"); \
            setregs(); \
            YAAM_FAIL; \
			break; \
		  } \
		  else { \
            if (IsIntTerm(d1)) { \
	          d1 = IntOfTerm(d1); \
            } else { \
	          saveregs(); \
	          Yap_Error(TYPE_ERROR_INTEGER,ARG3,"functor/3"); \
	          setregs(); \
	          YAAM_FAIL; \
			  break; \
            } \
            if (!IsAtomicTerm(d0)) { \
	          saveregs(); \
	          Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	          setregs(); \
	          YAAM_FAIL; \
			  break; \
            } \
            if (d0 == TermDot && d1 == 2) { \
	          RESET_VARIABLE(HR); \
	          RESET_VARIABLE(HR+1); \
	          d0 = AbsPair(HR); \
	          HR += 2; \
            } \
            else if ((Int)d1 > 0) { \
	          if (!IsAtomTerm(d0)) { \
	            saveregs(); \
	            Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	            setregs(); \
	            YAAM_FAIL; \
				break; \
	          } \
	          if (!IsAtomTerm(d0)) { \
	            YAAM_FAIL; \
				break; \
	          } \
	          else { \
	            d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1); \
		      } \
	          pt1 = HR; \
	          *pt1++ = d0; \
	          d0 = AbsAppl(HR); \
	          if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) { \
	            saveregs(); \
	            if (!Yap_gcl((1+d1)*sizeof(CELL), 3, YREG, NEXTOP(NEXTOP((*_PREG),e),Osbmp))) { \
	              Yap_NilError(RESOURCE_ERROR_STACK,LOCAL_ErrorMessage); \
	              setregs(); \
	              JMPNext(); \
				  break; \
	            } else { \
	              setregs(); \
	            } \
	            continue; \
	          } \
	          while ((Int)d1--) { \
	            RESET_VARIABLE(pt1); \
	            pt1++; \
	          } \
	          HR = pt1; \
            } else if ((Int)d1  < 0) { \
	          saveregs(); \
	          Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3"); \
	          setregs(); \
	          YAAM_FAIL; \
			  break; \
            } \
            (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbpp),l); \
            Bind(pt0, d0); \
            GONext(); \
			break; \
	      } \
		} \
	  } \
	  else { \
	    if (IsApplTerm(d0)) { \
	      d1 = *RepAppl(d0); \
	      if (IsExtensionFunctor((Functor) d1)) { \
	        if (d1 <= (CELL)FunctorDouble && d1 >= (CELL)FunctorLongInt ) { \
	          d1 = MkIntTerm(0); \
	        } else { \
	          YAAM_FAIL; \
			  break; \
			} \
	      } else { \
	        d0 = MkAtomTerm(NameOfFunctor((Functor) d1)); \
	        d1 = MkIntTerm(ArityOfFunctor((Functor) d1)); \
	      } \
        } \
        else if (IsPairTerm(d0)) { \
	      d0 = TermDot; \
	      d1 = MkIntTerm(2); \
        } \
        else { \
	      d1 = MkIntTerm(0); \
        } \
	    register CELL arity = d1; \
	    d1 = ARG2; \
		if (IsVarTerm(d1) { \
		  (pt0) = (CELL *)(d1); \
          (d1) = *(CELL *)(d1); \
		  while (Unsigned(pt0) != (d1)) { \
		    if(!IsVarTerm(d1)) { \
	          if (d0 != d1) { \
	            YAAM_FAIL; \
				break; \
	          } \
	          d0 = arity; \
		      d1 = ARG3; \
	          if (IsVarTerm(d1)) { \
			    (pt0) = (CELL *)(d1); \
                (d1) = *(CELL *)(d1); \
		        while (Unsigned(pt0) != (d1)) { \
		          if(!IsVarTerm(d1)) { \
			        if (d0 != d1) { \
	                  YAAM_FAIL; \
					  break; \
	                } \
	                (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	                GONext(); \
					break; \
				  } \
				  (pt0) = (CELL *)(d1); \
                  (d1) = *(CELL *)(d1); \
				} \
				(*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	            Bind(pt0, d0); \
	            GONext(); \
				break; \
			  } \
			  else { \
	            if (d0 != d1) { \
	              YAAM_FAIL; \
				  break; \
	            } \
	            (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	            GONext(); \
				break; \
			  } \
			} \
			(pt0) = (CELL *)(d1); \
            (d1) = *(CELL *)(d1); \
		  } \
		  Bind(pt0, d0); \
	      d0 = arity; \
		} \
		else { \
	      if (d0 != d1) { \
	        YAAM_FAIL; \
			break; \
	      } \
	      d0 = arity; \
		} \
		d1 = ARG3; \
		if (IsVarTerm(d1)) { \
		  (pt0) = (CELL *)(d1); \
          (d1) = *(CELL *)(d1); \
		  while (Unsigned(pt0) != (d1)) { \
		    if(!IsVarTerm(d1)) { \
			  if (d0 != d1) { \
	            YAAM_FAIL; \
				break; \
	          } \
	          (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	          GONext(); \
			  break; \
			} \
			(pt0) = (CELL *)(d1); \
            (d1) = *(CELL *)(d1); \
		  } \
	      (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	      Bind(pt0, d0); \
	      GONext(); \
		  break; \
		} \
        else { \
	      if (d0 != d1) { \
	        YAAM_FAIL; \
			break; \
	      } \
	      (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	      GONext(); \
		  break; \
		} \
	  } \
	}
#else /* LOW_LEVEL_TRACER */
#define P_FUNCTOR_INSTINIT \
    BLOCKADDRESS = (CELL)(*_PREG); \
    register CELL d0, d1; \
    register CELL *pt0, *pt1; \
	while (1) { \
      d0 = ARG1; \
	  if (IsVarTerm(d0)) { \
	    (pt0) = (CELL *)(d0); \
        (d0) = *(CELL *)(d0); \
		while (Unsigned(pt0) != (d0)) { \
		  if(!IsVarTerm(d0)) { \
            if (IsApplTerm(d0)) { \
	          d1 = *RepAppl(d0); \
	          if (IsExtensionFunctor((Functor) d1)) { \
	            if (d1 <= (CELL)FunctorDouble && d1 >= (CELL)FunctorLongInt ) { \
	              d1 = MkIntTerm(0); \
	            } else { \
	              YAAM_FAIL; \
				  break; \
				} \
	          } else { \
	            d0 = MkAtomTerm(NameOfFunctor((Functor) d1)); \
	            d1 = MkIntTerm(ArityOfFunctor((Functor) d1)); \
	          } \
            } \
            else if (IsPairTerm(d0)) { \
	          d0 = TermDot; \
	          d1 = MkIntTerm(2); \
            } \
            else { \
	          d1 = MkIntTerm(0); \
            } \
	        register CELL arity = d1; \
	        d1 = ARG2; \
			if (IsVarTerm(d1)) { \
			  (pt0) = (CELL *)(d1); \
              (d1) = *(CELL *)(d1); \
		      while (Unsigned(pt0) != (d1)) { \
		        if(!IsVarTerm(d1)) { \
	              if (d0 != d1) { \
	                YAAM_FAIL; \
					break; \
	              } \
	              d0 = arity; \
			      d1 = ARG3; \
	              if (IsVarTerm(d1)) { \
			        (pt0) = (CELL *)(d1); \
                    (d1) = *(CELL *)(d1); \
		            while (Unsigned(pt0) != (d1)) { \
		              if(!IsVarTerm(d1)) { \
					    if (d0 != d1) { \
	                      YAAM_FAIL; \
						  break; \
	                    } \
	                    (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	                    GONext(); \
						break; \
					  } \
					  (pt0) = (CELL *)(d1); \
                      (d1) = *(CELL *)(d1); \
					} \
					(*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	                Bind(pt0, d0); \
	                GONext(); \
					break; \
				  } \
				  else { \
	                if (d0 != d1) { \
	                  YAAM_FAIL; \
					  break; \
	                } \
	                (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	                GONext(); \
					break; \
				  } \
				} \
				(pt0) = (CELL *)(d1); \
                (d1) = *(CELL *)(d1); \
			  } \
			  Bind(pt0, d0); \
	          d0 = arity; \
			} \
			else { \
	          if (d0 != d1) { \
	            YAAM_FAIL; \
				break; \
	          } \
	          d0 = arity; \
			} \
			d1 = ARG3; \
			if (IsVarTerm(d1)) { \
			  (pt0) = (CELL *)(d1); \
              (d1) = *(CELL *)(d1); \
		      while (Unsigned(pt0) != (d1)) { \
		        if(!IsVarTerm(d1)) { \
				  if (d0 != d1) { \
	                YAAM_FAIL; \
					break; \
	              } \
	              (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	              GONext(); \
				  break; \
				} \
				(pt0) = (CELL *)(d1); \
                (d1) = *(CELL *)(d1); \
			  } \
			  (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	          Bind(pt0, d0); \
	          GONext(); \
			  break; \
			} \
            else { \
	          if (d0 != d1) { \
	            YAAM_FAIL; \
				break; \
	          } \
	          (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	          GONext(); \
			  break; \
			} \
		  } \
		  (pt0) = (CELL *)(d0); \
          (d0) = *(CELL *)(d0); \
		} \
		d0 = ARG2; \
		if (IsVarTerm(d0)) { \
		  (pt1) = (CELL *)(d0); \
          (d0) = *(CELL *)(d0); \
		  while (Unsigned(pt1) != (d0)) { \
		    if(!IsVarTerm(d0)) { \
			  d1 = ARG3; \
		      if (IsVarTerm(d1)) { \
		        (pt1) = (CELL *)(d1); \
                (d1) = *(CELL *)(d1); \
		        while (Unsigned(pt1) != (d1)) { \
		          if(!IsVarTerm(d1)) { \
				    if (IsIntTerm(d1)) { \
	                  d1 = IntOfTerm(d1); \
                    } else { \
	                  saveregs(); \
	                  Yap_Error(TYPE_ERROR_INTEGER,ARG3,"functor/3"); \
	                  setregs(); \
	                  YAAM_FAIL; \
					  break; \
                    } \
                    if (!IsAtomicTerm(d0)) { \
	                  saveregs(); \
	                  Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	                  setregs(); \
	                  YAAM_FAIL; \
					  break; \
                    } \
                    if (d0 == TermDot && d1 == 2) { \
	                  RESET_VARIABLE(HR); \
	                  RESET_VARIABLE(HR+1); \
	                  d0 = AbsPair(HR); \
	                  HR += 2; \
                    } \
                    else if ((Int)d1 > 0) { \
	                  if (!IsAtomTerm(d0)) { \
	                    saveregs(); \
	                    Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	                    setregs(); \
	                    YAAM_FAIL; \
						break; \
	                  } \
	                  if (!IsAtomTerm(d0)) { \
	                    YAAM_FAIL; \
						break; \
	                  } \
	                  else { \
	                    d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1); \
		              } \
	                  pt1 = HR; \
	                  *pt1++ = d0; \
	                  d0 = AbsAppl(HR); \
	                  if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) { \
	                    saveregs(); \
	                    if (!Yap_gcl((1+d1)*sizeof(CELL), 3, YREG, NEXTOP(NEXTOP((*_PREG),e),Osbmp))) { \
	                      Yap_NilError(RESOURCE_ERROR_STACK,LOCAL_ErrorMessage); \
	                      setregs(); \
	                      JMPNext(); \
						  break; \
	                    } else { \
	                      setregs(); \
	                    } \
	                    continue; \
	                  } \
	                  while ((Int)d1--) { \
	                    RESET_VARIABLE(pt1); \
	                    pt1++; \
	                  } \
	                  HR = pt1; \
                    } else if ((Int)d1  < 0) { \
	                  saveregs(); \
	                  Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3"); \
	                  setregs(); \
	                  YAAM_FAIL; \
					  break; \
                    } \
                    (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbpp),l); \
                    Bind(pt0, d0); \
                    GONext(); \
					break; \
			      } \
			      (pt1) = (CELL *)(d1); \
                  (d1) = *(CELL *)(d1); \
		        } \
		        saveregs(); \
                Yap_Error(INSTANTIATION_ERROR, d1, "functor/3"); \
                setregs(); \
                YAAM_FAIL; \
				break; \
		      } \
		      else { \
               if (IsIntTerm(d1)) { \
	              d1 = IntOfTerm(d1); \
                } else { \
	              saveregs(); \
	              Yap_Error(TYPE_ERROR_INTEGER,ARG3,"functor/3"); \
	              setregs(); \
	              YAAM_FAIL; \
				  break; \
                } \
                if (!IsAtomicTerm(d0)) { \
	              saveregs(); \
	              Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	              setregs(); \
	              YAAM_FAIL; \
				  break; \
                } \
                if (d0 == TermDot && d1 == 2) { \
	              RESET_VARIABLE(HR); \
	              RESET_VARIABLE(HR+1); \
	              d0 = AbsPair(HR); \
	              HR += 2; \
                } \
                else if ((Int)d1 > 0) { \
	              if (!IsAtomTerm(d0)) { \
	                saveregs(); \
	                Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	                setregs(); \
	                YAAM_FAIL; \
					break; \
	              } \
	              if (!IsAtomTerm(d0)) { \
	                YAAM_FAIL; \
					break; \
	              } \
	              else { \
	                d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1); \
		          } \
	              pt1 = HR; \
	              *pt1++ = d0; \
	              d0 = AbsAppl(HR); \
	              if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) { \
	                saveregs(); \
	                if (!Yap_gcl((1+d1)*sizeof(CELL), 3, YREG, NEXTOP(NEXTOP((*_PREG),e),Osbmp))) { \
	                  Yap_NilError(RESOURCE_ERROR_STACK,LOCAL_ErrorMessage); \
	                  setregs(); \
	                  JMPNext(); \
					  break; \
	                } else { \
	                  setregs(); \
	                } \
	                continue; \
	              } \
	              while ((Int)d1--) { \
	                RESET_VARIABLE(pt1); \
	                pt1++; \
	              } \
	              HR = pt1; \
                } else if ((Int)d1  < 0) { \
	              saveregs(); \
	              Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3"); \
	              setregs(); \
	              YAAM_FAIL; \
				  break; \
                } \
                (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbpp),l); \
                Bind(pt0, d0); \
                GONext(); \
				break; \
	          } \
			} \
			(pt1) = (CELL *)(d0); \
            (d0) = *(CELL *)(d0); \
		  } \
		  saveregs(); \
          Yap_Error(INSTANTIATION_ERROR, d0, "functor/3"); \
          setregs(); \
          YAAM_FAIL; \
		  break; \
		} \
		else { \
          d1 = ARG3; \
		  if (IsVarTerm(d1)) { \
		    (pt1) = (CELL *)(d1); \
            (d1) = *(CELL *)(d1); \
		    while (Unsigned(pt1) != (d1)) { \
		      if(!IsVarTerm(d1)) { \
				if (IsIntTerm(d1)) { \
	              d1 = IntOfTerm(d1); \
                } else { \
	              saveregs(); \
	              Yap_Error(TYPE_ERROR_INTEGER,ARG3,"functor/3"); \
	              setregs(); \
	              YAAM_FAIL; \
				  break; \
                } \
                if (!IsAtomicTerm(d0)) { \
	              saveregs(); \
	              Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	              setregs(); \
	              YAAM_FAIL; \
				  break; \
                } \
                if (d0 == TermDot && d1 == 2) { \
	              RESET_VARIABLE(HR); \
	              RESET_VARIABLE(HR+1); \
	              d0 = AbsPair(HR); \
	              HR += 2; \
                } \
                else if ((Int)d1 > 0) { \
	              if (!IsAtomTerm(d0)) { \
	                saveregs(); \
	                Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	                setregs(); \
	                YAAM_FAIL; \
					break; \
	              } \
	              if (!IsAtomTerm(d0)) { \
	                YAAM_FAIL; \
					break; \
	              } \
	              else { \
	                d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1); \
		          } \
	              pt1 = HR; \
	              *pt1++ = d0; \
	              d0 = AbsAppl(HR); \
	              if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) { \
	                saveregs(); \
	                if (!Yap_gcl((1+d1)*sizeof(CELL), 3, YREG, NEXTOP(NEXTOP((*_PREG),e),Osbmp))) { \
	                  Yap_NilError(RESOURCE_ERROR_STACK,LOCAL_ErrorMessage); \
	                  setregs(); \
	                  JMPNext(); \
					  break; \
	                } else { \
	                  setregs(); \
	                } \
	                continue; \
	              } \
	              while ((Int)d1--) { \
	                RESET_VARIABLE(pt1); \
	                pt1++; \
	              } \
	              HR = pt1; \
                } else if ((Int)d1  < 0) { \
	              saveregs(); \
	              Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3"); \
	              setregs(); \
	              YAAM_FAIL; \
				  break; \
                } \
                (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbpp),l); \
                Bind(pt0, d0); \
                GONext(); \
				break; \
			  } \
			  (pt1) = (CELL *)(d1); \
              (d1) = *(CELL *)(d1); \
		    } \
		    saveregs(); \
            Yap_Error(INSTANTIATION_ERROR, d1, "functor/3"); \
            setregs(); \
            YAAM_FAIL; \
			break; \
		  } \
		  else { \
            if (IsIntTerm(d1)) { \
	          d1 = IntOfTerm(d1); \
            } else { \
	          saveregs(); \
	          Yap_Error(TYPE_ERROR_INTEGER,ARG3,"functor/3"); \
	          setregs(); \
	          YAAM_FAIL; \
			  break; \
            } \
            if (!IsAtomicTerm(d0)) { \
	          saveregs(); \
	          Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	          setregs(); \
	          YAAM_FAIL; \
			  break; \
            } \
            if (d0 == TermDot && d1 == 2) { \
	          RESET_VARIABLE(HR); \
	          RESET_VARIABLE(HR+1); \
	          d0 = AbsPair(HR); \
	          HR += 2; \
            } \
            else if ((Int)d1 > 0) { \
	          if (!IsAtomTerm(d0)) { \
	            saveregs(); \
	            Yap_Error(TYPE_ERROR_ATOM,d0,"functor/3"); \
	            setregs(); \
	            YAAM_FAIL; \
				break; \
	          } \
	          if (!IsAtomTerm(d0)) { \
	            YAAM_FAIL; \
				break; \
	          } \
	          else { \
	            d0 = (CELL) Yap_MkFunctor(AtomOfTerm(d0), (Int) d1); \
		      } \
	          pt1 = HR; \
	          *pt1++ = d0; \
	          d0 = AbsAppl(HR); \
	          if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) { \
	            saveregs(); \
	            if (!Yap_gcl((1+d1)*sizeof(CELL), 3, YREG, NEXTOP(NEXTOP((*_PREG),e),Osbmp))) { \
	              Yap_NilError(RESOURCE_ERROR_STACK,LOCAL_ErrorMessage); \
	              setregs(); \
	              JMPNext(); \
				  break; \
	            } else { \
	              setregs(); \
	            } \
	            continue; \
	          } \
	          while ((Int)d1--) { \
	            RESET_VARIABLE(pt1); \
	            pt1++; \
	          } \
	          HR = pt1; \
            } else if ((Int)d1  < 0) { \
	          saveregs(); \
	          Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3"); \
	          setregs(); \
	          YAAM_FAIL; \
			  break; \
            } \
            (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbpp),l); \
            Bind(pt0, d0); \
            GONext(); \
			break; \
	      } \
		} \
	  } \
	  else { \
	    if (IsApplTerm(d0)) { \
	      d1 = *RepAppl(d0); \
	      if (IsExtensionFunctor((Functor) d1)) { \
	        if (d1 <= (CELL)FunctorDouble && d1 >= (CELL)FunctorLongInt ) { \
	          d1 = MkIntTerm(0); \
	        } else { \
	          YAAM_FAIL; \
			  break; \
			} \
	      } else { \
	        d0 = MkAtomTerm(NameOfFunctor((Functor) d1)); \
	        d1 = MkIntTerm(ArityOfFunctor((Functor) d1)); \
	      } \
        } \
        else if (IsPairTerm(d0)) { \
	      d0 = TermDot; \
	      d1 = MkIntTerm(2); \
        } \
        else { \
	      d1 = MkIntTerm(0); \
        } \
	    register CELL arity = d1; \
	    d1 = ARG2; \
		if (IsVarTerm(d1)) { \
		  (pt0) = (CELL *)(d1); \
          (d1) = *(CELL *)(d1); \
		  while (Unsigned(pt0) != (d1)) { \
		    if(!IsVarTerm(d1)) { \
	          if (d0 != d1) { \
	            YAAM_FAIL; \
				break; \
	          } \
	          d0 = arity; \
		      d1 = ARG3; \
	          if (IsVarTerm(d1)) { \
			    (pt0) = (CELL *)(d1); \
                (d1) = *(CELL *)(d1); \
		        while (Unsigned(pt0) != (d1)) { \
		          if(!IsVarTerm(d1)) { \
			        if (d0 != d1) { \
	                  YAAM_FAIL; \
					  break; \
	                } \
	                (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	                GONext(); \
					break; \
				  } \
				  (pt0) = (CELL *)(d1); \
                  (d1) = *(CELL *)(d1); \
				} \
				(*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	            Bind(pt0, d0); \
	            GONext(); \
				break; \
			  } \
			  else { \
	            if (d0 != d1) { \
	              YAAM_FAIL; \
				  break; \
	            } \
	            (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	            GONext(); \
				break; \
			  } \
			} \
			(pt0) = (CELL *)(d1); \
            (d1) = *(CELL *)(d1); \
		  } \
		  Bind(pt0, d0); \
	      d0 = arity; \
		} \
		else { \
	      if (d0 != d1) { \
	        YAAM_FAIL; \
			break; \
	      } \
	      d0 = arity; \
		} \
		d1 = ARG3; \
		if (IsVarTerm(d1)) { \
		  (pt0) = (CELL *)(d1); \
          (d1) = *(CELL *)(d1); \
		  while (Unsigned(pt0) != (d1)) { \
		    if(!IsVarTerm(d1)) { \
			  if (d0 != d1) { \
	            YAAM_FAIL; \
				break; \
	          } \
	          (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	          GONext(); \
			  break; \
			} \
			(pt0) = (CELL *)(d1); \
            (d1) = *(CELL *)(d1); \
		  } \
	      (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	      Bind(pt0, d0); \
	      GONext(); \
		  break; \
		} \
        else { \
	      if (d0 != d1) { \
	        YAAM_FAIL; \
			break; \
	      } \
	      (*_PREG) = NEXTOP(NEXTOP(NEXTOP((*_PREG), e),Osbmp),l); \
	      GONext(); \
		  break; \
		} \
	  } \
	}
#endif /* LOW_LEVEL_TRACER */

#define P_FUNCTOR_END \
      BLOCK = (CELL)P_FUNCTOR_END;
