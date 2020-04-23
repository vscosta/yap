/************************************************************************\
 *    Indexing in ARG1                                                *
\************************************************************************/


#ifdef INDENT_CODE
{
  {
#endif /* INDENT_CODE */

      BOp(user_switch, lp);
      {
        yamop *new = Yap_udi_search(PREG->y_u.lp.p);
        if (!new) {
          PREG = PREG->y_u.lp.l;
          JMPNext();
        }
        PREG = new;
        JMPNext();
      }
      ENDBOp();

      BOp(switch_on_type, llll);
      BEGD(d0);
      d0 = CACHED_A1();
      deref_head(d0, swt_unk);
      /* nonvar */
    swt_nvar:
      if (IsPairTerm(d0)) {
        /* pair */
        SREG = RepPair(d0);
        copy_jmp_address(PREG->y_u.llll.l1);
        PREG = PREG->y_u.llll.l1;
        JMPNext();
      }
      else if (!IsApplTerm(d0)) {
        /* constant */
        copy_jmp_address(PREG->y_u.llll.l2);
        PREG = PREG->y_u.llll.l2;
        I_R = d0;
        JMPNext();
      }
      else {
        /* appl */
        copy_jmp_address(PREG->y_u.llll.l3);
        PREG = PREG->y_u.llll.l3;
        SREG = RepAppl(d0);
        JMPNext();
      }

      BEGP(pt0);
      deref_body(d0, pt0, swt_unk, swt_nvar);
      /* variable */
      copy_jmp_address(PREG->y_u.llll.l4);
      PREG = PREG->y_u.llll.l4;
      JMPNext();
      ENDP(pt0);
      ENDD(d0);
      ENDBOp();

      /* specialised case where the arguments may be:
       * a list;
       * the empty list;
       * some other atom;
       * a variable;
       *
       */
      BOp(switch_list_nl, ollll);
      ALWAYS_LOOKAHEAD(PREG->y_u.ollll.pop);
      BEGD(d0);
      d0 = CACHED_A1();
#if UNIQUE_TAG_FOR_PAIRS
      deref_list_head(d0, swlnl_unk_p);
    swlnl_list_p:
      {
#else
        deref_head(d0, swlnl_unk_p);
        /* non variable */
      swlnl_nvar_p:
        if (__builtin_expect(IsPairTerm(d0),1)) {
          /* pair */
#endif
          copy_jmp_address(PREG->y_u.ollll.l1);
          PREG = PREG->y_u.ollll.l1;
          SREG = RepPair(d0);
          ALWAYS_GONext();
        }
#if UNIQUE_TAG_FOR_PAIRS
      swlnl_nlist_p:
#endif
        if (d0 == TermNil) {
          /* empty list */
          PREG = PREG->y_u.ollll.l2;
          JMPNext();
        }
        else {
          /* appl or constant */
          if (IsApplTerm(d0)) {
            copy_jmp_address(PREG->y_u.ollll.l3);
            PREG = PREG->y_u.ollll.l3;
            SREG = RepAppl(d0);
            JMPNext();
          } else {
            copy_jmp_address(PREG->y_u.ollll.l3);
            PREG = PREG->y_u.ollll.l3;
            I_R = d0;
            JMPNext();
          }
        }

        BEGP(pt0);
#if UNIQUE_TAG_FOR_PAIRS
      swlnl_unk_p:
        deref_list_body(d0, pt0, swlnl_list_p, swlnl_nlist_p);
#else
        deref_body(d0, pt0, swlnl_unk_p, swlnl_nvar_p);
#endif
        ENDP(pt0);
        /* variable */
        copy_jmp_address(PREG->y_u.ollll.l4);
        PREG = PREG->y_u.ollll.l4;
        JMPNext();
        ENDD(d0);
      }
      ENDBOp();

      BOp(switch_on_arg_type, xllll);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xllll.x);
      deref_head(d0, arg_swt_unk);
      /* nonvar */
    arg_swt_nvar:
      if (IsPairTerm(d0)) {
        /* pair */
        copy_jmp_address(PREG->y_u.xllll.l1);
        PREG = PREG->y_u.xllll.l1;
        SREG = RepPair(d0);
        JMPNext();
      }
      else if (!IsApplTerm(d0)) {
        /* constant */
        copy_jmp_address(PREG->y_u.xllll.l2);
        PREG = PREG->y_u.xllll.l2;
        I_R = d0;
        JMPNext();
      }
      else {
        /* appl */
        copy_jmp_address(PREG->y_u.xllll.l3);
        PREG = PREG->y_u.xllll.l3;
        SREG = RepAppl(d0);
        JMPNext();
      }

      BEGP(pt0);
      deref_body(d0, pt0, arg_swt_unk, arg_swt_nvar);
      /* variable */
      copy_jmp_address(PREG->y_u.xllll.l4);
      PREG = PREG->y_u.xllll.l4;
      JMPNext();
      ENDP(pt0);
      ENDD(d0);
      ENDBOp();

      BOp(switch_on_sub_arg_type, sllll);
      BEGD(d0);
      d0 = SREG[PREG->y_u.sllll.s];
      deref_head(d0, sub_arg_swt_unk);
      /* nonvar */
    sub_arg_swt_nvar:
      if (IsPairTerm(d0)) {
        /* pair */
        copy_jmp_address(PREG->y_u.sllll.l1);
        PREG = PREG->y_u.sllll.l1;
        SREG = RepPair(d0);
        JMPNext();
      }
      else if (!IsApplTerm(d0)) {
        /* constant */
        copy_jmp_address(PREG->y_u.sllll.l2);
        PREG = PREG->y_u.sllll.l2;
        I_R = d0;
        JMPNext();
      }
      else {
        /* appl */
        copy_jmp_address(PREG->y_u.sllll.l3);
        PREG = PREG->y_u.sllll.l3;
        SREG = RepAppl(d0);
        JMPNext();
      }

      BEGP(pt0);
      deref_body(d0, pt0, sub_arg_swt_unk, sub_arg_swt_nvar);
      /* variable */
      copy_jmp_address(PREG->y_u.sllll.l4);
      PREG = PREG->y_u.sllll.l4;
      JMPNext();
      ENDP(pt0);
      ENDD(d0);
      ENDBOp();

      BOp(jump_if_var, l);
      BEGD(d0);
      d0 = CACHED_A1();
      deref_head(d0, jump_if_unk);
      /* non var */
    jump0_if_nonvar:
      PREG = NEXTOP(PREG, l);
      JMPNext();

      BEGP(pt0);
      deref_body(d0, pt0, jump_if_unk, jump0_if_nonvar);
      /* variable */
      copy_jmp_address(PREG->y_u.l.l);
      PREG = PREG->y_u.l.l;
      ENDP(pt0);
      JMPNext();
      ENDD(d0);
      ENDBOp();

      BOp(jump_if_nonvar, xll);
      BEGD(d0);
      d0 = XREG(PREG->y_u.xll.x);
      deref_head(d0, jump2_if_unk);
      /* non var */
    jump2_if_nonvar:
      copy_jmp_address(PREG->y_u.xll.l1);
      PREG = PREG->y_u.xll.l1;
      JMPNext();

      BEGP(pt0);
      deref_body(d0, pt0, jump2_if_unk, jump2_if_nonvar);
      /* variable */
      PREG = NEXTOP(PREG, xll);
      ENDP(pt0);
      JMPNext();
      ENDD(d0);
      ENDBOp();

      BOp(if_not_then, clll);
      BEGD(d0);
      d0 = CACHED_A1();
      deref_head(d0, if_n_unk);
    if_n_nvar:
      /* not variable */
      if (d0 == PREG->y_u.clll.c) {
        /* equal to test value */
        copy_jmp_address(PREG->y_u.clll.l2);
        PREG = PREG->y_u.clll.l2;
        JMPNext();
      }
      else {
        /* different from test value */
        /* the case to optimise */
        copy_jmp_address(PREG->y_u.clll.l1);
        PREG = PREG->y_u.clll.l1;
        JMPNext();
      }

      BEGP(pt0);
      deref_body(d0, pt0, if_n_unk, if_n_nvar);
      ENDP(pt0);
      /* variable */
      copy_jmp_address(PREG->y_u.clll.l3);
      PREG = PREG->y_u.clll.l3;
      JMPNext();
      ENDD(d0);
      ENDBOp();

      /************************************************************************\
       *    Indexing on ARG1                                                        *
\************************************************************************/

#define HASH_SHIFT 6

      BOp(switch_on_func, sssl);
      BEGD(d1);
      d1 = *SREG++;
      /* we use a very simple hash function to find elements in a
       * switch table */
      {
        CELL
          /* first, calculate the mask */
          Mask = (PREG->y_u.sssl.s - 1) << 1,       /* next, calculate the hash function */
          hash = d1 >> (HASH_SHIFT - 1) & Mask;
        CELL *base;

        base = (CELL *)PREG->y_u.sssl.l;
        /* PREG now points at the beginning of the hash table */
        BEGP(pt0);
        /* pt0 will always point at the item */
        pt0 = base + hash;
        BEGD(d0);
        d0 = pt0[0];
        /* a match happens either if we found the value, or if we
         * found an empty slot */
        if (d0 == d1 || d0 == 0) {
          copy_jmp_addressa(pt0+1);
          PREG = (yamop *) (pt0[1]);
          JMPNext();
        }
        else {
          /* ooops, collision, look for other items   */
          register CELL d = ((d1 | 1) << 1) & Mask;

          while (1) {
            hash = (hash + d) & Mask;
            pt0 = base + hash;
            d0 = pt0[0];
            if (d0 == d1 || d0 == 0) {
              copy_jmp_addressa(pt0+1);
              PREG = (yamop *) pt0[1];
              JMPNext();
            }
          }
        }
        ENDD(d0);
        ENDP(pt0);
      }
      ENDD(d1);
      ENDBOp();

      BOp(switch_on_cons, sssl);
      BEGD(d1);
      d1 = I_R;
      /* we use a very simple hash function to find elements in a
       * switch table */
      {
        CELL
          /* first, calculate the mask */
          Mask = (PREG->y_u.sssl.s - 1) << 1,       /* next, calculate the hash function */
          hash = d1 >> (HASH_SHIFT - 1) & Mask;
        CELL *base;

        base = (CELL *)PREG->y_u.sssl.l;
        /* PREG now points at the beginning of the hash table */
        BEGP(pt0);
        /* pt0 will always point at the item */
        pt0 = base + hash;
        BEGD(d0);
        d0 = pt0[0];
        /* a match happens either if we found the value, or if we
         * found an empty slot */
        if (d0 == d1 || d0 == 0) {
          copy_jmp_addressa(pt0+1);
          PREG = (yamop *) (pt0[1]);
          JMPNext();
        }
        else {
          /* ooops, collision, look for other items   */
          register CELL d = ((d1 | 1) << 1) & Mask;

          while (1) {
            hash = (hash + d) & Mask;
            pt0 = base + hash;
            d0 = pt0[0];
            if (d0 == d1 || d0 == 0) {
              copy_jmp_addressa(pt0+1);
              PREG = (yamop *) pt0[1];
              JMPNext();
            }
          }
        }
        ENDD(d0);
        ENDP(pt0);
      }
      ENDD(d1);
      ENDBOp();

      BOp(go_on_func, sssl);
      BEGD(d0);
      {
        CELL *pt = (CELL *)(PREG->y_u.sssl.l);

        d0 = *SREG++;
        if (d0 == pt[0]) {
          copy_jmp_addressa(pt+1);
          PREG = (yamop *) pt[1];
          JMPNext();
        } else {
          copy_jmp_addressa(pt+3);
          PREG = (yamop *) pt[3];
          JMPNext();
        }
      }
      ENDD(d0);
      ENDBOp();

      BOp(go_on_cons, sssl);
      BEGD(d0);
      {
        CELL *pt = (CELL *)(PREG->y_u.sssl.l);

        d0 = I_R;
        if (d0 == pt[0]) {
          copy_jmp_addressa(pt+1);
          PREG = (yamop *) pt[1];
          JMPNext();
        } else {
          copy_jmp_addressa(pt+3);
          PREG = (yamop *) pt[3];
          JMPNext();
        }
      }
      ENDD(d0);
      ENDBOp();

      BOp(if_func, sssl);
      BEGD(d1);
      BEGP(pt0);
      pt0 = (CELL *) PREG->y_u.sssl.l;
      d1 = *SREG++;
      while (pt0[0] != d1 && pt0[0] != (CELL)NULL ) {
        pt0 += 2;
      }
      copy_jmp_addressa(pt0+1);
      PREG = (yamop *) (pt0[1]);
      JMPNext();
      ENDP(pt0);
      ENDD(d1);
      ENDBOp();

      BOp(if_cons, sssl);
      BEGD(d1);
      BEGP(pt0);
      pt0 = (CELL *) PREG->y_u.sssl.l;
      d1 = I_R;
      while (pt0[0] != d1 && pt0[0] != 0L ) {
	pt0 += 2;
      }
      copy_jmp_addressa(pt0+1);
      PREG = (yamop *) (pt0[1]);
      JMPNext();
      ENDP(pt0);
      ENDD(d1);
      ENDBOp();

      Op(index_dbref, e);
      PREG = NEXTOP(PREG, e);
      I_R = AbsAppl(SREG-1);
      GONext();
      ENDOp();

      Op(index_blob, e);
      PREG = NEXTOP(PREG, e);
      I_R = Yap_DoubleP_key(SREG);
      GONext();
      ENDOp();

      Op(index_long, e);
      PREG = NEXTOP(PREG, e);
      I_R = Yap_IntP_key(SREG);
      GONext();
      ENDOp();



