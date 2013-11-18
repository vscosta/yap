
#include "pl-incl.h"
#include "pl-ctype.h"
#include "pl-utf8.h"
#include "pl-dtoa.h"
#include "pl-umap.c"			/* Unicode map */

#include "pl-read.h"			/* read structure */



static bool
isStringStream(IOSTREAM *s)
{ return s->functions == &Sstringfunctions;
}



static void
init_read_data(ReadData _PL_rd, IOSTREAM *in ARG_LD)
{  CACHE_REGS
   memset(_PL_rd, 0, sizeof(*_PL_rd));	/* optimise! */

  _PL_rd->magic = RD_MAGIC;
  _PL_rd->varnames = 0;
  _PL_rd->module = Yap_GetModuleEntry(CurrentModule);
  rb.stream = in;
  _PL_rd->has_exception = 0;
  _PL_rd->exception = 0;
}

static void
free_read_data(ReadData _PL_rd)
{ 
}

static void
ptr_to_location(const unsigned char *here, source_location *pos, ReadData _PL_rd)
{ unsigned char const *s, *ll = NULL;
  int c;

  *pos = _PL_rd->start_of_term;

						/* update line number */
  for(s=rdbase; s<here; s = utf8_get_uchar(s, &c))
  { pos->position.charno++;

    if ( c == '\n' )
    { pos->position.lineno++;
      ll = s+1;
    }
  }
						/* update line position */
  if ( ll )
  { s = ll;
    pos->position.linepos = 0;
  } else
  { s = rdbase;
  }

  for(; s<here; s++)
  { switch(*s)
    { case '\b':
	if ( pos->position.linepos > 0 )
	  pos->position.linepos--;
      break;
    case '\t':
      pos->position.linepos |= 7;		/* TBD: set tab distance */
    default:
      pos->position.linepos++;
    }
  }

  pos->position.byteno = 0;			/* we do not know */
}

static int 
read_term(term_t t, ReadData _PL_rd ARG_LD)
{
  return Yap_read_term(t, rb.stream, _PL_rd);
}


static void	  addUTF8Buffer(Buffer b, int c);

static void
addUTF8Buffer(Buffer b, int c)
{ if ( c >= 0x80 )
  { char buf[6];
    char *p, *end;

    end = utf8_put_char(buf, c);
    for(p=buf; p<end; p++)
    { addBuffer(b, *p&0xff, char);
    }
  } else
  { addBuffer(b, c, char);
  }
}

                 /*******************************
                 *     UNICODE CLASSIFIERS      *
                 *******************************/

#define CharTypeW(c, t, w) \
        ((unsigned)(c) <= 0xff ? (_PL_char_types[(unsigned)(c)] t) \
                               : (uflagsW(c) & (w)))

#define PlBlankW(c)     CharTypeW(c, == SP, U_SEPARATOR)
#define PlUpperW(c)     CharTypeW(c, == UC, U_UPPERCASE)
#define PlIdStartW(c)   (c <= 0xff ? (isLower(c)||isUpper(c)||c=='_') \
                                   : uflagsW(c) & U_ID_START)
#define PlIdContW(c)    CharTypeW(c, >= UC, U_ID_CONTINUE)
#define PlSymbolW(c)    CharTypeW(c, == SY, U_SYMBOL)
#define PlPunctW(c)     CharTypeW(c, == PU, 0)
#define PlSoloW(c)      CharTypeW(c, == SO, U_OTHER)
#define PlInvalidW(c)   (uflagsW(c) == 0)

int
f_is_prolog_var_start(wint_t c)
{ return PlIdStartW(c) && (PlUpperW(c) || c == '_');
}

int
f_is_prolog_atom_start(wint_t c)
{ return PlIdStartW(c) != 0;
}

int
f_is_prolog_identifier_continue(wint_t c)
{ return PlIdContW(c) || c == '_';
}

int
f_is_prolog_symbol(wint_t c)
{ return PlSymbolW(c) != 0;
}

int
unicode_separator(pl_wchar_t c)
{ return PlBlankW(c);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	FALSE	return false
	TRUE	redo
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
reportReadError(ReadData rd)
{ if ( rd->on_error == ATOM_error )
    return PL_raise_exception(rd->exception);
  if ( rd->on_error != ATOM_quiet )
    printMessage(ATOM_error, PL_TERM, rd->exception);
  PL_clear_exception();

  if ( rd->on_error == ATOM_dec10 )
    return TRUE;

  return FALSE;
}


		/********************************
		*           RAW READING         *
		*********************************/


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Scan the input, give prompts when necessary and return a char *  holding
a  stripped  version of the next term.  Contiguous white space is mapped
on a single space, block and % ... \n comment  is  deleted.   Memory  is
claimed automatically en enlarged if necessary.

(char *) NULL is returned on a syntax error.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define syntaxError(what, rd) { errorWarning(what, 0, rd); fail; }

static term_t
makeErrorTerm(const char *id_str, term_t id_term, ReadData _PL_rd)
{ GET_LD
  term_t ex, loc=0;			/* keep compiler happy */
  unsigned char const *s, *ll = NULL;
  int rc = TRUE;

  if ( !(ex = PL_new_term_ref()) ||
       !(loc = PL_new_term_ref()) )
    rc = FALSE;

  if ( rc && !id_term )
  { if ( !(id_term=PL_new_term_ref()) ||
	 !PL_put_atom_chars(id_term, id_str) )
      rc = FALSE;
  }

  if ( rc )
    rc = PL_unify_term(ex,
		       PL_FUNCTOR, FUNCTOR_error2,
		         PL_FUNCTOR, FUNCTOR_syntax_error1,
		           PL_TERM, id_term,
		         PL_TERM, loc);

  source_char_no += last_token_start - rdbase;
  for(s=rdbase; s<last_token_start; s++)
  { if ( *s == '\n' )
    { source_line_no++;
      ll = s+1;
    }
  }

  if ( ll )
  { int lp = 0;

    for(s = ll; s<last_token_start; s++)
    { switch(*s)
      { case '\b':
	  if ( lp > 0 ) lp--;
	  break;
	case '\t':
	  lp |= 7;
	default:
	  lp++;
      }
    }

    source_line_pos = lp;
  }

  if ( rc )
  { if ( ReadingSource )			/* reading a file */
    { rc = PL_unify_term(loc,
			 PL_FUNCTOR, FUNCTOR_file4,
			   PL_ATOM, source_file_name,
			   PL_INT, source_line_no,
			   PL_INT, source_line_pos,
			   PL_INT64, source_char_no);
    } else if ( isStringStream(rb.stream) )
    { size_t pos;

      pos = utf8_strlen((char *)rdbase, last_token_start-rdbase);

      rc = PL_unify_term(loc,
			 PL_FUNCTOR, FUNCTOR_string2,
			   PL_UTF8_STRING, rdbase,
			   PL_INT, (int)pos);
    } else				/* any stream */
    { term_t stream;

      if ( !(stream=PL_new_term_ref()) ||
	   !PL_unify_stream_or_alias(stream, rb.stream) ||
	   !PL_unify_term(loc,
			  PL_FUNCTOR, FUNCTOR_stream4,
			    PL_TERM, stream,
			    PL_INT, source_line_no,
			    PL_INT, source_line_pos,
			    PL_INT64, source_char_no) )
	rc = FALSE;
    }
  }

  return (rc ? ex : (term_t)0);
}



static bool
errorWarning(const char *id_str, term_t id_term, ReadData _PL_rd)
{ GET_LD
  term_t ex;

  LD->exception.processing = TRUE;	/* allow using spare stack */

  ex = makeErrorTerm(id_str, id_term, _PL_rd);

  if ( _PL_rd )
  { _PL_rd->has_exception = TRUE;
    if ( ex )
      PL_put_term(_PL_rd->exception, ex);
    else
      PL_put_term(_PL_rd->exception, exception_term);
  } else
  { if ( ex )
      PL_raise_exception(ex);
  }

  fail;
}



static void
clearBuffer(ReadData _PL_rd)
{ if (rb.size == 0)
  { rb.base = rb.fast;
    rb.size = sizeof(rb.fast);
  }
  rb.end = rb.base + rb.size;
  rdbase = rb.here = rb.base;

  _PL_rd->posp = rdbase;
  _PL_rd->posi = 0;
}


static void
growToBuffer(int c, ReadData _PL_rd)
{ if ( rb.base == rb.fast )		/* intptr_t clause: jump to use malloc() */
  { rb.base = PL_malloc(FASTBUFFERSIZE * 2);
    memcpy(rb.base, rb.fast, FASTBUFFERSIZE);
  } else
    rb.base = PL_realloc(rb.base, rb.size*2);

  DEBUG(8, Sdprintf("Reallocated read buffer at %ld\n", (intptr_t) rb.base));
  _PL_rd->posp = rdbase = rb.base;
  rb.here = rb.base + rb.size;
  rb.size *= 2;
  rb.end  = rb.base + rb.size;
  _PL_rd->posi = 0;

  *rb.here++ = c;
}


static inline void
addByteToBuffer(int c, ReadData _PL_rd)
{ c &= 0xff;

  if ( rb.here >= rb.end )
    growToBuffer(c, _PL_rd);
  else
    *rb.here++ = c;
}


static void
addToBuffer(int c, ReadData _PL_rd)
{ if ( c <= 0x7f )
  { addByteToBuffer(c, _PL_rd);
  } else
  { char buf[10];
    char *s, *e;

    e = utf8_put_char(buf, c);
    for(s=buf; s<e; s++)
      addByteToBuffer(*s, _PL_rd);
  }
}



#if __YAP_PROLOG__
void
Yap_setCurrentSourceLocation(IOSTREAM ** rd)
{
  GET_LD
  if (*rd) {
    read_data rdt;
    rdt._rb.stream = *rd;
    setCurrentSourceLocation(&rdt PASS_LD);
  }
}
#endif


static inline int
getchr__(ReadData _PL_rd)
{ int c = Sgetcode(rb.stream);

  if ( !_PL_rd->char_conversion_table || c < 0 || c >= 256 )
    return c;

  return _PL_rd->char_conversion_table[c];
}


#define getchr()  getchr__(_PL_rd)
#define getchrq() Sgetcode(rb.stream)

#define ensure_space(c) { if ( something_read && \
			       (c == '\n' || !isBlank(rb.here[-1])) ) \
			   addToBuffer(c, _PL_rd); \
		        }
#define set_start_line { if ( !something_read ) \
			 { setCurrentSourceLocation(_PL_rd PASS_LD); \
			   something_read++; \
			 } \
		       }





#ifdef O_QUASIQUOTATIONS
/** '$qq_open'(+QQRange, -Stream) is det.

Opens a quasi-quoted memory range.

@arg QQRange is a term '$quasi_quotation'(ReadData, Start, Length)
@arg Stream  is a UTF-8 encoded string, whose position indication
	     reflects the location in the real file.
*/

static
PRED_IMPL("$qq_open", 2, qq_open, 0)
{ PRED_LD

  if ( PL_is_functor(A1, FUNCTOR_dquasi_quotation3) )
  { void *ptr;
    size_t start, len;
    term_t arg = PL_new_term_ref();

    if ( PL_get_arg(1, A1, arg) && PL_get_pointer_ex(arg, &ptr) &&
	 PL_get_arg(2, A1, arg) && PL_get_size_ex(arg, &start) &&
	 PL_get_arg(3, A1, arg) && PL_get_size_ex(arg, &len) )
    { ReadData _PL_rd = ptr;

      if ( _PL_rd->magic == RD_MAGIC )
      { char *s_start = (char*)rdbase+start;
	IOSTREAM *s;

	if ( (s=Sopenmem(&s_start, &len, "r")) )
	{ source_location pos;

	  s->encoding = ENC_UTF8;
	  ptr_to_location((unsigned char*)s_start, &pos, _PL_rd);
	  if ( pos.file )
	    setFileNameStream(s, pos.file);
	  if ( pos.position.lineno > 0 )
	  { s->position = &s->posbuf;
	    *s->position = pos.position;
	  }

	  return PL_unify_stream(A2, s);
	}
      } else
	PL_existence_error("read_context", A1);
    }
  } else
    PL_type_error("read_context", A1);

  return FALSE;
}


static int
parse_quasi_quotations(ReadData _PL_rd ARG_LD)
{ if ( _PL_rd->qq_tail )
  { term_t av;
    int rc;

    if ( !PL_unify_nil(_PL_rd->qq_tail) )
      return FALSE;

    if ( !_PL_rd->quasi_quotations )
    { if ( (av = PL_new_term_refs(2)) &&
	   PL_put_term(av+0, _PL_rd->qq) &&
#if __YAP_PROLOG__
	   PL_put_atom(av+1, YAP_SWIAtomFromAtom(_PL_rd->module->AtomOfME)) &&
#else
	   PL_put_atom(av+1, _PL_rd->module->name) &&
#endif
	   PL_cons_functor_v(av, FUNCTOR_dparse_quasi_quotations2, av) )
      { term_t ex;

	rc = callProlog(MODULE_system, av+0, PL_Q_CATCH_EXCEPTION, &ex);
	if ( rc )
	  return TRUE;
	_PL_rd->exception = ex;
	_PL_rd->has_exception = TRUE;
      }
      return FALSE;
    } else
      return TRUE;
  } else if ( _PL_rd->quasi_quotations )	/* user option, but no quotes */
  { return PL_unify_nil(_PL_rd->quasi_quotations);
  } else
    return TRUE;
}


static int
is_quasi_quotation_syntax(term_t type, ReadData _PL_rd)
{ GET_LD
  term_t plain = PL_new_term_ref();
  term_t ex;
  Module m = _PL_rd->module;
  atom_t name;
  int arity;

  PL_strip_module(type, &m, plain);

  if ( PL_get_name_arity(plain, &name, &arity) )
  { if ( _PL_rd->quasi_quotations )
    { return TRUE;
    } else
    { Procedure proc;

      if ( (proc=resolveProcedure(PL_new_functor(name, 4), m)) &&
#if __YAP_PROLOG__
	   proc->PredFlags & QuasiQuotationPredFlag
#else
	   true(proc->definition, P_QUASI_QUOTATION_SYNTAX)
#endif
	   )
	return TRUE;

      if ( (ex = PL_new_term_ref()) &&
	   PL_unify_term(ex,
			 PL_FUNCTOR_CHARS, "unknown_quasi_quotation_syntax", 2,
			   PL_TERM, type,
#if __YAP_PROLOG__
			   PL_ATOM, YAP_SWIAtomFromAtom(m->AtomOfME)
#else
			   PL_ATOM, m->name
#endif
			 ) )
	return errorWarning(NULL, ex, _PL_rd);
    }
  } else
  { if ( (ex = PL_new_term_ref()) &&
	 PL_unify_term(ex, PL_FUNCTOR_CHARS, "invalid_quasi_quotation_syntax", 1,
		       PL_TERM, type) )
      return errorWarning(NULL, ex, _PL_rd);
  }

  return FALSE;
}

#endif /*O_QUASIQUOTATIONS*/



#define rawSyntaxError(what) { addToBuffer(EOS, _PL_rd); \
			       rdbase = rb.base, last_token_start = rb.here-1; \
			       syntaxError(what, _PL_rd); \
			     }

static int
raw_read_quoted(int q, ReadData _PL_rd)
{ int newlines = 0;
  int c;

  addToBuffer(q, _PL_rd);
  while((c=getchrq()) != EOF && c != q)
  { if ( c == '\\' && DO_CHARESCAPE )
    { int base;

      addToBuffer(c, _PL_rd);

      switch( (c=getchrq()) )
      { case EOF:
	  goto eofinstr;
	case 'u':			/* \uXXXX */
	case 'U':			/* \UXXXXXXXX */
	  addToBuffer(c, _PL_rd);
	  continue;
	case 'x':			/* \xNN\ */
	  addToBuffer(c, _PL_rd);
	  c = getchrq();
	  if ( c == EOF )
	    goto eofinstr;
	  if ( digitValue(16, c) >= 0 )
	  { base = 16;
	    addToBuffer(c, _PL_rd);

	  xdigits:
	    c = getchrq();
	    while( digitValue(base, c) >= 0 )
	    { addToBuffer(c, _PL_rd);
	      c = getchrq();
	    }
	  }
	  if ( c == EOF )
	    goto eofinstr;
	  addToBuffer(c, _PL_rd);
	  if ( c == q )
	    return TRUE;
	  continue;
	default:
	  addToBuffer(c, _PL_rd);
	  if ( digitValue(8, c) >= 0 )	/* \NNN\ */
	  { base = 8;
	    goto xdigits;
	  } else if ( c == '\n' )	/* \<newline> */
	  { c = getchrq();
	    if ( c == EOF )
	      goto eofinstr;
	    addToBuffer(c, _PL_rd);
	    if ( c == q )
	      return TRUE;
	  }
	  continue;			/* \symbolic-control-char */
      }
    } else if (c == '\n' &&
	       newlines++ > MAXNEWLINES &&
	       (_PL_rd->styleCheck & LONGATOM_CHECK))
    { rawSyntaxError("long_string");
    }
    addToBuffer(c, _PL_rd);
  }
  if (c == EOF)
  { eofinstr:
      rawSyntaxError("end_of_file_in_string");
  }
  addToBuffer(c, _PL_rd);

  return TRUE;
}


static int
add_comment(Buffer b, IOPOS *pos, ReadData _PL_rd ARG_LD)
{ term_t head = PL_new_term_ref();

  assert(_PL_rd->comments);
  if ( !PL_unify_list(_PL_rd->comments, head, _PL_rd->comments) )
    return FALSE;
  if ( pos )
  { if ( !PL_unify_term(head,
			PL_FUNCTOR, FUNCTOR_minus2,
			  PL_FUNCTOR, FUNCTOR_stream_position4,
			    PL_INT64, pos->charno,
			    PL_INT, pos->lineno,
			    PL_INT, pos->linepos,
			    PL_INT, 0,
			  PL_UTF8_STRING, baseBuffer(b, char)) )
      return FALSE;
  } else
  { if ( !PL_unify_term(head,
			PL_FUNCTOR, FUNCTOR_minus2,
			  ATOM_minus,
			  PL_UTF8_STRING, baseBuffer(b, char)) )
      return FALSE;
  }

  PL_reset_term_refs(head);
  return TRUE;
}


static void
setErrorLocation(IOPOS *pos, ReadData _PL_rd)
{ if ( pos )
  { GET_LD

    source_char_no = pos->charno;
    source_line_pos = pos->linepos;
    source_line_no = pos->lineno;
  }
  rb.here = rb.base+1;			/* see rawSyntaxError() */
}


static unsigned char *
raw_read2(ReadData _PL_rd ARG_LD)
{ int c;
  bool something_read = FALSE;
  bool dotseen = FALSE;
  IOPOS pbuf;					/* comment start */
  IOPOS *pos;

  clearBuffer(_PL_rd);				/* clear input buffer */
  _PL_rd->strictness = truePrologFlag(PLFLAG_ISO);
  source_line_no = -1;

  for(;;)
  { c = getchr();

  handle_c:
    switch(c)
    { case EOF:
		if ( isStringStream(rb.stream) ) /* do not require '. ' when */
		{ addToBuffer(' ', _PL_rd);     /* reading from a string */
		  addToBuffer('.', _PL_rd);
		  addToBuffer(' ', _PL_rd);
		  addToBuffer(EOS, _PL_rd);
		  return rb.base;
		}
		if (something_read)
		{ if ( dotseen )		/* term.<EOF> */
		  { if ( rb.here - rb.base == 1 )
		      rawSyntaxError("end_of_clause");
		    ensure_space(' ');
		    addToBuffer(EOS, _PL_rd);
		    return rb.base;
		  }
		  rawSyntaxError("end_of_file");
		}
		if ( Sfpasteof(rb.stream) )
		{ term_t stream;

		  LD->exception.processing = TRUE;
		  stream = PL_new_term_ref();
		  PL_unify_stream_or_alias(stream, rb.stream);
		  PL_error(NULL, 0, NULL, ERR_PERMISSION,
			   ATOM_input, ATOM_past_end_of_stream, stream);
		  return NULL;
		}
		set_start_line;
		strcpy((char *)rb.base, "end_of_file. ");
		rb.here = rb.base + 14;
		return rb.base;
      case '/': if ( rb.stream->position )
		{ pbuf = *rb.stream->position;
		  pbuf.charno--;
		  pbuf.linepos--;
		  pos = &pbuf;
		} else
		  pos = NULL;

	        c = getchr();
		if ( c == '*' )
		{ int last;
		  int level = 1;
		  union {
		    tmp_buffer ctmpbuf;
		    buffer tmpbuf;
		  } u;
		  Buffer cbuf;

		  if ( _PL_rd->comments )
		  { initBuffer(&u.ctmpbuf);
		    cbuf = &u.tmpbuf;
		    addUTF8Buffer(cbuf, '/');
		    addUTF8Buffer(cbuf, '*');
		  } else
		  { cbuf = NULL;
		  }

		  if ((last = getchr()) == EOF)
		  { if ( cbuf )
		      discardBuffer(cbuf);
		    setErrorLocation(pos, _PL_rd);
		    rawSyntaxError("end_of_file_in_block_comment");
		  }
		  if ( cbuf )
		    addUTF8Buffer(cbuf, last);

		  if ( something_read )
		  { addToBuffer(' ', _PL_rd);	/* positions */
		    addToBuffer(' ', _PL_rd);
		    addToBuffer(last == '\n' ? last : ' ', _PL_rd);
		  }

		  for(;;)
		  { c = getchr();

		    if ( cbuf )
		      addUTF8Buffer(cbuf, c);

		    switch( c )
		    { case EOF:
			if ( cbuf )
			  discardBuffer(cbuf);
		        setErrorLocation(pos, _PL_rd);
			rawSyntaxError("end_of_file_in_block_comment");
#ifndef __YAP_PROLOG__
			/* YAP does not support comment levels in original scanner */
		      case '*':
			if ( last == '/' )
			  level++;
			break;
#endif
		      case '/':
			if ( last == '*' &&
			     (--level == 0 || _PL_rd->strictness) )
			{ if ( cbuf )
			  { addUTF8Buffer(cbuf, EOS);
			    if ( !add_comment(cbuf, pos, _PL_rd PASS_LD) )
			    { discardBuffer(cbuf);
			      return FALSE;
			    }
			    discardBuffer(cbuf);
			  }
			  c = ' ';
			  goto handle_c;
			}
			break;
		    }
		    if ( something_read )
		      addToBuffer(c == '\n' ? c : ' ', _PL_rd);
		    last = c;
		  }
		} else
		{ set_start_line;
		  addToBuffer('/', _PL_rd);
		  if ( isSymbolW(c) )
		  { while( c != EOF && isSymbolW(c) &&
			   !(c == '`' && _PL_rd->backquoted_string) )
		    { addToBuffer(c, _PL_rd);
		      c = getchr();
		    }
		  }
		  dotseen = FALSE;
		  goto handle_c;
		}
      case '%': if ( something_read )
		  addToBuffer(' ', _PL_rd);
      		if ( _PL_rd->comments )
		  { union {
		    tmp_buffer ctmpbuf;
		    buffer uctmpbuf;
		  } u;
		  Buffer cbuf;

		  if ( rb.stream->position )
		  { pbuf = *rb.stream->position;
		    pbuf.charno--;
		    pbuf.linepos--;
		    pos = &pbuf;
		  } else
		    pos = NULL;

		  initBuffer(&u.ctmpbuf);
		  cbuf = (Buffer)&u.uctmpbuf;
		  addUTF8Buffer(cbuf, '%');

		  for(;;)
		  { while((c=getchr()) != EOF && c != '\n')
		    { addUTF8Buffer(cbuf, c);
		      if ( something_read )		/* record positions */
			addToBuffer(' ', _PL_rd);
		    }
		    if ( c == '\n' )
		    { int c2 = Speekcode(rb.stream);

		      if ( c2 == '%' )
		      { if ( something_read )
			{ addToBuffer(c, _PL_rd);
			  addToBuffer(' ', _PL_rd);
			}
			addUTF8Buffer(cbuf, c);
			c = Sgetcode(rb.stream);
			assert(c==c2);
			addUTF8Buffer(cbuf, c);
			continue;
		      }
		    }
		    break;
		  }
		  addUTF8Buffer(cbuf, EOS);
		  if ( !add_comment(cbuf, pos, _PL_rd PASS_LD) )
		  { discardBuffer(cbuf);
		    return FALSE;
		  }
		  discardBuffer(cbuf);
		} else
		{ while((c=getchr()) != EOF && c != '\n')
		  { if ( something_read )		/* record positions */
		      addToBuffer(' ', _PL_rd);
		  }
		}
		goto handle_c;		/* is the newline */
     case '\'': if ( rb.here > rb.base && isDigit(rb.here[-1]) )
		{ cucharp bs = &rb.here[-1];

		  if ( bs > rb.base && isDigit(bs[-1]) )
		    bs--;
		  if ( bs > rb.base && isSign(bs[-1]) )
		    bs--;

		  if ( bs == rb.base || !PlIdContW(bs[-1]) )
		  { int base;

		    if ( isSign(bs[0]) )
		      bs++;
		    base = atoi((char*)bs);

		    if ( base <= 36 )
		    { if ( base == 0 )			/* 0'<c> */
		      { addToBuffer(c, _PL_rd);
			{ if ( (c=getchr()) != EOF )
			  { addToBuffer(c, _PL_rd);
			    if ( c == '\\' ) 		/* 0'\<c> */
			    { if ( (c=getchr()) != EOF )
				addToBuffer(c, _PL_rd);
			    } else if ( c == '\'' ) 	/* 0'' */
			    { if ( (c=getchr()) != EOF )
			      { if ( c == '\'' )
				  addToBuffer(c, _PL_rd);
				else
				  goto handle_c;
			      }
			    }
			    break;
			  }
			  rawSyntaxError("end_of_file");
			}
		      } else
		      { int c2 = Speekcode(rb.stream);

			if ( c2 != EOF )
			{ if ( digitValue(base, c2) >= 0 )
			  { addToBuffer(c, _PL_rd);
			    c = Sgetcode(rb.stream);
			    addToBuffer(c, _PL_rd);
			    dotseen = FALSE;
			    break;
			  }
			  goto sqatom;
			}
			rawSyntaxError("end_of_file");
		      }
		    }
		  }
		}

	      sqatom:
     		set_start_line;
     		if ( !raw_read_quoted(c, _PL_rd) )
		  fail;
		dotseen = FALSE;
		break;
      case '"':	set_start_line;
                if ( !raw_read_quoted(c, _PL_rd) )
		  fail;
		dotseen = FALSE;
		break;
      case '.': addToBuffer(c, _PL_rd);
		set_start_line;
		dotseen++;
		c = getchr();
		if ( isSymbolW(c) )
		{ while( c != EOF && isSymbolW(c) &&
			 !(c == '`' && _PL_rd->backquoted_string) )
		  { addToBuffer(c, _PL_rd);
		    c = getchr();
		  }
		  dotseen = FALSE;
		}
		goto handle_c;
      case '`': if ( _PL_rd->backquoted_string )
		{ set_start_line;
		  if ( !raw_read_quoted(c, _PL_rd) )
		    fail;
		  dotseen = FALSE;
		  break;
		}
      	        /*FALLTHROUGH*/
      default:	if ( c < 0xff )
		{ switch(_PL_char_types[c])
		  { case SP:
		    case CT:
		    blank:
		      if ( dotseen )
		      { if ( rb.here - rb.base == 1 )
			  rawSyntaxError("end_of_clause");
			ensure_space(c);
			addToBuffer(EOS, _PL_rd);
			return rb.base;
		      }
		      do
		      { if ( something_read ) /* positions, \0 --> ' ' */
			  addToBuffer(c ? c : ' ', _PL_rd);
			else
			  ensure_space(c);
			c = getchr();
		      } while( c != EOF && PlBlankW(c) );
		      goto handle_c;
		    case SY:
		      set_start_line;
		      do
		      { addToBuffer(c, _PL_rd);
			c = getchr();
			if ( c == '`' && _PL_rd->backquoted_string )
			  break;
		      } while( c != EOF && c <= 0xff && isSymbol(c) );
					/* TBD: wide symbols? */
		      dotseen = FALSE;
		      goto handle_c;
		    case LC:
		    case UC:
		      set_start_line;
		      do
		      { addToBuffer(c, _PL_rd);
			c = getchr();
		      } while( c != EOF && PlIdContW(c) );
		      dotseen = FALSE;
		      goto handle_c;
		    default:
		      addToBuffer(c, _PL_rd);
		      dotseen = FALSE;
		      set_start_line;
		  }
		} else			/* > 255 */
		{ if ( PlIdStartW(c) )
		  { set_start_line;
		    do
		    { addToBuffer(c, _PL_rd);
		      c = getchr();
		    } while( c != EOF && PlIdContW(c) );
		    dotseen = FALSE;
		    goto handle_c;
		  } else if ( PlBlankW(c) )
		  { goto blank;
		  } else
		  { addToBuffer(c, _PL_rd);
		    dotseen = FALSE;
		    set_start_line;
		  }
		}
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Raw reading returns a string in  UTF-8   notation  of the a Prolog term.
Comment inside the term is  replaced  by   spaces  or  newline to ensure
proper reconstruction of source locations. Comment   before  the term is
skipped.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static unsigned char *
raw_read(ReadData _PL_rd, unsigned char **endp ARG_LD)
{ unsigned char *s;

  if ( (rb.stream->flags & SIO_ISATTY) && Sfileno(rb.stream) >= 0 )
  { ttybuf tab;

    PushTty(rb.stream, &tab, TTY_SAVE);		/* make sure tty is sane */
    PopTty(rb.stream, &ttytab, FALSE);
    s = raw_read2(_PL_rd PASS_LD);
    PopTty(rb.stream, &tab, TRUE);
  } else
  { s = raw_read2(_PL_rd PASS_LD);
  }

  if ( endp )
    *endp = _PL_rd->_rb.here;

  return s;
}


		/********************************
		*       PROLOG CONNECTION       *
		*********************************/

static unsigned char *
backSkipUTF8(unsigned const char *start, unsigned const char *end, int *chr)
{ const unsigned char *s;

  for(s=end-1 ; s>start && *s&0x80; s--)
    ;
  utf8_get_char((char*)s, chr);

  return (unsigned char *)s;
}


static unsigned char *
backSkipBlanks(const unsigned char *start, const unsigned char *end)
{ const unsigned char *s;

  for( ; end > start; end = s)
  { unsigned char *e;
    int chr;

    for(s=end-1 ; s>start && ISUTF8_CB(*s); s--)
      ;
    e = (unsigned char*)utf8_get_char((char*)s, &chr);
    assert(e == end);
    if ( !PlBlankW(chr) )
      return (unsigned char*)end;
  }

  return (unsigned char *)start;
}

static inline ucharp
skipSpaces(cucharp in)
{ int chr;
  ucharp s;

  for( ; *in; in=s)
  { s = utf8_get_uchar(in, &chr);

    if ( !PlBlankW(chr) )
      return (ucharp)in;
  }

  return (ucharp)in;
}



word
pl_raw_read2(term_t from, term_t term)
{ GET_LD
  unsigned char *s, *e, *t2, *top;
  read_data rd;
  word rval;
  IOSTREAM *in;
  int chr;
  PL_chars_t txt;

  if ( !getTextInputStream(from, &in) )
    fail;

  init_read_data(&rd, in PASS_LD);
  if ( !(s = raw_read(&rd, &e PASS_LD)) )
  { rval = PL_raise_exception(rd.exception);
    goto out;
  }

					/* strip the input from blanks */
  top = backSkipBlanks(s, e-1);
  t2 = backSkipUTF8(s, top, &chr);
  if ( chr == '.' )
    top = backSkipBlanks(s, t2);
					/* watch for "0' ." */
  if ( top < e && top-2 >= s && top[-1] == '\'' && top[-2] == '0' )
    top++;
  *top = EOS;
  s = skipSpaces(s);

  txt.text.t    = (char*)s;
  txt.length    = top-s;
  txt.storage   = PL_CHARS_HEAP;
  txt.encoding  = ENC_UTF8;
  txt.canonical = FALSE;

  rval = PL_unify_text(term, 0, &txt, PL_ATOM);

out:
  free_read_data(&rd);
  if ( Sferror(in) )
    return streamStatus(in);
  else
    PL_release_stream(in);

  return rval;
}


static int
unify_read_term_position(term_t tpos ARG_LD)
{ if ( tpos && source_line_no > 0 )
  { return PL_unify_term(tpos,
			 PL_FUNCTOR, FUNCTOR_stream_position4,
			   PL_INT64, source_char_no,
			   PL_INT, source_line_no,
			   PL_INT, source_line_pos,
			   PL_INT64, source_byte_no);
  } else
  { return TRUE;
  }
}


word
pl_raw_read(term_t term)
{ return pl_raw_read2(0, term);
}


static const opt_spec read_term_options[] =
{ { ATOM_variable_names,    OPT_TERM },
  { ATOM_variables,         OPT_TERM },
  { ATOM_singletons,        OPT_TERM },
  { ATOM_term_position,     OPT_TERM },
  { ATOM_subterm_positions, OPT_TERM },
  { ATOM_character_escapes, OPT_BOOL },
  { ATOM_double_quotes,	    OPT_ATOM },
  { ATOM_module,	    OPT_ATOM },
  { ATOM_syntax_errors,     OPT_ATOM },
  { ATOM_backquoted_string, OPT_BOOL },
  { ATOM_comments,	    OPT_TERM },
#ifdef O_QUASIQUOTATIONS
  { ATOM_quasi_quotations,  OPT_TERM },
#endif
  { ATOM_cycles,	    OPT_BOOL },
  { NULL_ATOM,		    0 }
};


static foreign_t
read_term_from_stream(IOSTREAM *s, term_t term, term_t options ARG_LD)
{ term_t tpos = 0;
  term_t tcomments = 0;
  int rval;
  atom_t w;
  read_data rd;
  bool charescapes = -1;
  atom_t dq = NULL_ATOM;
  atom_t mname = NULL_ATOM;
  fid_t fid = PL_open_foreign_frame();

retry:
  init_read_data(&rd, s PASS_LD);

  if ( !scan_options(options, 0, ATOM_read_option, read_term_options,
		     &rd.varnames,
		     &rd.variables,
		     &rd.singles,
		     &tpos,
		     &rd.subtpos,
		     &charescapes,
		     &dq,
		     &mname,
		     &rd.on_error,
		     &rd.backquoted_string,
		     &tcomments,
#ifdef O_QUASIQUOTATIONS
		     &rd.quasi_quotations,
#endif
		     &rd.cycles) ) {
    return FALSE;
  }

  if ( mname )
  { rd.module = lookupModule(mname);
    rd.flags  = rd.module->flags;
  }

  if ( charescapes != -1 )
  { if ( charescapes )
      set(&rd, M_CHARESCAPE);
    else
      clear(&rd, M_CHARESCAPE);
  }
  if ( dq )
  { if ( !setDoubleQuotes(dq, &rd.flags) )
      return FALSE;
  }
  if ( rd.singles && PL_get_atom(rd.singles, &w) && w == ATOM_warning )
    rd.singles = TRUE;
  if ( tcomments )
    rd.comments = PL_copy_term_ref(tcomments);

  rval = read_term(term, &rd PASS_LD);
  if ( Sferror(s) )
    return FALSE;

  if ( rval )
  { if ( tpos )
      rval = unify_read_term_position(tpos PASS_LD);
    if ( rval && tcomments )
    { if ( !PL_unify_nil(rd.comments) )
	rval = FALSE;
    }
  } else
  { if ( rd.has_exception && reportReadError(&rd) )
    { PL_rewind_foreign_frame(fid);
      free_read_data(&rd);
      goto retry;
    }
  }

  free_read_data(&rd);

  return rval;
}


/** read_term(-Term, +Options) is det.
*/

static
PRED_IMPL("read_term", 2, read_term, PL_FA_ISO)
{ PRED_LD
  IOSTREAM *s;

  if ( getTextInputStream(0, &s) )
  { if ( read_term_from_stream(s, A1, A2 PASS_LD) )
      return PL_release_stream(s);
    if ( Sferror(s) )
      return streamStatus(s);
    PL_release_stream(s);
    return FALSE;
  }

  return FALSE;
}


		 /*******************************
		 *	   TERM <->ATOM		*
		 *******************************/

static int
atom_to_term(term_t atom, term_t term, term_t bindings)
{ GET_LD
  PL_chars_t txt;

  if ( !bindings && PL_is_variable(atom) ) /* term_to_atom(+, -) */
  { char buf[1024];
    size_t bufsize = sizeof(buf);
    int rval;
    char *s = buf;
    IOSTREAM *stream;
    PL_chars_t txt;

    stream = Sopenmem(&s, &bufsize, "w");
    stream->encoding = ENC_UTF8;
    PL_write_term(stream, term, 1200, PL_WRT_QUOTED);
    Sflush(stream);

    txt.text.t = s;
    txt.length = bufsize;
    txt.storage = PL_CHARS_HEAP;
    txt.encoding = ENC_UTF8;
    txt.canonical = FALSE;
    rval = PL_unify_text(atom, 0, &txt, PL_ATOM);

    Sclose(stream);
    if ( s != buf )
      Sfree(s);

    return rval;
  }

  if ( PL_get_text(atom, &txt, CVT_ALL|CVT_EXCEPTION) )
  { GET_LD
    read_data rd;
    int rval;
    IOSTREAM *stream;
    source_location oldsrc = LD->read_source;

    stream = Sopen_text(&txt, "r");

    init_read_data(&rd, stream PASS_LD);
    if ( bindings && (PL_is_variable(bindings) || PL_is_list(bindings)) )
      rd.varnames = bindings;
    else if ( bindings )
      return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, bindings);

    if ( !(rval = read_term(term, &rd PASS_LD)) && rd.has_exception )
      rval = PL_raise_exception(rd.exception);
    free_read_data(&rd);
    Sclose(stream);
    LD->read_source = oldsrc;

    return rval;
  }

  fail;
}


static
PRED_IMPL("atom_to_term", 3, atom_to_term, 0)
{ return atom_to_term(A1, A2, A3);
}


static
PRED_IMPL("term_to_atom", 2, term_to_atom, 0)
{ return atom_to_term(A2, A1, 0);
}


int
PL_chars_to_term(const char *s, term_t t)
{ GET_LD
  read_data rd;
  int rval;
  IOSTREAM *stream = Sopen_string(NULL, (char *)s, -1, "r");
  source_location oldsrc = LD->read_source;

  init_read_data(&rd, stream PASS_LD);
  PL_put_variable(t);
  if ( !(rval = read_term(t, &rd PASS_LD)) && rd.has_exception )
    PL_put_term(t, rd.exception);
  free_read_data(&rd);
  Sclose(stream);
  LD->read_source = oldsrc;

  return rval;
}

		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(read)
  PRED_DEF("swi_read_term",		  3, read_term,		  PL_FA_ISO)
  PRED_DEF("swi_read_term",		  2, read_term,		  PL_FA_ISO)
  PRED_DEF("atom_to_term", 3, atom_to_term, 0)
  PRED_DEF("term_to_atom", 2, term_to_atom, 0)
#ifdef O_QUASIQUOTATIONS
  PRED_DEF("$qq_open",            2, qq_open,             0)
#endif
EndPredDefs
