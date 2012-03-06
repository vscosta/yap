
#include "pl-incl.h"
#include "pl-ctype.h"
#include "pl-utf8.h"
#include "pl-dtoa.h"
#include "pl-umap.c"			/* Unicode map */

typedef       unsigned char * ucharp;
typedef const unsigned char * cucharp;

#define utf8_get_uchar(s, chr) (ucharp)utf8_get_char((char *)(s), chr)

#define FASTBUFFERSIZE	256	/* read quickly upto this size */

struct read_buffer
{ int	size;			/* current size of read buffer */
  unsigned char *base;		/* base of read buffer */
  unsigned char *here;		/* current position in read buffer */
  unsigned char *end;		/* end of the valid buffer */

  IOSTREAM *stream;		/* stream we are reading from */
  unsigned char fast[FASTBUFFERSIZE];	/* Quick internal buffer */
};


typedef struct
{ unsigned char *here;			/* current character */
  unsigned char *base;			/* base of clause */
  unsigned char *end;			/* end of the clause */
  unsigned char *token_start;		/* start of most recent read token */
  int		has_exception;		/* exception is raised */

  unsigned char *posp;			/* position pointer */
  size_t	posi;			/* position number */

  unsigned int	flags;			/* Module syntax flags */
  int		styleCheck;		/* style-checking mask */
  bool		backquoted_string;	/* Read `hello` as string */

  int	       *char_conversion_table;	/* active conversion table */

  term_t	exception;		/* raised exception */
  term_t	varnames;		/* Report variables+names */  
  int		strictness;		/* Strictness level */

  term_t	comments;		/* Report comments */

  struct read_buffer _rb;		/* keep read characters here */
} read_data, *ReadData;

#define	rdhere		  (_PL_rd->here)
#define	rdbase		  (_PL_rd->base)
#define	rdend		  (_PL_rd->end)
#define	last_token_start  (_PL_rd->token_start)
#define	rb		  (_PL_rd->_rb)

#define DO_CHARESCAPE true(_PL_rd, CHARESCAPE)

extern IOFUNCTIONS Sstringfunctions;

static bool
isStringStream(IOSTREAM *s)
{ return s->functions == &Sstringfunctions;
}



static void
init_read_data(ReadData _PL_rd, IOSTREAM *in ARG_LD)
{  memset(_PL_rd, 0, sizeof(*_PL_rd));	/* optimise! */

  _PL_rd->varnames = 0;
  rb.stream = in;
  _PL_rd->has_exception = 0;
  _PL_rd->exception = 0;
}

static void
free_read_data(ReadData _PL_rd)
{ 
}

static int 
read_term(term_t t, ReadData _PL_rd ARG_LD)
{
  int rval;
  term_t except;

  if (!(rval = Yap_read_term(t, rb.stream, &except, _PL_rd->varnames))) {
    if (except) {
      _PL_rd->has_exception = TRUE;
      _PL_rd->exception = except;
    }
  }
  return rval;
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
		 *     UNICODE CLASSIFIERS	*
		 *******************************/

#define CharTypeW(c, t, w) \
	((unsigned)(c) <= 0xff ? (_PL_char_types[(unsigned)(c)] t) \
			       : (uflagsW(c) & w))

#define PlBlankW(c)	CharTypeW(c, <= SP, U_SEPARATOR)
#define PlUpperW(c)	CharTypeW(c, == UC, U_UPPERCASE)
#define PlIdStartW(c)	(c <= 0xff ? (isLower(c)||isUpper(c)||c=='_') \
				   : uflagsW(c) & U_ID_START)
#define PlIdContW(c)	CharTypeW(c, >= UC, U_ID_CONTINUE)
#define PlSymbolW(c)	CharTypeW(c, == SY, 0)
#define PlPunctW(c)	CharTypeW(c, == PU, 0)
#define PlSoloW(c)	CharTypeW(c, == SO, 0)

int
unicode_separator(pl_wchar_t c)
{ return PlBlankW(c);
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


static void
setCurrentSourceLocation(IOSTREAM *s ARG_LD)
{ atom_t a;

  if ( s->position )
  { source_line_no  = s->position->lineno;
    source_line_pos = s->position->linepos - 1;	/* char just read! */
    source_char_no  = s->position->charno - 1;	/* char just read! */
  } else
  { source_line_no  = -1;
    source_line_pos = -1;
    source_char_no  = 0;
  }

  if ( (a = fileNameStream(s)) )
    source_file_name = a;
  else
    source_file_name = NULL_ATOM;
}

#if __YAP_PROLOG__
void
Yap_setCurrentSourceLocation(IOSTREAM **s)
{
  GET_LD
  if (!*s)
    *s = Suser_input;
  setCurrentSourceLocation(*s PASS_LD);
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
			 { setCurrentSourceLocation(rb.stream PASS_LD); \
			   something_read++; \
			 } \
		       }

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

  if ( !getInputStream(from, &in) )
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


word
pl_raw_read(term_t term)
{ return pl_raw_read2(0, term);
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
  PRED_DEF("atom_to_term", 3, atom_to_term, 0)
  PRED_DEF("term_to_atom", 2, term_to_atom, 0)
EndPredDefs
