
typedef struct vlist_struct_t {
  struct VARSTRUCT *ve;
  struct vlist_struct_t *next;
} vlist_t;

typedef struct qq_struct_t {
  unsigned char *text;
  IOPOS start, mid, end;
  vlist_t *vlist;
  struct qq_struct_t *next;
} qq_t;

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

#define RD_MAGIC 0xefebe128

typedef struct read_data_t
{ unsigned char *here;			/* current character */
  unsigned char *base;			/* base of clause */
  unsigned char *end;			/* end of the clause */
  unsigned char *token_start;		/* start of most recent read token */

  int		magic;			/* RD_MAGIC */
  IOPOS         position;               /* Line, line pos, char and byte */
  unsigned char *posp;			/* position pointer */
  size_t	posi;			/* position number */

  term_t	subtpos;		/* Report Subterm positions */
  bool		cycles;			/* Re-establish cycles */
  source_location start_of_term;        /* Position of start of term */
  module_t	module;			/* Current source module */
  unsigned int	flags;			/* Module syntax flags */
  int		styleCheck;		/* style-checking mask */
  bool		backquoted_string;	/* Read `hello` as string */

  int	       *char_conversion_table;	/* active conversion table */

  atom_t	on_error;		/* Handling of syntax errors */
  int		has_exception;		/* exception is raised */

  term_t	exception;		/* raised exception */
  term_t	variables;		/* report variables */
  term_t	singles;		/* Report singleton variables */
  term_t	varnames;		/* Report variables+names */
  int		strictness;		/* Strictness level */

#ifdef O_QUASIQUOTATIONS
  term_t        quasi_quotations;       /* User option quasi_quotations(QQ) */
  term_t        qq;                     /* Quasi quoted list */
  term_t        qq_tail;                /* Tail of the quoted stuff */
#endif

  term_t	comments;		/* Report comments */

  struct read_buffer _rb;		/* keep read characters here */
} read_data, *ReadData;

#define	rdhere		  (_PL_rd->here)
#define	rdbase		  (_PL_rd->base)
#define	rdend		  (_PL_rd->end)
#define	last_token_start  (_PL_rd->token_start)
#define	rb		  (_PL_rd->_rb)

#define DO_CHARESCAPE True(_PL_rd, M_CHARESCAPE)

extern IOFUNCTIONS Sstringfunctions;

#ifndef NULL_ATOM
#define NULL_ATOM 0
#endif

static inline void
setCurrentSourceLocation(ReadData _PL_rd ARG_LD)
{ atom_t a;
  IOSTREAM *s = rb.stream;

  if ( (a = fileNameStream(s)) )
    _PL_rd->start_of_term.file = a;
  else
    _PL_rd->start_of_term.file = NULL_ATOM;

  if ( s->position )
  { _PL_rd->start_of_term.position.lineno  = s->position->lineno;
    _PL_rd->start_of_term.position.linepos = s->position->linepos - 1;
    _PL_rd->start_of_term.position.charno  = s->position->charno - 1;
    /* byteno maintained get getchr__() */
  } else
  { _PL_rd->start_of_term.position.lineno  = -1;
    _PL_rd->start_of_term.position.linepos = -1;
    _PL_rd->start_of_term.position.charno  = 0;
    _PL_rd->start_of_term.position.byteno  = 0;
  }

  LD->read_source = _PL_rd->start_of_term;
}

extern int Yap_read_term(term_t t, IOSTREAM *st, struct read_data_t *rdt);
/* parser.c */
extern Term	Yap_Parse( struct read_data_t *);
extern void	init_read_data( struct read_data_t *, IOSTREAM *st ARG_LD);
extern void	free_read_data( struct read_data_t *);

