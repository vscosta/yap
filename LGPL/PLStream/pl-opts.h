		 /*******************************
		 *	   OPTION LISTS		*
		 *******************************/

#define OPT_BOOL	(0)		/* types */
#define OPT_INT		(1)
#define OPT_STRING	(2)
#define OPT_ATOM	(3)
#define OPT_TERM	(4)		/* arbitrary term */
#define OPT_LONG	(5)
#define OPT_NATLONG	(6)		/* > 0 */
#define OPT_TYPE_MASK	0xff
#define OPT_INF		0x100		/* allow 'inf' */

#define OPT_ALL		0x1		/* flags */

typedef struct
{ atom_t	name;			/* Name of option */
  int		type;			/* Type of option */
} opt_spec, *OptSpec;

extern bool scan_options(term_t options, int flags, atom_t optype,
			 const opt_spec *specs, ...);

