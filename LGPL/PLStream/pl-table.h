/*****
      Hash Tables pl-table.h
****/

#define LMASK_BITS	7		/* total # mask bits */

#define TABLE_MASK		0xf0000000UL

#define pointerHashValue(p, size) ((((intptr_t)(p) >> LMASK_BITS) ^ \
				    ((intptr_t)(p) >> (LMASK_BITS+5)) ^ \
				    ((intptr_t)(p))) & \
				   ((size)-1))

typedef struct symbol *		Symbol;		/* symbol of hash table */

/* hash Table + lock + scaling + enumerator */
typedef struct table *Table;

typedef struct table_enum* TableEnum;

/* symbol table hash  package */

struct table
{ int		buckets;	/* size of hash table */
  int		size;		/* # symbols in the table */
  TableEnum	enumerators;	/* Handles for enumeration */
#ifdef O_PLMT
  simpleMutex  *mutex;		/* Mutex to guard table */
#endif
  void 		(*copy_symbol)(Symbol s);
  void 		(*free_symbol)(Symbol s);
  Symbol	*entries;	/* array of hash symbols */
};

struct symbol
{ Symbol	next;		/* next in chain */
  void *	name;		/* name entry of symbol */
  void *	value;		/* associated value with name */
};

struct table_enum
{ Table		table;		/* Table we are working on */
  int		key;		/* Index of current symbol-chain */
  Symbol	current;	/* The current symbol */
  TableEnum	next;		/* More choice points */
};

extern void 		initTables(void);
extern Table 		newHTable(int size);
extern void 		destroyHTable(Table ht);
extern Symbol 		lookupHTable(Table ht, void *name);
extern Symbol 		addHTable(Table ht, void *name, void *value);
extern void 		deleteSymbolHTable(Table ht, Symbol s);
extern void 		clearHTable(Table ht);
extern Table 		copyHTable(Table org);
extern TableEnum 	newTableEnum(Table ht);
extern void 		freeTableEnum(TableEnum e);
extern Symbol 		advanceTableEnum(TableEnum e);
