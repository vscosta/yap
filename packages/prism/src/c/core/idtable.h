#ifndef IDTABLE_H
#define IDTABLE_H

#include "bpx.h"

/*--------------------------------------------------------------------*/

#define ID_NONE ((IDNUM)(-1))

/*--------------------------------------------------------------------*/

typedef struct id_table ID_TABLE;
typedef unsigned int IDNUM;

/*--------------------------------------------------------------------*/

ID_TABLE *  id_table_create(void);
void        id_table_delete(ID_TABLE *);
TERM        id_table_id2term(const ID_TABLE *, IDNUM);
IDNUM       id_table_retrieve(const ID_TABLE *, TERM);
IDNUM       id_table_register(ID_TABLE *, TERM);
int         id_table_count(const ID_TABLE *);

TERM unnumber_var_term(TERM);

/*--------------------------------------------------------------------*/

#endif /* IDTABLE_H */

