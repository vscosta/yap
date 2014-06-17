#include "core/xmalloc.h"
#include "core/vector.h"
#include "core/termpool.h"
#include "core/idtable.h"
#include "core/stuff.h"

/*--------------------------------------------------------------------*/

/* table.c */
TERM    unnumberVarTerm(TERM, BPLONG_PTR, BPLONG_PTR);

/*--------------------------------------------------------------------*/

struct id_table {
    TERM_POOL *store;
    struct id_table_entry *elems;
    IDNUM *bucks;
    IDNUM nbucks;
};

struct id_table_entry {
    TERM  term;
    IDNUM next;
};

/*--------------------------------------------------------------------*/

static void id_table_rehash(ID_TABLE *this)
{
    IDNUM *bucks, nbucks, i, j;

    nbucks = 2 * this->nbucks + 1;

    /* find the next prime number */
    for (i = 3; i * i <= nbucks; ) {
        if (nbucks % i == 0) {
            nbucks += 2;
            i = 3;
        }
        else {
            i += 2;
        }
    }

    bucks = MALLOC(sizeof(struct hash_entry *) * nbucks);

    for (i = 0; i < nbucks; i++)
        bucks[i] = ID_NONE;

    for (i = 0; i < VECTOR_SIZE(this->elems); i++) {
        j = (IDNUM)((BPULONG)(this->elems[i].term) % nbucks);
        this->elems[i].next = bucks[j];
        bucks[j] = i;
    }

	FREE(this->bucks);

    this->nbucks = nbucks;
    this->bucks = bucks;
}

static IDNUM id_table_search(const ID_TABLE *this, TERM term)
{
    BPULONG hash;
    IDNUM i;

    hash = (BPULONG)(term);

    i = this->bucks[hash % this->nbucks];

    while (i != ID_NONE) {
        if (term == this->elems[i].term) {
            return i;
        }
        i = this->elems[i].next;
    }

    return ID_NONE;
}

static IDNUM id_table_insert(ID_TABLE *this, TERM term)
{
    BPULONG hash;
    IDNUM n;
    const char *bpx_term_2_string(TERM);

    hash = (BPULONG)(term);

    n = (IDNUM)(VECTOR_SIZE(this->elems));

    if (n >= this->nbucks) {
        id_table_rehash(this);
    }

    VECTOR_PUSH_NONE(this->elems);
    this->elems[n].term = term;
    this->elems[n].next = this->bucks[hash % this->nbucks];
    this->bucks[hash % this->nbucks] = n;

    /* fprintf(curr_out,">> TERM: %s = %d\n",bpx_term_2_string(term),n); */

    return n;
}

/*--------------------------------------------------------------------*/

ID_TABLE * id_table_create(void)
{
    ID_TABLE *this;
    IDNUM i;

    this = MALLOC(sizeof(struct id_table));
	
    this->elems  = NULL;
    this->nbucks = 17;  /* prime number */
    this->bucks  = MALLOC(sizeof(IDNUM) * this->nbucks);
    this->store  = term_pool_create();

    for (i = 0; i < this->nbucks; i++)
        this->bucks[i] = ID_NONE;

    VECTOR_INIT(this->elems);
    return this;
}

void id_table_delete(ID_TABLE *this)
{
    VECTOR_FREE(this->elems);
    FREE(this->bucks);
    term_pool_delete(this->store);

    FREE(this);
}

/*--------------------------------------------------------------------*/

TERM id_table_id2term(const ID_TABLE *this, IDNUM i)
{
    return this->elems[i].term; /* numbered */
}

IDNUM id_table_retrieve(const ID_TABLE *this, TERM term)
{
    term = term_pool_retrieve(this->store, term);

    return id_table_search(this, term);
}

IDNUM id_table_register(ID_TABLE *this, TERM term)
{
  //    BPULONG hash;
    IDNUM i;

    term = term_pool_register(this->store, term);
    //hash = (BPULONG)(term);

    i = id_table_search(this, term);
    if (i == ID_NONE) {
        i = id_table_insert(this, term);
    }
    return i;
}

int id_table_count(const ID_TABLE *this)
{
    return (int)VECTOR_SIZE(this->elems);
}

/*--------------------------------------------------------------------*/

TERM unnumber_var_term(TERM term)
{
  CACHE_REGS
    BPLONG mvn = -1;
    return unnumberVarTerm(term, (BPLONG_PTR)local_top, &mvn);
}
