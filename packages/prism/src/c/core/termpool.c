#include <assert.h>
#include "core/termpool.h"
#include "core/xmalloc.h"
#include "core/vector.h"
#include "core/stuff.h"

/* FIXME */
#define prism_quit(msg) quit("***  {PRISM FATAL ERROR: " msg "}\n")
NORET quit(const char *);

/*--------------------------------------------------------------------*/

/* [04 Apr 2009, by yuizumi]
 * This value should be sufficiently large enough to have malloc(3)
 * return an address with its top bit set on 32-bit Linux systems.
 */
#define BLOCK_SIZE 1048576

/*--------------------------------------------------------------------*/

/* [05 Apr 2009, by yuizumi]
 * The area referred by this variable is shared by prism_hash_value()
 * and term_pool_store(), under the assumption that BPLONG values and
 * BPLONG_PTR values (i.e. pointers) are aligned in the same way even
 * without cast operations.
 */
static BPLONG_PTR work;

/*--------------------------------------------------------------------*/

struct term_pool {
    BPLONG_PTR head;
    BPLONG_PTR curr;
    BPLONG_PTR tail;
    struct hash_entry **bucks;
    size_t nbucks;
    size_t count;
};

struct hash_entry {
    TERM term;
    BPULONG hash;
    struct hash_entry *next;
};

/*--------------------------------------------------------------------*/
/*                      Functions from B-Prolog                       */

/* mic.c */
void    c_STATISTICS(void);

/* table.c */
void    numberVarTermOpt(TERM);
TERM    unnumberVarTerm(TERM, BPLONG_PTR, BPLONG_PTR);

/* unify.c */
int     unifyNumberedTerms(TERM, TERM);

/*--------------------------------------------------------------------*/

static ptrdiff_t trail_pos0 = 0;

static void number_vars(TERM term)
{
  CACHE_REGS
    assert(trail_pos0 == 0);

    trail_pos0 = trail_up_addr - trail_top;
    PRE_NUMBER_VAR(0);
    numberVarTermOpt(term);

    if (number_var_exception != 0) {
        prism_quit("suspension variables not supported in Prism");
    }
}

static void revert_vars(void)
{
  CACHE_REGS
    BPLONG_PTR trail_top0;

    assert(trail_pos0 != 0);

    trail_top0 = (BPLONG_PTR)(trail_up_addr - trail_pos0);
    UNDO_TRAILING;
    trail_pos0 = 0;
}

/* [29 Mar 2009, by yuizumi]
 * See Also: "Algorithms in C, Third Edition," by Robert Sedgewick,
 * Addison-Wesley, 1998.
 */
static BPULONG prism_hash_value(TERM term)
{
    TERM    t, *rest;
    BPLONG  i, n;
    SYM_REC_PTR sym;

    BPULONG a = 2130563839ul;
    BPULONG b = 1561772629ul;
    BPULONG h = 0;
    BPULONG u;

    rest = (TERM *)work;

    VECTOR_PUSH(rest, term);

    while (! VECTOR_EMPTY(rest)) {
        t = VECTOR_POP(rest);

//nderef_loop:
        switch (XTAG(t)) {
        case REF0:
        case REF1:
            XNDEREF(t, nderef_loop);
            assert(false); /* numbered by number_vars() */

        case ATM:
        case INT:
        case NVAR:
	    u = (BPULONG)t;
            break;

        case LST:
            VECTOR_PUSH(rest, GET_CDR(t));
            VECTOR_PUSH(rest, GET_CAR(t));
            u = (BPULONG)LST;
            break;

        case STR:
            sym = GET_STR_SYM_REC(t);
            n = GET_ARITY_STR(sym);
            for (i = n; i >= 1; i--) {
                VECTOR_PUSH(rest, GET_ARG(t, i));
            }
            u = (BPULONG)ADDTAG(sym, STR);
            break;

        case SUSP:
            assert(false); /* rejected by number_vars() */

        default:
            assert(false);
        }
        h = (a * h) + (BPULONG)(u);
        a *= b;
    }

    work = (BPLONG *)rest;
    return h;
}

/*--------------------------------------------------------------------*/

static BPLONG_PTR term_pool_allocate(TERM_POOL *this, size_t size)
{
    BPLONG_PTR p_tmp;

    assert(size <= MAX_ARITY + 1);

    if (this->head == NULL || this->curr + size > this->tail) {
	    BP_MALLOC(p_tmp, BLOCK_SIZE, "(prism part)");
        *p_tmp = (BPLONG)(this->head);
        this->head = p_tmp + 0;
        this->curr = p_tmp + 1;
        this->tail = p_tmp + BLOCK_SIZE;
    }

    p_tmp = this->curr;
    this->curr += size;
    return p_tmp;
}

/*--------------------------------------------------------------------*/

static TERM term_pool_store(TERM_POOL *this, TERM term)
{
    TERM    *p, *q, **rest;
    BPLONG  i, n;

    SYM_REC_PTR sym;

    rest = (void *)(work);

    VECTOR_PUSH(rest, &term);

    while (! VECTOR_EMPTY(rest)) {
        p = VECTOR_POP(rest);

//nderef_loop:
        switch (XTAG(*p)) {
        case REF0:
        case REF1:
            XNDEREF(*p, nderef_loop);
            assert(false); /* numbered by number_vars() */

        case ATM:
        case INT:
        case NVAR:
            break;

        case LST:
            q = (TERM *)term_pool_allocate(this, 2);
            *(q + 1) = GET_CDR(*p);
            VECTOR_PUSH(rest, q + 1);
            *(q + 0) = GET_CAR(*p);
            VECTOR_PUSH(rest, q + 0);
            *p = ADDTAG(q, LST);
            break;

        case STR:
            sym = GET_STR_SYM_REC(*p);
            n = GET_ARITY_STR(sym);
            q = (TERM *)term_pool_allocate(this, n + 1);
            *q = (TERM)(sym);
            for (i = n; i >= 1; i--) {
                *(q + i) = GET_ARG(*p, i);
                VECTOR_PUSH(rest, q + i);
            }
            *p = ADDTAG(q, STR);
            break;

        case SUSP:
            assert(false); /* rejected by number_vars() */

        default:
            assert(false);
        }
    }

    work = (void *)(rest);
    return term;
}

/*--------------------------------------------------------------------*/

static void term_pool_rehash(TERM_POOL *this)
{
    struct hash_entry **bucks, *p, *q;
    size_t nbucks, i;

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
        bucks[i] = NULL;

    for (i = 0; i < this->nbucks; i++) {
        p = this->bucks[i];

        while (p != NULL) {
            q = p;
            p = p->next;
            q->next = bucks[q->hash % nbucks];
            bucks[q->hash % nbucks] = q;
        }
    }

	FREE(this->bucks);

    this->nbucks = nbucks;
    this->bucks = bucks;
}

/*--------------------------------------------------------------------*/

static TERM term_pool_search(const TERM_POOL *this, TERM term, BPULONG hash)
{
    struct hash_entry *p;

    p = this->bucks[hash % this->nbucks];

    while (p != NULL) {
        if (hash == p->hash) {
            if (unifyNumberedTerms(term, p->term)) {
                return p->term;
            }
        }
        p = p->next;
    }

    return NULL_TERM;
}

static TERM term_pool_insert(TERM_POOL *this, TERM term, BPULONG hash)
{
    struct hash_entry *entry;

    if (++(this->count) >= this->nbucks)
        term_pool_rehash(this);

    entry = MALLOC(sizeof(struct hash_entry));
    entry->term = term_pool_store(this, term);
    entry->hash = hash;
    entry->next = this->bucks[hash % this->nbucks];
    this->bucks[hash % this->nbucks] = entry;

    return entry->term;
}

/*--------------------------------------------------------------------*/

static TERM term_pool_intern(const TERM_POOL *this1, TERM_POOL *this2, TERM term)
{
    BPULONG hash;
    TERM    rval;

    assert(this2 == NULL || this2 == this1);

//nderef_loop:
    switch (XTAG(term)) {
    case REF0:
    case REF1:
        XNDEREF(term, nderef_loop);
        return MAKE_NVAR(0);

    case ATM:
    case INT:
    case NVAR:
        return term;

    case LST:
    case STR:
        break;

    case SUSP:
        prism_quit("suspension variables not supported in Prism");

    default:
        assert(false);
    }

    number_vars(term);

    hash = prism_hash_value(term);
    rval = term_pool_search(this1, term, hash);

    if (rval == NULL_TERM && this2 != NULL) {
        rval = term_pool_insert(this2, term, hash);
    }

    revert_vars();

    return rval;
}

/*--------------------------------------------------------------------*/

TERM_POOL * term_pool_create(void)
{
    TERM_POOL *this;
    int i;

    this = MALLOC(sizeof(struct term_pool));

    this->head   = NULL;
    this->curr   = NULL;
    this->tail   = NULL;
    this->nbucks = 17;
    this->count  = 0;
    this->bucks  = MALLOC(sizeof(struct hash_entry *) * this->nbucks);

    for (i = 0; i < this->nbucks; i++)
        this->bucks[i] = NULL;

    if (work == NULL) {
        VECTOR_INIT_CAPA(work, 4096);
    }

    return this;
}

/*--------------------------------------------------------------------*/

void term_pool_delete(TERM_POOL *this)
{
    BPLONG_PTR p1, p2;
    struct hash_entry *q1, *q2;
    int i;

    p1 = this->head;

    while (p1 != NULL) {
        p2 = p1;
        p1 = (BPLONG_PTR)(*p1);
        FREE(p2);
    }

    for (i = 0; i < this->nbucks; i++) {
        q1 = this->bucks[i];
        while (q1 != NULL) {
            q2 = q1;
            q1 = q1->next;
            FREE(q2);
        }
    }

    FREE(this->bucks);
    FREE(this);
}

/*--------------------------------------------------------------------*/

TERM term_pool_retrieve(const TERM_POOL *this, TERM term)
{
    return term_pool_intern(this, NULL, term);
}

TERM term_pool_register(TERM_POOL *this, TERM term)
{
    return term_pool_intern(this, this, term);
}

/*--------------------------------------------------------------------*/
