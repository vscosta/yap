

/** clause lists are dynamically constructed sets of pointers to clauses. They are grouped as a FIFO */
typedef struct ClauseList
{
 int n; /*counter*/
 void *start;
 void *end;
}  *clause_list_t;

 clause_list_t Yap_ClauseListInit(clause_list_t in);

 int           Yap_ClauseListExtend(clause_list_t cl, void * clause, void *pred);
 void	    Yap_ClauseListClose(clause_list_t cl);
 int	    Yap_ClauseListDestroy(clause_list_t cl);
 void         *Yap_ClauseListToClause(clause_list_t cl);
 void         *Yap_ClauseListCode(clause_list_t cl);
 void         *Yap_FAILCODE(void);

#define Yap_ClauseListCount(cl) ((cl)->n)


