
#if defined(_MSC_VER) && defined(YAP_EXPORTS)
#define X_API __declspec(dllexport)
#else
#define X_API
#endif


struct ClauseList
{
 int n; /*counter*/
 void *start;
 void *end;
};
typedef struct ClauseList *clause_list_t;

X_API clause_list_t Yap_ClauseListInit(clause_list_t in);

X_API int           Yap_ClauseListExtend(clause_list_t cl, void * clause, void *pred);
X_API void	    Yap_ClauseListClose(clause_list_t cl);
X_API int	    Yap_ClauseListDestroy(clause_list_t cl);
X_API void         *Yap_ClauseListToClause(clause_list_t cl);
X_API void         *Yap_ClauseListCode(clause_list_t cl);
X_API void         *Yap_FAILCODE(void);

#define Yap_ClauseListCount(cl) ((cl)->n)


