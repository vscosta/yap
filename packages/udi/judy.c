// needs to e cleaned up

#if JUDY_FOUND
#include <Judy.h>
/* Judy1 integer sparse set intersection */
static inline int j1_callback(void *key, void *data, void *arg)
{
  int r;
  Pvoid_t *arrayP = (Pvoid_t *) arg;
  J1S(r, *arrayP, (Word_t) data);
  if (r == JERR)
    return FALSE;
  return TRUE;
}
#endif

yamop *
Yap_udi_join(struct ClauseList *clauselist. 	UdiPArg parg. 	UdiInfo info)
{
#if JUDY_FOUND
		/*TODO: do more tests to this algorithm*/
		int i;
		Pvoid_t tmp = (Pvoid_t) NULL;
		Pvoid_t result = (Pvoid_t) NULL;
		Word_t count = 0L;
		Word_t idx_r = 0L;
		Word_t idx_tmp = 0L;
		int rc = 0;
		yamop **x;

		/*
		 * I will start with the simplest approach
		 * for each index create a set and intersect it with the
		 * next
		 *
		 * In the future it could pay to sort according to index type
		 * to improve intersection part
		 */
		for (i = 0; i < utarray_len(info->args) ; i++) {
			parg = (UdiPArg) utarray_eltptr(info->args,i);
			r = parg->control->search(parg->idxstr, parg->arg, j1_callback, &tmp);
			if (r == -1) /*this arg does not prune search*/
				continue;
			rc ++;
			J1C(count, result, 0, -1);
			if (r == 0) /* this arg gave 0 results -> FAIL*/
			{
				if (count > 0) // clear previous result if they exists
					J1FA(count, result);
				return Yap_FAILCODE();
			}

			if (count == 0) // first result_set
			{
				result = tmp;
				tmp = (Pvoid_t) NULL;
			}
			else /*intersection*/
			{
				idx_tmp = 0L;
				idx_r = 0L;
				J1F(count, result, idx_r); //succeeds one time at least
				assert(count > 0);
				J1F(count, tmp, idx_tmp);  //succeeds one time at least
				assert(count > 0);
				while (count)
				{
					while (idx_r < idx_tmp)
					{
						J1U(count, result, idx_r); //does not belong
						J1N(count, result, idx_r); //next
						if (! count) break;        //end result set
					}
					if(idx_r == idx_tmp)
					{
						J1N(count, result, idx_r); //next
						if (! count) break;        //end result set
						J1N(count, tmp, idx_tmp);  //next tmp
						//if (! count) break;      //end tmp set will break while
					}
					else // (idx_r > idx_tmp)
					{
						idx_tmp = idx_r; // fast forward
						J1F(count, tmp, idx_tmp); // first starting in idx_r
						//if (! count) break; //end tmp set will break while
					}
				}
				J1F(count, result, idx_r); // first starting in idx_r
				//clear up the rest
				while (idx_r > idx_tmp && count) //result has more setted values
				{
					J1U(count, result, idx_r); //does not belong
					J1N(count, result, idx_r); //next
				}
				J1FA(count, tmp); //free tmp
			}
		}
		if (rc == 0) /*no search performed*/
			return NULL;

		J1C(count, result, 0, -1);
		if (count == 0) { /*result set empty -> FAIL */
			J1FA(count, result);
			return Yap_FAILCODE();
		}

		/*convert Juddy1 to clauselist*/
		Yap_ClauseListInit(clauselistp);
		idx_r = 0L;
		J1F(count, result, idx_r);
		while (count)
		{
			x = (yamop **) utarray_eltptr(info->clauselist, idx_r - 1);
			Yap_ClauseListExtend(
					clauselist,
					*x,
					info->p);
			J1N(count, result, idx_r);
		}
		J1FA(count,result);
			fprintf(stderr,"J1 used space %ld bytes for %d clausules\n",
					count, Yap_ClauseListCount(clauselist));
		Yap_ClauseListClose(clauselist);
#else
		fprintf(stderr,"Without libJudy only one argument indexed is allowed."
				"Falling back to Yap Indexing\n");
		return NULL; //NO Judy Available
#endif
}
