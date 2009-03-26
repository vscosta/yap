
#ifndef THREADS_H

#define THREADS_H 1

typedef struct thread_attr_struct {
  UInt		    ssize;
  UInt		    tsize;
  UInt		    sysize;
  int		   (*cancel)(int thread);
  Term              egoal;
} thread_attr;

#ifdef THREADS

Int STD_PROTO(Yap_thread_self,(void));
int STD_PROTO(Yap_get_thread_ref_count,(int));
void STD_PROTO(Yap_set_thread_ref_count,(int,int));
CELL STD_PROTO(Yap_thread_create_engine,(thread_attr *));
Int STD_PROTO(Yap_thread_attach_engine,(int));
Int STD_PROTO(Yap_thread_detach_engine,(int));
Int STD_PROTO(Yap_thread_destroy_engine,(int));

#endif

#endif
