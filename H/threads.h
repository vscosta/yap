typedef struct{
  UInt		    ssize;
  UInt		    tsize;
  int		   (*cancel)(int thread);
} thread_attr;

Int STD_PROTO(Yap_thread_self,(void));
Int STD_PROTO(Yap_thread_create_engine,(thread_attr *));
Int STD_PROTO(Yap_thread_attach_engine,(int));
Int STD_PROTO(Yap_thread_detach_engine,(int));
Int STD_PROTO(Yap_thread_destroy_engine,(int));

