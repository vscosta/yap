
#ifndef THREADS_H

#define THREADS_H 1

#ifdef THREADS

Int Yap_thread_self(void);
int Yap_get_thread_ref_count(int);
void Yap_set_thread_ref_count(int,int);
CELL Yap_thread_create_engine(YAP_thread_attr *);
Int Yap_thread_attach_engine(int);
Int Yap_thread_detach_engine(int);
Int Yap_thread_destroy_engine(int);

#endif

#endif
