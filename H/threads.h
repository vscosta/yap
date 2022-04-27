
#ifndef THREADS_H

#define THREADS_H 1

#ifdef THREADS

extern YAP_Int Yap_thread_self(void);
extern int Yap_get_thread_ref_count(int);
extern void Yap_set_thread_ref_count(int,int);
extern YAP_CELL Yap_thread_create_engine(YAP_thread_attr *);
extern YAP_Int Yap_thread_attach_engine(int);
extern YAP_Int Yap_thread_detach_engine(int);
extern YAP_Int Yap_thread_destroy_engine(int);

#endif

#endif
